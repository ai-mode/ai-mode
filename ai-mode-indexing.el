;;; ai-mode-indexing.el --- Project indexing system for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; This file is part of ai-mode.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module manages the project indexing system for AI, encompassing file
;; summary generation, persistence, and versioning. It provides:
;; - Project file indexing with AI-generated summaries
;; - Multiple indexing strategies (parallel-independent, sequential)
;; - Index persistence and versioning system
;; - Index cleanup and management utilities
;; - Comprehensive telemetry, logging, and request auditing support

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-project)
(require 'ai-command-management)
(require 'ai-prompt-management)
(require 'ai-model-management)
(require 'ai-execution)
(require 'ai-context-management)
(require 'ai-telemetry)
(require 'ai-response-processors)
(require 'ai-progress)
(require 'ai-logging)
(require 'ai-request-audit)

(defcustom ai-mode-indexing--include-existing-context t
  "Include context from already indexed files when indexing new files.
When enabled, the indexing process will include summaries of previously
indexed files to ensure consistent format and improve contextual understanding."
  :type 'boolean
  :group 'ai)

(defcustom ai-mode-indexing--strategy 'parallel-independent
  "Strategy for file indexing process.
Controls how files are processed during project indexing:
- `parallel-independent': Parallel processing without context sharing between files
- `sequential': Sequential processing with accumulating context from current session only"
  :type '(choice (const :tag "Parallel processing without context sharing" parallel-independent)
                 (const :tag "Sequential with session context accumulation" sequential))
  :group 'ai)

(defcustom ai-mode-indexing--index-retention-depth 5
  "Number of index versions to retain before cleanup.
When exceeded, older index versions will be automatically deleted."
  :type 'integer
  :group 'ai)

(defcustom ai-mode-indexing--call-timeout 0.1
  "Timeout in seconds between individual file indexing calls during project summary generation.
Set to 0 for no delay."
  :type 'number
  :group 'ai)

;; Project files summary index
(defvar ai-mode-indexing--project-files-summary-index (make-hash-table :test 'equal)
  "Cached index of project files summary structures.
Maps project root paths to lists of file summary structs.")

;; Index persistence variables
(defvar ai-mode-indexing--persistent-index-metadata (make-hash-table :test 'equal)
  "Metadata for persistent index versions.
Maps project root paths to plists containing version information.")

(defun ai-mode-indexing--get-project-index-directory (project-root)
  "Get the index directory for PROJECT-ROOT."
  (file-name-as-directory (expand-file-name ".ai/index" project-root)))

(defun ai-mode-indexing--generate-index-version-name (start-time end-time)
  "Generate a version name from START-TIME and END-TIME.
Format: start_YYYYMMDD-HHMMSS_end_YYYYMMDD-HHMMSS"
  (let ((start-str (format-time-string "%Y%m%d-%H%M%S" start-time))
        (end-str (format-time-string "%Y%m%d-%H%M%S" end-time)))
    (format "start_%s_end_%s" start-str end-str)))

(defun ai-mode-indexing--parse-index-version-name (version-name)
  "Parse VERSION-NAME and return (start-time . end-time) or nil if invalid."
  (when (string-match "start_\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)_end_\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)" version-name)
    (let ((start-str (match-string 1 version-name))
          (end-str (match-string 2 version-name)))
      (condition-case nil
          (let ((start-iso (concat (substring start-str 0 4) "-"    ; YYYY-
                                  (substring start-str 4 6) "-"     ; MM-
                                  (substring start-str 6 8) "T"     ; DDT
                                  (substring start-str 9 11) ":"    ; HH:
                                  (substring start-str 11 13) ":"   ; MM:
                                  (substring start-str 13 15)))     ; SS
                (end-iso (concat (substring end-str 0 4) "-"        ; YYYY-
                                (substring end-str 4 6) "-"         ; MM-
                                (substring end-str 6 8) "T"         ; DDT
                                (substring end-str 9 11) ":"        ; HH:
                                (substring end-str 11 13) ":"       ; MM:
                                (substring end-str 13 15))))        ; SS
            (cons (date-to-time start-iso)
                  (date-to-time end-iso)))
        (error nil)))))

(defun ai-mode-indexing--get-index-version-directory (project-root version-name)
  "Get the directory path for a specific index VERSION-NAME in PROJECT-ROOT."
  (file-name-as-directory
   (expand-file-name version-name (ai-mode-indexing--get-project-index-directory project-root))))

(defun ai-mode-indexing--get-index-metadata-file (project-root version-name)
  "Get the metadata file path for index VERSION-NAME in PROJECT-ROOT."
  (expand-file-name "metadata.json" (ai-mode-indexing--get-index-version-directory project-root version-name)))

(defun ai-mode-indexing--get-index-mapping-file (project-root version-name)
  "Get the mapping file path for index VERSION-NAME in PROJECT-ROOT."
  (expand-file-name "mapping.json" (ai-mode-indexing--get-index-version-directory project-root version-name)))

(defun ai-mode-indexing--generate-index-file-id (relative-path)
  "Generate a unique file ID for RELATIVE-PATH.
Returns a hash-based identifier."
  (let ((hash (secure-hash 'sha256 relative-path)))
    (substring hash 0 16)))

(defun ai-mode-indexing--get-index-file-path (project-root version-name file-id)
  "Get the path for an index file with FILE-ID in VERSION-NAME for PROJECT-ROOT."
  (expand-file-name (format "%s.json" file-id)
                    (ai-mode-indexing--get-index-version-directory project-root version-name)))

(defun ai-mode-indexing--save-index-metadata (project-root version-name metadata)
  "Save METADATA for index VERSION-NAME in PROJECT-ROOT."
  (let ((metadata-file (ai-mode-indexing--get-index-metadata-file project-root version-name))
        (version-dir (ai-mode-indexing--get-index-version-directory project-root version-name)))
    (ai-logging--verbose-message "Saving index metadata for version %s in %s" version-name project-root)
    (unless (file-directory-p version-dir)
      (ai-logging--verbose-message "Creating index version directory: %s" version-dir)
      (make-directory version-dir t))
    (with-temp-file metadata-file
      (insert (json-encode metadata)))
    (ai-logging--verbose-message "Index metadata saved to %s" metadata-file)))

(defun ai-mode-indexing--load-index-metadata (project-root version-name)
  "Load metadata for index VERSION-NAME in PROJECT-ROOT.
Returns nil if metadata file doesn't exist or is invalid."
  (let ((metadata-file (ai-mode-indexing--get-index-metadata-file project-root version-name)))
    (ai-logging--verbose-message "Loading index metadata from %s" metadata-file)
    (when (file-readable-p metadata-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (json-read))
        (error
         (ai-logging--verbose-message "Error loading index metadata: %s" (error-message-string err))
         nil)))))

(defun ai-mode-indexing--save-index-mapping (project-root version-name mapping)
  "Save file MAPPING for index VERSION-NAME in PROJECT-ROOT.
MAPPING is an alist of (relative-path . file-id) pairs."
  (let ((mapping-file (ai-mode-indexing--get-index-mapping-file project-root version-name))
        (version-dir (ai-mode-indexing--get-index-version-directory project-root version-name)))
    (ai-logging--verbose-message "Saving index mapping for version %s with %d entries" version-name (length mapping))
    (unless (file-directory-p version-dir)
      (make-directory version-dir t))
    (with-temp-file mapping-file
      (insert (json-encode mapping)))
    (ai-logging--verbose-message "Index mapping saved to %s" mapping-file)))

(defun ai-mode-indexing--load-index-mapping (project-root version-name)
  "Load file mapping for index VERSION-NAME in PROJECT-ROOT.
Returns nil if mapping file doesn't exist or is invalid."
  (let ((mapping-file (ai-mode-indexing--get-index-mapping-file project-root version-name)))
    (ai-logging--verbose-message "Loading index mapping from %s" mapping-file)
    (when (file-readable-p mapping-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents mapping-file)
            (json-read))
        (error
         (ai-logging--verbose-message "Error loading index mapping: %s" (error-message-string err))
         nil)))))

(defun ai-mode-indexing--save-index-file (project-root version-name file-id summary-struct)
  "Save SUMMARY-STRUCT as FILE-ID in VERSION-NAME for PROJECT-ROOT."
  (let ((index-file (ai-mode-indexing--get-index-file-path project-root version-name file-id)))
    (ai-logging--verbose-message "Saving index file %s for version %s" file-id version-name)
    (with-temp-file index-file
      (insert (json-encode summary-struct)))
    (ai-logging--verbose-message "Index file saved to %s" index-file)))

(defun ai-mode-indexing--load-index-file (project-root version-name file-id)
  "Load summary struct for FILE-ID in VERSION-NAME for PROJECT-ROOT.
Returns nil if file doesn't exist or is invalid."
  (let ((index-file (ai-mode-indexing--get-index-file-path project-root version-name file-id)))
    (ai-logging--verbose-message "Loading index file %s from version %s" file-id version-name)
    (when (file-readable-p index-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents index-file)
            (json-read))
        (error
         (ai-logging--verbose-message "Error loading index file %s: %s" file-id (error-message-string err))
         nil)))))

(defun ai-mode-indexing--get-available-index-versions (project-root)
  "Get list of available index versions for PROJECT-ROOT.
Returns a list of version names sorted by creation time (newest first)."
  (ai-logging--verbose-message "Getting available index versions for %s" project-root)
  (let ((index-dir (ai-mode-indexing--get-project-index-directory project-root)))
    (when (file-directory-p index-dir)
      (let ((versions nil))
        (dolist (entry (directory-files index-dir nil "^start_.*_end_.*$"))
          (when (file-directory-p (expand-file-name entry index-dir))
            (push entry versions)))
        ;; Sort by creation time (newest first)
        (let ((sorted-versions (sort versions (lambda (a b)
                                                (let ((time-a (ai-mode-indexing--parse-index-version-name a))
                                                      (time-b (ai-mode-indexing--parse-index-version-name b)))
                                                  (when (and time-a time-b)
                                                    (time-less-p (cdr time-b) (cdr time-a))))))))
          (ai-logging--verbose-message "Found %d index versions for %s" (length sorted-versions) project-root)
          sorted-versions)))))

(defun ai-mode-indexing--format-index-version-display (version-name)
  "Format VERSION-NAME for display in completing-read."
  (if-let ((times (ai-mode-indexing--parse-index-version-name version-name)))
      (let ((start-time (car times))
            (end-time (cdr times)))
        (format "%s → %s (%s)"
                (format-time-string "%Y-%m-%d %H:%M:%S" start-time)
                (format-time-string "%Y-%m-%d %H:%M:%S" end-time)
                version-name))
    version-name))

(defun ai-mode-indexing--save-current-index-to-disk (project-root start-time)
  "Save current in-memory index to disk for PROJECT-ROOT.
START-TIME is when the indexing process began."
  (when-let ((summaries (gethash project-root ai-mode-indexing--project-files-summary-index)))
    (let* ((end-time (current-time))
           (version-name (ai-mode-indexing--generate-index-version-name start-time end-time))
           (mapping nil)
           (metadata `((version . ,version-name)
                      (project-root . ,project-root)
                      (start-time . ,(format-time-string "%Y-%m-%dT%H:%M:%S" start-time))
                      (end-time . ,(format-time-string "%Y-%m-%dT%H:%M:%S" end-time))
                      (file-count . ,(length summaries))
                      (strategy . ,(symbol-name ai-mode-indexing--strategy)))))

      (ai-logging--verbose-message "Saving index version %s to disk with %d files" version-name (length summaries))

      ;; Create version directory
      (let ((version-dir (ai-mode-indexing--get-index-version-directory project-root version-name)))
        (make-directory version-dir t))

      ;; Save each summary file and build mapping
      (dolist (summary summaries)
        (let* ((relative-path (plist-get summary :relative-path))
               (file-id (ai-mode-indexing--generate-index-file-id relative-path)))
          (ai-mode-indexing--save-index-file project-root version-name file-id summary)
          (push (cons relative-path file-id) mapping)))

      ;; Save metadata and mapping
      (ai-mode-indexing--save-index-metadata project-root version-name metadata)
      (ai-mode-indexing--save-index-mapping project-root version-name mapping)

      (message "Index version '%s' saved to disk with %d files" version-name (length summaries))
      (ai-logging--verbose-message "Index version %s successfully saved to disk" version-name)
      version-name)))

(defun ai-mode-indexing--load-index-version (project-root version-name)
  "Load index VERSION-NAME from disk for PROJECT-ROOT into memory.
Returns the loaded summaries list or nil if loading fails."
  (ai-logging--verbose-message "Loading index version %s for %s" version-name project-root)
  (when-let* ((mapping (ai-mode-indexing--load-index-mapping project-root version-name))
              (metadata (ai-mode-indexing--load-index-metadata project-root version-name)))
    (let ((summaries nil)
          (loaded-count 0))
      (dolist (mapping-entry mapping)
        (let* ((relative-path (car mapping-entry))
               (file-id (cdr mapping-entry))
               (summary (ai-mode-indexing--load-index-file project-root version-name file-id)))
          (when summary
            (push summary summaries)
            (setq loaded-count (1+ loaded-count)))))

      (when summaries
        ;; Update in-memory index
        (puthash project-root summaries ai-mode-indexing--project-files-summary-index)
        (message "Loaded index version '%s' with %d/%d files"
                 version-name loaded-count (length mapping))
        (ai-logging--verbose-message "Index version %s loaded successfully: %d/%d files"
                                   version-name loaded-count (length mapping))
        summaries))))

(defun ai-mode-indexing--delete-index-version (project-root version-name)
  "Delete index VERSION-NAME for PROJECT-ROOT from disk."
  (let ((version-dir (ai-mode-indexing--get-index-version-directory project-root version-name)))
    (ai-logging--verbose-message "Deleting index version %s at %s" version-name version-dir)
    (when (file-directory-p version-dir)
      (delete-directory version-dir t)
      (message "Deleted index version: %s" version-name)
      (ai-logging--verbose-message "Index version %s successfully deleted" version-name))))

(defun ai-mode-indexing--cleanup-old-index-versions (project-root)
  "Delete old index versions beyond the retention depth for PROJECT-ROOT."
  (let ((versions (ai-mode-indexing--get-available-index-versions project-root)))
    (ai-logging--verbose-message "Checking for old index versions to cleanup: %d versions found, retention depth: %d"
                               (length versions) ai-mode-indexing--index-retention-depth)
    (when (> (length versions) ai-mode-indexing--index-retention-depth)
      (let ((versions-to-delete (nthcdr ai-mode-indexing--index-retention-depth versions)))
        (ai-logging--verbose-message "Deleting %d old index versions" (length versions-to-delete))
        (dolist (version versions-to-delete)
          (ai-mode-indexing--delete-index-version project-root version))
        (message "Cleaned up %d old index versions" (length versions-to-delete))))))

(cl-defun ai-mode-indexing--get-indexing-context (file-struct model)
  "Create execution context for indexing a single FILE-STRUCT with MODEL.
Returns a plist suitable for the backend."
  (let* ((instruction-command "index file")
         (config (ai-command-management--get-command-config-by-type instruction-command))
         (project-root (ai-project--get-project-root))
         (request-id (ai-common--generate-request-id))
         ;; Basic context without existing summaries for parallel-independent strategy
         (basic-full-context `(:model-context ,model
                               :file-path ,(plist-get file-struct :relative-path)
                               :buffer-language ,(plist-get file-struct :buffer-language)
                               :file-size ,(plist-get file-struct :file-size)
                               :project-root ,project-root
                               :strategy ,ai-mode-indexing--strategy
                               :request-id ,request-id))
         (command-instructions (ai-common--make-typed-struct
                                (ai-prompt-management--render-instruction instruction-command basic-full-context)
                                'agent-instructions
                                'command-specific-instructions))

         ;; File to be indexed
         (file-content-as-user-input (ai-common--make-typed-struct
                                      file-struct
                                      'user-input
                                      'file-to-index))

         ;; Simple message list for parallel processing
         (messages (list command-instructions file-content-as-user-input)))

    (ai-logging--verbose-message "Creating indexing context for file %s with request ID %s"
                               (plist-get file-struct :relative-path) request-id)

    ;; Filter out any null messages
    (setq messages (ai-context-management--filter-non-empty-content messages))

    ;; Log to prompt buffer for debugging
    (ai-telemetry-write-context-to-prompt-buffer messages)

    `(:messages ,messages :model-context ,model :request-id ,request-id :command-config ,config)))

(cl-defun ai-mode-indexing--get-indexing-context-with-summaries (file-struct model existing-summaries)
  "Create execution context for indexing a single FILE-STRUCT with MODEL and EXISTING-SUMMARIES.
This version includes existing summaries for sequential processing.
Returns a plist suitable for the backend."
  (let* ((instruction-command "index file")
         (config (ai-command-management--get-command-config-by-type instruction-command))
         (project-root (ai-project--get-project-root))
         (request-id (ai-common--generate-request-id))
         ;; Enhanced context with existing summaries information
         (enhanced-full-context `(:model-context ,model
                                  :file-path ,(plist-get file-struct :relative-path)
                                  :buffer-language ,(plist-get file-struct :buffer-language)
                                  :file-size ,(plist-get file-struct :file-size)
                                  :project-root ,project-root
                                  :has-existing-context ,(and ai-mode-indexing--include-existing-context
                                                             existing-summaries
                                                             (> (length existing-summaries) 0))
                                  :context-source "session-accumulation"
                                  :context-count ,(length existing-summaries)
                                  :strategy ,ai-mode-indexing--strategy
                                  :request-id ,request-id))
         (command-instructions (ai-common--make-typed-struct
                                (ai-prompt-management--render-instruction instruction-command enhanced-full-context)
                                'agent-instructions
                                'command-specific-instructions))

         ;; Include existing context if enabled and available
         (existing-context-struct
          (when (and ai-mode-indexing--include-existing-context
                     existing-summaries
                     (> (length existing-summaries) 0))
            (ai-common--make-typed-struct
             existing-summaries
             'additional-context
             'session-accumulated-files
             :count (length existing-summaries)
             :project-root project-root
             :strategy ai-mode-indexing--strategy)))

         ;; File to be indexed
         (file-content-as-user-input (ai-common--make-typed-struct
                                      file-struct
                                      'user-input
                                      'file-to-index))

         ;; Combine all messages, filtering out nil values
         (messages (cl-remove-if #'null
                                (list command-instructions
                                      existing-context-struct
                                      file-content-as-user-input))))

    (ai-logging--verbose-message "Creating indexing context with summaries for file %s with request ID %s (existing summaries: %d)"
                               (plist-get file-struct :relative-path) request-id (length existing-summaries))

    ;; Filter out any null messages
    (setq messages (ai-context-management--filter-non-empty-content messages))

    ;; Log to prompt buffer for debugging
    (ai-telemetry-write-context-to-prompt-buffer messages)

    `(:messages ,messages :model-context ,model :request-id ,request-id :command-config ,config)))

(defun ai-mode-indexing--index-completion-check (pending-count accumulated-summaries-ht target-buffer project-root start-time)
  "Check if indexing is complete and finalize the results.
PENDING-COUNT is the number of remaining requests.
ACCUMULATED-SUMMARIES-HT is the hash table containing completed summaries keyed by relative path.
TARGET-BUFFER is the buffer where the indexing was initiated.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (when (zerop pending-count)
    (ai-logging--verbose-message "Indexing complete for %s with %d files"
                               project-root (hash-table-count accumulated-summaries-ht))
    ;; Stop batch operation progress using ai-progress
    (ai-progress-stop-batch-operation target-buffer)
    ;; Store the list of summary structs for this project root
    (puthash project-root (hash-table-values accumulated-summaries-ht) ai-mode-indexing--project-files-summary-index)
    ;; Save to disk
    (ai-mode-indexing--save-current-index-to-disk project-root start-time)
    ;; Cleanup old versions
    (ai-mode-indexing--cleanup-old-index-versions project-root)
    (message "Project files summary index updated with %d files for project '%s'."
             (hash-table-count accumulated-summaries-ht)
             project-root)))

(defun ai-mode-indexing--wrap-callback-with-audit (callback request-id audit-type)
  "Wrap CALLBACK to complete audit appropriately when called.
REQUEST-ID is the audit request identifier.
AUDIT-TYPE should be 'success' or 'fail'."
  (lambda (&rest args)
    (cond
     ((eq audit-type 'success)
      (ai-request-audit-complete-request request-id (car args)))
     ((eq audit-type 'fail)
      (ai-request-audit-fail-request request-id (cdr args))))
    (when (functionp callback)
      (apply callback args))))

(defun ai-mode-indexing--create-file-summarization-success-callback (accumulated-summaries-ht pending-count-ref target-buffer original-file-struct project-root start-time)
  "Create success callback for file summarization.
ACCUMULATED-SUMMARIES-HT is a hash table to store summaries per file.
PENDING-COUNT-REF is a cons cell containing the pending requests counter.
TARGET-BUFFER is the buffer where indexing was initiated.
ORIGINAL-FILE-STRUCT is the original file struct passed for summarization.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (lambda (messages)
    (let* ((relative-path (plist-get original-file-struct :relative-path))
           (summary-struct (ai-response-processors--get-message messages)))
      (ai-logging--verbose-message "Processing successful indexing response for file %s" relative-path)
      (when (and summary-struct (listp summary-struct) (plist-get summary-struct :type))
        (let* ((summary-content (plist-get summary-struct :content))
               (file-path (plist-get original-file-struct :file))
               (file-size (plist-get original-file-struct :file-size))
               (final-summary-struct (ai-common--make-file-summary-struct
                                      summary-content
                                      file-path
                                      project-root
                                      :relative-path relative-path
                                      :file-size file-size)))
          ;; Store the summary struct in the local hash table, keyed by its relative path
          (puthash relative-path final-summary-struct accumulated-summaries-ht)
          (ai-logging--verbose-message "Successfully stored summary for file %s" relative-path))))

    ;; Update batch progress using ai-progress
    (ai-progress-increment-batch-processed target-buffer)
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (ai-mode-indexing--index-completion-check (car pending-count-ref)
                                    accumulated-summaries-ht
                                    target-buffer
                                    project-root
                                    start-time)))))

(defun ai-mode-indexing--create-file-summarization-fail-callback (pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create failure callback for file summarization.
PENDING-COUNT-REF is a cons cell containing the pending requests counter.
ACCUMULATED-SUMMARIES-HT is a hash table to accumulate summaries.
TARGET-BUFFER is the buffer where indexing was initiated.
ORIGINAL-FILE-STRUCT is the original file struct that was sent for summarization.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (lambda (request-data error-message)
    (let ((relative-path (plist-get original-file-struct :relative-path))
          (error-text (ai-common--get-text-content-from-struct error-message)))
      (ai-logging--verbose-message "Failed to summarize file %s: %s" relative-path error-text)
      (when (buffer-live-p target-buffer)
        (with-current-buffer target-buffer
          (message "Failed to summarize file '%s'. Error: %s" relative-path error-text))))

    ;; Update batch progress using ai-progress
    (ai-progress-increment-batch-processed target-buffer)
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (ai-mode-indexing--index-completion-check (car pending-count-ref)
                                    accumulated-summaries-ht
                                    target-buffer
                                    project-root
                                    start-time)))))

(defun ai-mode-indexing--process-files-parallel-independent (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing in parallel without context sharing.
Each file is processed independently without any existing context.
START-TIME is when the indexing process began."
  (let ((execution-backend (map-elt current-model :execution-backend)))

    (ai-logging--verbose-message "Parallel independent strategy: processing %d files without context sharing"
                               (length file-structs))

    (dolist (file-struct file-structs)
      (let ((relative-path (plist-get file-struct :relative-path)))
        (ai-logging--verbose-message "Starting parallel indexing for file %s" relative-path)
        (setcar pending-count-ref (1+ (car pending-count-ref)))

        (let* ((indexing-context (ai-mode-indexing--get-indexing-context file-struct current-model))
               (request-id (plist-get indexing-context :request-id))
               (command-config (plist-get indexing-context :command-config))
               (command (plist-get command-config :command))
               ;; Start audit for this indexing request
               (audit-request-id (ai-request-audit-start-request
                                  request-id
                                  command
                                  current-model
                                  indexing-context))
               (success-callback (ai-mode-indexing--wrap-callback-with-audit
                                  (ai-mode-indexing--create-file-summarization-success-callback
                                   accumulated-summaries-ht pending-count-ref target-buffer file-struct project-root start-time)
                                  audit-request-id
                                  'success))
               (fail-callback (ai-mode-indexing--wrap-callback-with-audit
                               (ai-mode-indexing--create-file-summarization-fail-callback
                                pending-count-ref accumulated-summaries-ht target-buffer file-struct project-root start-time)
                               audit-request-id
                               'fail)))

          (ai-execution-perform-async-backend-query
           indexing-context
           success-callback
           :fail-callback fail-callback
           :model current-model))

        ;; Introduce a configurable timeout between indexing calls
        (when (> ai-mode-indexing--call-timeout 0)
          (ai-logging--verbose-message "Waiting %s seconds before next indexing call" ai-mode-indexing--call-timeout)
          (sit-for ai-mode-indexing--call-timeout))))))

(defun ai-mode-indexing--process-files-sequential (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing sequentially with session context accumulation only.
Uses only files from current session for context accumulation, no existing index.
START-TIME is when the indexing process began."
  (let ((remaining-files (copy-sequence file-structs)))

    (ai-logging--verbose-message "Sequential strategy: processing %d files with session context accumulation only"
                               (length file-structs))

    (when remaining-files
      (ai-mode-indexing--process-next-file-sequential remaining-files current-model pending-count-ref
                                        accumulated-summaries-ht target-buffer project-root start-time))))

(defun ai-mode-indexing--process-next-file-sequential (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process the next file in REMAINING-FILES sequentially.
This is a recursive helper function that processes one file at a time.
Uses only current session context for accumulation, no existing index.
START-TIME is when the indexing process began."
  (if (null remaining-files)
      ;; No more files to process
      (progn
        (ai-logging--verbose-message "Sequential processing complete for %s" project-root)
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer
            (ai-mode-indexing--index-completion-check (car pending-count-ref)
                                        accumulated-summaries-ht
                                        target-buffer
                                        project-root
                                        start-time))))
    ;; Process the next file
    (let* ((file-struct (car remaining-files))
           (rest-files (cdr remaining-files))
           (execution-backend (map-elt current-model :execution-backend))
           (relative-path (plist-get file-struct :relative-path))
           ;; Use only accumulated session summaries for context - no existing index
           (session-summaries (when ai-mode-indexing--include-existing-context
                                (hash-table-values accumulated-summaries-ht))))

      (ai-logging--verbose-message "Sequential step: processing file '%s' with %d session summaries for context"
                                 relative-path (length (or session-summaries '())))

      (setcar pending-count-ref (1+ (car pending-count-ref)))

      (let* ((indexing-context (ai-mode-indexing--get-indexing-context-with-summaries file-struct current-model session-summaries))
             (request-id (plist-get indexing-context :request-id))
             (command-config (plist-get indexing-context :command-config))
             (command (plist-get command-config :command))
             ;; Start audit for this indexing request
             (audit-request-id (ai-request-audit-start-request
                                request-id
                                command
                                current-model
                                indexing-context))
             (success-callback (ai-mode-indexing--wrap-callback-with-audit
                                (ai-mode-indexing--create-sequential-success-callback
                                 rest-files current-model pending-count-ref
                                 accumulated-summaries-ht target-buffer
                                 file-struct project-root start-time)
                                audit-request-id
                                'success))
             (fail-callback (ai-mode-indexing--wrap-callback-with-audit
                             (ai-mode-indexing--create-sequential-fail-callback
                              rest-files current-model pending-count-ref
                              accumulated-summaries-ht target-buffer
                              file-struct project-root start-time)
                             audit-request-id
                             'fail)))

        (ai-execution-perform-async-backend-query
         indexing-context
         success-callback
         :fail-callback fail-callback
         :model current-model)))))

(defun ai-mode-indexing--create-sequential-success-callback (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create success callback for sequential file processing.
START-TIME is when the indexing process began."
  (lambda (messages)
    (let* ((relative-path (plist-get original-file-struct :relative-path))
           (summary-struct (ai-response-processors--get-message messages)))
      (ai-logging--verbose-message "Sequential success callback for file %s" relative-path)
      ;; Process the successful response
      (when (and summary-struct (listp summary-struct) (plist-get summary-struct :type))
        (let* ((summary-content (plist-get summary-struct :content))
               (file-path (plist-get original-file-struct :file))
               (file-size (plist-get original-file-struct :file-size))
               (final-summary-struct (ai-common--make-file-summary-struct
                                      summary-content
                                      file-path
                                      project-root
                                      :relative-path relative-path
                                      :file-size file-size)))
          ;; Store the summary struct in the accumulated hash table
          (puthash relative-path final-summary-struct accumulated-summaries-ht)
          (ai-logging--verbose-message "Sequential success: added summary for '%s', total session summaries: %d"
                                     relative-path
                                     (hash-table-count accumulated-summaries-ht)))))

    ;; Update batch progress using ai-progress
    (ai-progress-increment-batch-processed target-buffer)
    (setcar pending-count-ref (1- (car pending-count-ref)))

    ;; Continue with next file
    (ai-mode-indexing--process-next-file-sequential remaining-files current-model pending-count-ref
                                      accumulated-summaries-ht target-buffer project-root start-time)))

(defun ai-mode-indexing--create-sequential-fail-callback (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create failure callback for sequential file processing.
START-TIME is when the indexing process began."
  (lambda (request-data error-message)
    (let ((relative-path (plist-get original-file-struct :relative-path))
          (error-text (ai-common--get-text-content-from-struct error-message)))
      (ai-logging--verbose-message "Sequential fail callback for file %s: %s" relative-path error-text)
      ;; Log the error but continue processing
      (when (buffer-live-p target-buffer)
        (with-current-buffer target-buffer
          (message "Failed to summarize file '%s'. Error: %s" relative-path error-text))))

    ;; Update batch progress using ai-progress
    (ai-progress-increment-batch-processed target-buffer)
    (setcar pending-count-ref (1- (car pending-count-ref)))

    ;; Continue with next file despite the error
    (ai-mode-indexing--process-next-file-sequential remaining-files current-model pending-count-ref
                                      accumulated-summaries-ht target-buffer project-root start-time)))

(defun ai-mode-indexing--process-file-structs-for-indexing (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing using the configured strategy.
Dispatches to either parallel-independent or sequential processing based on ai-mode-indexing--strategy.
START-TIME is when the indexing process began."
  (ai-logging--verbose-message "Processing %d files for indexing using %s strategy"
                             (length file-structs) (symbol-name ai-mode-indexing--strategy))
  (pcase ai-mode-indexing--strategy
    ('parallel-independent
     (ai-mode-indexing--process-files-parallel-independent file-structs current-model pending-count-ref
                                             accumulated-summaries-ht target-buffer project-root start-time))
    ('sequential
     (ai-mode-indexing--process-files-sequential file-structs current-model pending-count-ref
                                   accumulated-summaries-ht target-buffer project-root start-time))
    (_
     ;; Fallback to parallel-independent for unknown strategies
     (ai-logging--verbose-message "Unknown indexing strategy %s, falling back to parallel-independent"
                                (symbol-name ai-mode-indexing--strategy))
     (ai-mode-indexing--process-files-parallel-independent file-structs current-model pending-count-ref
                                             accumulated-summaries-ht target-buffer project-root start-time))))

(defun ai-mode-indexing-update-project-files-summary-index ()
  "Update the project files summary index with current project files.
This command populates `ai-mode-indexing--project-files-summary-index` with typed structures
from the current project for use in project AI summary context mode."
  (if-let ((project-root (ai-project--get-project-root)))
      (let* ((filtered-file-structs (ai-project--get-filtered-project-files-as-structs))
             (total-files (length filtered-file-structs))
             ;; Local hash table to collect summaries for the *current* project indexing run
             (accumulated-summaries-for-this-run (make-hash-table :test 'equal))
             (pending-count-ref (cons 0 nil))
             (current-model (ai-model-management-get-current))
             (execution-backend (map-elt current-model :execution-backend))
             (target-buffer (current-buffer))
             (start-time (current-time)))

        (ai-logging--verbose-message "Starting project indexing for %s with %d files" project-root total-files)

        (unless execution-backend
          (user-error "No AI execution backend defined for current model: %s" (map-elt current-model :name)))

        (when (zerop total-files)
          (message "No project files found to index or all are ignored. Clearing index for project '%s'." project-root)
          (ai-logging--verbose-message "No files to index for project %s, clearing index" project-root)
          (remhash project-root ai-mode-indexing--project-files-summary-index)
          (cl-return-from ai-mode-indexing-update-project-files-summary-index))

        (message "Indexing %d project files using %s strategy for project '%s'... This may take a while."
                 total-files
                 (symbol-name ai-mode-indexing--strategy)
                 project-root)

        ;; Start batch operation progress using ai-progress
        (ai-progress-start-batch-operation "Indexing project files" target-buffer total-files)

        (ai-mode-indexing--process-file-structs-for-indexing filtered-file-structs
                                               current-model
                                               pending-count-ref
                                               accumulated-summaries-for-this-run
                                               target-buffer
                                               project-root
                                               start-time))
    (message "Not in a project. Cannot update project files summary index."))
  nil)

(defun ai-mode-indexing-switch-indexing-strategy ()
  "Interactively switch the indexing strategy."
  (let* ((current-strategy ai-mode-indexing--strategy)
         (strategies '(("parallel-independent" . parallel-independent)
                       ("sequential" . sequential)))
         (strategy-descriptions '((parallel-independent . "Parallel processing without context sharing between files")
                                  (sequential . "Sequential processing with accumulating context from current session only")))
         (prompt (format "Current strategy: %s. Select new indexing strategy: "
                        (cdr (assoc current-strategy strategy-descriptions))))
         (selected-name (completing-read prompt (mapcar #'car strategies)))
         (selected-strategy (cdr (assoc selected-name strategies))))

    (setq ai-mode-indexing--strategy selected-strategy)
    (customize-save-variable 'ai-mode-indexing--strategy selected-strategy)
    (message "Indexing strategy changed to: %s (%s)"
             selected-name
             (cdr (assoc selected-strategy strategy-descriptions)))
    (ai-logging--verbose-message "Indexing strategy changed to %s" selected-name)))

(defun ai-mode-indexing-select-index-version ()
  "Interactively select and load an index version for the current project."
  (if-let ((project-root (ai-project--get-project-root)))
      (let ((versions (ai-mode-indexing--get-available-index-versions project-root)))
        (if versions
            (let* ((version-displays (mapcar #'ai-mode-indexing--format-index-version-display versions))
                   (version-alist (cl-mapcar #'cons version-displays versions))
                   (selected-display (completing-read "Select index version: " version-displays))
                   (selected-version (cdr (assoc selected-display version-alist))))
              (when selected-version
                (ai-logging--verbose-message "User selected index version %s" selected-version)
                (ai-mode-indexing--load-index-version project-root selected-version)))
          (message "No index versions found for project")))
    (message "Not in a project. Cannot select index version.")))

(defun ai-mode-indexing-list-index-versions ()
  "List all available index versions for the current project."
  (if-let ((project-root (ai-project--get-project-root)))
      (let ((versions (ai-mode-indexing--get-available-index-versions project-root)))
        (if versions
            (with-output-to-temp-buffer "*AI Index Versions*"
              (princ (format "Index versions for project: %s\n\n" project-root))
              (dolist (version versions)
                (princ (format "• %s\n" (ai-mode-indexing--format-index-version-display version)))
                (when-let ((metadata (ai-mode-indexing--load-index-metadata project-root version)))
                  (princ (format "  Strategy: %s, Files: %s\n"
                                (alist-get 'strategy metadata "unknown")
                                (alist-get 'file-count metadata "unknown"))))))
          (message "No index versions found for project")))
    (message "Not in a project. Cannot list index versions.")))

(defun ai-mode-indexing-delete-old-index-versions ()
  "Interactively delete old index versions beyond retention depth."
  (if-let ((project-root (ai-project--get-project-root)))
      (let ((versions (ai-mode-indexing--get-available-index-versions project-root)))
        (if (> (length versions) ai-mode-indexing--index-retention-depth)
            (let ((versions-to-delete (nthcdr ai-mode-indexing--index-retention-depth versions)))
              (when (yes-or-no-p (format "Delete %d old index versions? " (length versions-to-delete)))
                (ai-logging--verbose-message "User confirmed deletion of %d old index versions" (length versions-to-delete))
                (ai-mode-indexing--cleanup-old-index-versions project-root)))
          (message "No old index versions to delete (current: %d, retention: %d)"
                   (length versions) ai-mode-indexing--index-retention-depth)))
    (message "Not in a project. Cannot delete index versions.")))

(defun ai-mode-indexing-toggle-indexing-context ()
  "Toggle inclusion of existing context in indexing process."
  (setq ai-mode-indexing--include-existing-context
        (not ai-mode-indexing--include-existing-context))
  (customize-save-variable 'ai-mode-indexing--include-existing-context ai-mode-indexing--include-existing-context)
  (message "Indexing context inclusion: %s"
           (if ai-mode-indexing--include-existing-context "enabled" "disabled"))
  (ai-logging--verbose-message "Indexing context inclusion toggled to %s"
                             (if ai-mode-indexing--include-existing-context "enabled" "disabled")))

(defun ai-mode-indexing-reindex-project-with-context ()
  "Reindex the entire project with existing context enabled."
  (let ((ai-mode-indexing--include-existing-context t))
    (ai-logging--verbose-message "Starting project reindex with context enabled")
    (ai-mode-indexing-update-project-files-summary-index)))

(defun ai-mode-indexing-get-project-ai-summary-context ()
  "Get project context using project AI summary mode with cached index.
Returns a typed struct containing the project files summary from cached index,
or nil if no project is detected or index is empty."
  (when-let* ((project-root (ai-project--get-project-root))
              (files-list (ai-project--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai-mode-indexing--project-files-summary-index)))
    (let* ((files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count (length files-list)))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root)))
      (ai-logging--verbose-message "Generated project AI summary context for %s with %d indexed files"
                                 project-root (length summaries-for-current-project))
      project-struct)))

(defun ai-mode-indexing-get-enhanced-project-ai-summary-context ()
  "Get enhanced project context using project AI summary mode with dependency awareness."
  (when-let* ((project-root (ai-project--get-project-root))
              (files-list (ai-project--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai-mode-indexing--project-files-summary-index)))
    (let* ((files-count (length files-list))
           (indexed-count (length summaries-for-current-project))
           (files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count files-count
                              :indexed-count indexed-count))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index
                         :has-context (> indexed-count 0)))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root
                           :indexing-mode "enhanced")))
      (ai-logging--verbose-message "Generated enhanced project AI summary context for %s: %d/%d files indexed"
                                 project-root indexed-count files-count)
      project-struct)))

(provide 'ai-mode-indexing)

;;; ai-mode-indexing.el ends here
