;;; ai-request-audit.el --- Request auditing system for AI mode -*- lexical-binding: t -*-

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
;; This module provides a structured request auditing system for AI mode,
;; replacing the simple buffer-based logging with a comprehensive audit trail.
;; Features include:
;; - Structured storage of request/response data in JSON format
;; - Unique request identification and tracking
;; - Project-based audit organization
;; - Interactive request browsing and detailed views
;; - Token usage statistics tracking
;; - Automatic cleanup of old audit records

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magit-section)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-project)
(require 'ai-logging)

(defcustom ai-request-audit-enabled t
  "Enable request auditing for AI mode.
When enabled, saves detailed information about each AI request including
context, model configuration, requests, and responses."
  :type 'boolean
  :group 'ai-mode)

(defcustom ai-request-audit-max-records 100
  "Maximum number of audit records to keep per project.
When exceeded, oldest records will be automatically deleted if cleanup is enabled."
  :type 'integer
  :group 'ai-mode)

(defcustom ai-request-audit-cleanup-old-records t
  "Automatically cleanup old audit records when max-records is exceeded.
When enabled, oldest records are deleted to maintain the specified limit."
  :type 'boolean
  :group 'ai-mode)

(defun ai-request-audit--get-audit-directory (project-root)
  "Get the audit directory path for PROJECT-ROOT."
  (let ((audit-dir (file-name-as-directory (expand-file-name ".ai/audit" project-root))))
    audit-dir))

(defun ai-request-audit--get-request-directory (project-root request-id)
  "Get the directory path for REQUEST-ID in PROJECT-ROOT."
  (let ((request-dir (file-name-as-directory
                      (expand-file-name request-id (ai-request-audit--get-audit-directory project-root)))))
    request-dir))

(defun ai-request-audit--ensure-audit-directory (project-root)
  "Ensure audit directory exists for PROJECT-ROOT."
  (let ((audit-dir (ai-request-audit--get-audit-directory project-root)))
    (unless (file-directory-p audit-dir)
      (ai-logging--verbose-message "Creating audit directory: %s" audit-dir)
      (make-directory audit-dir t))
    audit-dir))

(defun ai-request-audit--ensure-request-directory (project-root request-id)
  "Ensure request directory exists for REQUEST-ID in PROJECT-ROOT."
  (let ((request-dir (ai-request-audit--get-request-directory project-root request-id)))
    (unless (file-directory-p request-dir)
      (ai-logging--verbose-message "Creating request directory: %s" request-dir)
      (make-directory request-dir t))
    request-dir))

(defun ai-request-audit--create-request-metadata (request-id command model)
  "Create metadata structure for REQUEST-ID with COMMAND and MODEL."
  (ai-logging--verbose-message "Creating request metadata for %s" request-id)
  `((request-id . ,request-id)
    (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
    (command . ,(format "%s" command))
    (model-name . ,(map-elt model :name))
    (model-provider . ,(map-elt model :provider))
    (status . "in-progress")
    (start-time . ,(float-time))
    (end-time . nil)
    (execution-time . nil)
    (error-message . nil)))

(defun ai-request-audit--update-request-status (project-root request-id status &optional error-message)
  "Update the status of REQUEST-ID in PROJECT-ROOT to STATUS with optional ERROR-MESSAGE."
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (metadata-file (expand-file-name "metadata.json" request-dir)))
    (ai-logging--verbose-message "Updating request status for %s to %s" request-id status)
    (when (file-exists-p metadata-file)
      (ai-logging--verbose-message "Loading metadata from %s" metadata-file)
      (let* ((metadata (json-read-file metadata-file))
             (start-time (alist-get 'start-time metadata))
             (end-time (float-time))
             (execution-time (when start-time (- end-time start-time))))
        (setf (alist-get 'status metadata) status)
        (setf (alist-get 'end-time metadata) end-time)
        (setf (alist-get 'execution-time metadata) execution-time)
        (when error-message
          (setf (alist-get 'error-message metadata) error-message))
        (ai-logging--verbose-message "Saving updated metadata to %s" metadata-file)
        (ai-request-audit--save-json-file metadata-file metadata)))))

(defun ai-request-audit--save-json-file (file-path data)
  "Save DATA as JSON to FILE-PATH with human-readable formatting.
DATA should be in alist format for consistent JSON encoding."
  (ai-logging--verbose-message "Saving JSON data to %s" file-path)
  (let* ((json-encoding-pretty-print t)
         (json-encoding-default-indentation "  ")
         (dir (file-name-directory file-path)))
    ;; Ensure directory exists before writing
    (unless (file-directory-p dir)
      (ai-logging--verbose-message "Creating directory for JSON file: %s" dir)
      (make-directory dir t))

    (with-temp-file file-path
      (insert (json-encode data)))
    (ai-logging--verbose-message "JSON data saved successfully to %s" file-path)))

(defun ai-request-audit--save-text-file (file-path data)
  "Save DATA as plain text to FILE-PATH."
  (ai-logging--verbose-message "Saving text data to %s" file-path)
  (let ((dir (file-name-directory file-path)))
    ;; Ensure directory exists before writing
    (unless (file-directory-p dir)
      (ai-logging--verbose-message "Creating directory for text file: %s" dir)
      (make-directory dir t))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file-path
        (insert (if (stringp data) data (format "%s" data)))))
    (ai-logging--verbose-message "Text data saved successfully to %s" file-path)))

(defun ai-request-audit--save-context (project-root request-id context)
  "Save CONTEXT data for REQUEST-ID in PROJECT-ROOT."
  (ai-logging--verbose-message "Saving context for request %s" request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (context-file (expand-file-name "context.json" request-dir)))
    (ai-request-audit--save-json-file context-file (ai-common--plist-to-alist context))))

(defun ai-request-audit--save-request (project-root request-id request-data)
  "Save REQUEST-DATA for REQUEST-ID in PROJECT-ROOT as plain text."
  (ai-logging--verbose-message "Saving request data for request %s" request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (request-file (expand-file-name "request.txt" request-dir)))
    (ai-request-audit--save-text-file request-file request-data)))

(defun ai-request-audit--save-response (project-root request-id response-data response-type)
  "Save RESPONSE-DATA for REQUEST-ID in PROJECT-ROOT with RESPONSE-TYPE (raw, processed, or error).
Raw responses are saved as plain text, processed and error responses as JSON."
  (ai-logging--verbose-message "Saving %s response for request %s" response-type request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id)))
    (if (string= response-type "raw")
        ;; Save raw response as plain text
        (let ((response-file (expand-file-name "raw_response.txt" request-dir)))
          (ai-request-audit--save-text-file response-file response-data))
      ;; Save processed or error response as JSON
      (let ((response-file (expand-file-name
                           (if (string= response-type "error")
                               "error_response.json"
                             "processed_response.json")
                           request-dir)))
        ;; Convert plist to alist for JSON encoding if needed
        (let ((data-for-json (ai-common--plist-to-alist response-data)))
          (ai-request-audit--save-json-file response-file data-for-json))))))

(defun ai-request-audit--save-model-config (project-root request-id model-config)
  "Save MODEL-CONFIG for REQUEST-ID in PROJECT-ROOT."
  (ai-logging--verbose-message "Saving model config for request %s" request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (config-file (expand-file-name "model_config.json" request-dir)))
    (ai-request-audit--save-json-file config-file model-config)))

(defun ai-request-audit--save-command-config (project-root request-id command-config)
  "Save COMMAND-CONFIG for REQUEST-ID in PROJECT-ROOT."
  (ai-logging--verbose-message "Saving command config for request %s" request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (config-file (expand-file-name "command_config.json" request-dir)))
    (ai-request-audit--save-json-file config-file command-config)))

(defun ai-request-audit--save-usage-stats (project-root request-id usage-stats)
  "Save USAGE-STATS for REQUEST-ID in PROJECT-ROOT."
  (ai-logging--verbose-message "Saving usage stats for request %s" request-id)
  (ai-request-audit--ensure-request-directory project-root request-id)
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (stats-file (expand-file-name "usage_stats.json" request-dir)))
    (ai-request-audit--save-json-file stats-file usage-stats)))

(defun ai-request-audit--get-all-request-directories (project-root)
  "Get all request directories for PROJECT-ROOT."
  (let* ((audit-dir (ai-request-audit--get-audit-directory project-root)))
    (when (file-directory-p audit-dir)
      (let ((dirs (directory-files audit-dir t "^[^.].*" t)))
        (cl-remove-if-not #'file-directory-p dirs)))))

(defun ai-request-audit--load-metadata-from-directories (project-root)
  "Load metadata from all request directories in PROJECT-ROOT."
  (let* ((request-dirs (ai-request-audit--get-all-request-directories project-root))
         (metadata-list '()))
    (dolist (request-dir request-dirs)
      (let* ((metadata-file (expand-file-name "metadata.json" request-dir)))
        (when (file-exists-p metadata-file)
          (condition-case err
              (let ((metadata (json-read-file metadata-file)))
                (push metadata metadata-list))
            (error
             (ai-logging--verbose-message "Error loading metadata from %s: %s"
                                          metadata-file (error-message-string err)))))))
    ;; Sort by timestamp (newest first)
    (sort metadata-list (lambda (a b)
                          (string> (or (alist-get 'timestamp a) "")
                                   (or (alist-get 'timestamp b) ""))))))

(defun ai-request-audit--add-request-to-index (index request-metadata)
  "Add REQUEST-METADATA to INDEX and return updated index."
  (push request-metadata index)
  ;; Sort by timestamp (newest first)
  (sort index (lambda (a b)
                (string> (alist-get 'timestamp a "")
                         (alist-get 'timestamp b "")))))

(defun ai-request-audit--cleanup-old-records (project-root index)
  "Cleanup old records from INDEX for PROJECT-ROOT if cleanup is enabled.
Returns the cleaned up index."
  (if (and ai-request-audit-cleanup-old-records
           (> (length index) ai-request-audit-max-records))
      (progn
        (ai-logging--verbose-message "Cleaning up old audit records, keeping %d records" ai-request-audit-max-records)
        (let ((records-to-remove (nthcdr ai-request-audit-max-records index)))
          (dolist (record records-to-remove)
            (let* ((request-id (alist-get 'request-id record))
                   (request-dir (ai-request-audit--get-request-directory project-root request-id)))
              (when (file-directory-p request-dir)
                (ai-logging--verbose-message "Removing old audit record directory: %s" request-dir)
                (delete-directory request-dir t))))
          ;; Return truncated index
          (cl-subseq index 0 ai-request-audit-max-records)))
    ;; Return original index if no cleanup needed
    index))

(defun ai-request-audit--save-updated-index (project-root index)
  "Save the updated INDEX for PROJECT-ROOT."
  (let* ((audit-dir (ai-request-audit--get-audit-directory project-root))
         (index-file (expand-file-name "index.json" audit-dir)))
    (ai-logging--verbose-message "Saving updated index to %s" index-file)
    (ai-request-audit--save-json-file index-file index)))

(defun ai-request-audit--update-index (project-root request-metadata)
  "Update the audit index for PROJECT-ROOT with REQUEST-METADATA."
  (ai-logging--verbose-message "Updating audit index for project %s" project-root)
  ;; Ensure audit directory exists
  (ai-request-audit--ensure-audit-directory project-root)
  ;; Load existing index from metadata files
  (let ((index (ai-request-audit--load-metadata-from-directories project-root)))
    ;; Cleanup old records if needed
    (setq index (ai-request-audit--cleanup-old-records project-root index))
    ;; Save updated index
    (ai-request-audit--save-updated-index project-root index)))

(defun ai-request-audit-start-request (request-id command model context)
  "Start auditing a new request with REQUEST-ID, COMMAND, MODEL, and CONTEXT.
REQUEST-ID should be a unique identifier for the request."
  (when ai-request-audit-enabled
    (ai-logging--verbose-message "Starting audit for request %s" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      (ai-request-audit--ensure-audit-directory project-root)
      (let* ((metadata (ai-request-audit--create-request-metadata request-id command model)))

        ;; Ensure request directory exists
        (ai-request-audit--ensure-request-directory project-root request-id)

        ;; Save initial metadata
        (ai-request-audit--save-context project-root request-id context)
        (ai-request-audit--save-model-config project-root request-id model)

        ;; Save metadata file
        (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
               (metadata-file (expand-file-name "metadata.json" request-dir)))
          (ai-logging--verbose-message "Saving initial metadata to %s" metadata-file)
          (ai-request-audit--save-json-file metadata-file metadata))

        ;; Update index
        (ai-request-audit--update-index project-root metadata)

        request-id))))

(defun ai-request-audit-log-request (request-id request-data)
  "Log REQUEST-DATA for REQUEST-ID."
  (when (and ai-request-audit-enabled request-id)
    (ai-logging--verbose-message "Logging request data for %s" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      (ai-request-audit--save-request project-root request-id request-data))))

(defun ai-request-audit-log-raw-response (request-id response-data)
  "Log raw RESPONSE-DATA for REQUEST-ID."
  (when (and ai-request-audit-enabled request-id)
    (ai-logging--verbose-message "Logging raw response for %s" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      (ai-request-audit--save-response project-root request-id response-data "raw"))))

(defun ai-request-audit-log-usage-stats (request-id usage-stats)
  "Log USAGE-STATS for REQUEST-ID."
  (when (and ai-request-audit-enabled request-id)
    (ai-logging--verbose-message "Logging usage stats for %s" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      ;; Convert plist to alist for JSON encoding
      (let ((stats-alist (ai-common--plist-to-alist usage-stats)))
        (ai-request-audit--save-usage-stats project-root request-id stats-alist)))))

(defun ai-request-audit-complete-request (request-id processed-response &optional usage-stats)
  "Complete audit for REQUEST-ID with PROCESSED-RESPONSE and optional USAGE-STATS."
  (when (and ai-request-audit-enabled request-id)
    (ai-logging--verbose-message "Completing audit for request %s" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      (ai-request-audit--save-response project-root request-id processed-response "processed")
      (when usage-stats
        (ai-request-audit-log-usage-stats request-id usage-stats))
      (ai-request-audit--update-request-status project-root request-id "success"))))

(defun ai-request-audit-fail-request (request-id error-data)
  "Mark REQUEST-ID as failed with ERROR-DATA."
  (when (and ai-request-audit-enabled request-id)
    (ai-logging--verbose-message "Marking request %s as failed" request-id)
    (when-let ((project-root (ai-project--get-project-root)))
      ;; Save error response data
      (ai-request-audit--save-response project-root request-id error-data "error")
      ;; Update status with error message
      (let ((error-message (if (stringp error-data)
                               error-data
                             (format "%s" error-data))))
        (ai-request-audit--update-request-status project-root request-id "error" error-message)))))

(defun ai-request-audit--load-index (project-root)
  "Load audit index for PROJECT-ROOT from metadata files."
  (ai-logging--verbose-message "Loading audit index from metadata files for %s" project-root)
  (ai-request-audit--load-metadata-from-directories project-root))

(defun ai-request-audit--load-usage-stats-for-request (project-root request-id)
  "Load usage statistics for REQUEST-ID from PROJECT-ROOT."
  (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
         (stats-file (expand-file-name "usage_stats.json" request-dir)))
    (when (file-exists-p stats-file)
      (condition-case err
          (json-read-file stats-file)
        (error
         (ai-logging--verbose-message "Error loading usage stats from %s: %s"
                                      stats-file (error-message-string err))
         nil)))))

(defun ai-request-audit--format-usage-stats (usage-stats)
  "Format USAGE-STATS for display in the table.
Returns a string in format: input-tokens/output-tokens/thoughts-tokens/total-tokens"
  (if usage-stats
      (let* ((input-tokens (or (alist-get 'input-tokens usage-stats)
                               (alist-get 'input_tokens usage-stats) 0))
             (output-tokens (or (alist-get 'output-tokens usage-stats)
                                (alist-get 'output_tokens usage-stats) 0))
             (thoughts-tokens (or (alist-get 'thoughts-tokens usage-stats)
                                  (alist-get 'thoughts_tokens usage-stats) 0))
             (total-tokens (or (alist-get 'total-tokens usage-stats)
                               (alist-get 'total_tokens usage-stats)
                               (+ input-tokens output-tokens thoughts-tokens))))
        (format "%7d + %7d + %7d = %7d" input-tokens output-tokens thoughts-tokens total-tokens)

        )
    "N/A"))

(defun ai-request-audit--format-request-for-display (request-metadata project-root)
  "Format REQUEST-METADATA for display in requests list with PROJECT-ROOT for loading usage stats."
  (let* ((timestamp (alist-get 'timestamp request-metadata))
         (request-id (alist-get 'request-id request-metadata))
         (command (alist-get 'command request-metadata))
         (status (alist-get 'status request-metadata))
         (execution-time (alist-get 'execution-time request-metadata))
         (time-str (if execution-time
                       (format "%.2fs" execution-time)
                     "N/A"))
         (status-indicator (cond
                           ((string= status "success") "✓")
                           ((string= status "error") "✗")
                           ((string= status "in-progress") "⟲")
                           (t "?")))
         (usage-stats (ai-request-audit--load-usage-stats-for-request project-root request-id))
         (usage-str (ai-request-audit--format-usage-stats usage-stats)))
    (format "%-19s %s %-12s %-18s %8s %-38s %s"
            timestamp
            status-indicator
            status
            (truncate-string-to-width command 18)
            time-str
            usage-str
            request-id)))

(defun ai-request-audit--get-request-id-at-line ()
  "Get the request ID from the current line in the audit buffer."
  (save-excursion
    (beginning-of-line)
    (when (looking-at ".*\\([0-9]\\{8\\}_[0-9]\\{6\\}_req_[a-f0-9]\\{8\\}\\)\\s-*$")
      (match-string 1))))

(defun ai-request-audit--show-details-at-point ()
  "Show details for the request at the current line."
  (interactive)
  (if-let ((request-id (ai-request-audit--get-request-id-at-line)))
      (ai-request-audit-show-request-details request-id)
    (message "No request found at current line")))

(defvar ai-request-audit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'ai-request-audit--show-details-at-point)
    (define-key map (kbd "SPC") 'ai-request-audit--show-details-at-point)
    (define-key map (kbd "d") 'ai-request-audit--show-details-at-point)
    (define-key map (kbd "r") 'ai-request-audit-show-requests-buffer)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for AI Request Audit mode.")

(define-derived-mode ai-request-audit-mode special-mode "AI-Request-Audit"
  "Major mode for AI Request Audit buffer.
\\{ai-request-audit-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun ai-request-audit-show-requests-buffer ()
  "Show buffer with list of audit requests for current project."
  (interactive)
  (if-let ((project-root (ai-project--get-project-root)))
      (let* ((index (ai-request-audit--load-index project-root))
             (buffer-name "*AI Request Audit*")
             (buffer (get-buffer-create buffer-name)))
        (ai-logging--verbose-message "Showing audit requests for project %s" project-root)
        (with-current-buffer buffer
          (ai-request-audit-mode)
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (format "AI Request Audit - Project: %s\n\n" project-root))
          (insert "Press RET or SPC to show details, 'd' for details, 'r' to refresh, 'q' to quit\n\n")
          (insert (format "%-19s %s %-12s %-18s %8s %-38s %s\n"
                          "Timestamp" "S" "Status" "Command" "Time" "Usage Stats" "Request ID"))
          (insert (make-string 140 ?-) "\n")

          (if index
              (dolist (request index)
                (insert (ai-request-audit--format-request-for-display request project-root) "\n"))
            (insert "No audit records found.\n"))

          (setq buffer-read-only t)
          (goto-char (point-min))
          ;; Move to first data line (skip headers)
          (forward-line 5))
        (display-buffer buffer))
    (message "Not in a project. Cannot show audit requests.")))

;; Magit-section based request details visualization

(defvar ai-request-audit-details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "TAB") 'magit-section-toggle)
    (define-key map (kbd "C-<tab>") 'magit-section-cycle)
    (define-key map (kbd "M-<tab>") 'magit-section-cycle-diffs)
    map)
  "Keymap for AI Request Audit Details mode.")

(define-derived-mode ai-request-audit-details-mode magit-section-mode "AI-Request-Details"
  "Major mode for viewing AI request audit details using magit-section.
\\{ai-request-audit-details-mode-map}"
  (setq buffer-read-only t))

(defun ai-request-audit--load-file-content (filepath)
  "Load content from FILEPATH, handling both JSON and text files."
  (when (file-exists-p filepath)
    (condition-case err
        (cond
         ;; Handle JSON files
         ((string-suffix-p ".json" filepath)
          (let ((data (json-read-file filepath)))
            (with-temp-buffer
              (let ((json-encoding-pretty-print t)
                    (json-encoding-default-indentation "  "))
                (insert (json-encode data))
                (buffer-string)))))
         ;; Handle plain text files
         ((string-suffix-p ".txt" filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (buffer-string)))
         ;; Default case
         (t
          (with-temp-buffer
            (insert-file-contents filepath)
            (buffer-string))))
      (error
       (format "Error reading file: %s" (error-message-string err))))))

(defun ai-request-audit--insert-metadata-section (metadata)
  "Insert metadata section for request details."
  (magit-insert-section (metadata)
    (magit-insert-heading "METADATA")
    (dolist (item metadata)
      (insert (format "  %s: %s\n" (car item) (cdr item))))
    (insert "\n")))

(defun ai-request-audit--insert-usage-stats-section (usage-stats)
  "Insert usage statistics section for request details."
  (magit-insert-section (usage-stats)
    (magit-insert-heading "USAGE STATISTICS")
    (dolist (item usage-stats)
      (insert (format "  %s: %s\n" (car item) (cdr item))))
    (insert "\n")))

(defun ai-request-audit--insert-file-section (file-info request-dir)
  "Insert a collapsible section for FILE-INFO in REQUEST-DIR."
  (let* ((filename (car file-info))
         (title (cadr file-info))
         (filepath (expand-file-name filename request-dir))
         (content (ai-request-audit--load-file-content filepath)))
    (when content
      (magit-insert-section (audit-file filename t)
        (magit-insert-heading title)
        (insert content)
        (insert "\n\n")))))

(defun ai-request-audit--get-available-files (request-dir)
  "Get list of available audit files in REQUEST-DIR."
  (let ((file-definitions '(("model_config.json" "MODEL CONFIG")
                           ("command_config.json" "COMMAND CONFIG")
                           ("context.json" "CONTEXT")
                           ("request.txt" "REQUEST")
                           ("raw_response.txt" "RAW RESPONSE")
                           ("processed_response.json" "PROCESSED RESPONSE")
                           ("error_response.json" "ERROR RESPONSE"))))
    (cl-remove-if-not (lambda (file-info)
                        (file-exists-p (expand-file-name (car file-info) request-dir)))
                      file-definitions)))

(defun ai-request-audit--insert-all-file-sections (request-dir)
  "Insert all available file sections for REQUEST-DIR."
  (let ((available-files (ai-request-audit--get-available-files request-dir)))
    (dolist (file-info available-files)
      (ai-request-audit--insert-file-section file-info request-dir))))

(defun ai-request-audit--load-request-metadata (request-dir)
  "Load request metadata from REQUEST-DIR."
  (let ((metadata-file (expand-file-name "metadata.json" request-dir)))
    (when (file-exists-p metadata-file)
      (json-read-file metadata-file))))

(defun ai-request-audit--load-usage-stats (request-dir)
  "Load usage statistics from REQUEST-DIR."
  (let ((stats-file (expand-file-name "usage_stats.json" request-dir)))
    (when (file-exists-p stats-file)
      (json-read-file stats-file))))

(defun ai-request-audit--create-details-buffer (request-id)
  "Create and setup the details buffer for REQUEST-ID."
  (let* ((buffer-name (format "*AI Request Details: %s*" request-id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (ai-request-audit-details-mode)
      (setq buffer-read-only nil)
      (erase-buffer))
    buffer))

(defun ai-request-audit-show-request-details (request-id)
  "Show detailed view of REQUEST-ID using magit-section for collapsible sections."
  (interactive
   (list (if-let ((project-root (ai-project--get-project-root)))
             (let* ((index (ai-request-audit--load-index project-root))
                    (request-ids (mapcar (lambda (req) (alist-get 'request-id req)) index)))
               (completing-read "Select request ID: " request-ids nil t))
           (user-error "Not in a project"))))

  (if-let ((project-root (ai-project--get-project-root)))
      (let* ((request-dir (ai-request-audit--get-request-directory project-root request-id))
             (metadata-file (expand-file-name "metadata.json" request-dir))
             (buffer (ai-request-audit--create-details-buffer request-id)))

        (ai-logging--verbose-message "Showing details for request %s" request-id)
        (unless (file-exists-p metadata-file)
          (user-error "Request %s not found" request-id))

        (with-current-buffer buffer
          (let* ((metadata (ai-request-audit--load-request-metadata request-dir))
                 (usage-stats (ai-request-audit--load-usage-stats request-dir)))
            ;; Insert everything under a root section
            (magit-insert-section (request-details request-id t)
              (magit-insert-heading (format "Request Details: %s" request-id))

              ;; Insert metadata section
              (when metadata
                (ai-request-audit--insert-metadata-section metadata))

              ;; Insert usage statistics section
              (when usage-stats
                (ai-request-audit--insert-usage-stats-section usage-stats))

              ;; Insert all file sections
              (ai-request-audit--insert-all-file-sections request-dir))

            ;; Hide all sections by default
            (magit-section-hide-children magit-root-section)

            ;; Enable line wrapping for long lines
            (setq truncate-lines nil)

            ;; Make buffer read-only and go to top
            (setq buffer-read-only t)
            (goto-char (point-min))))

        (display-buffer buffer))
    (message "Not in a project. Cannot show request details.")))

(defun ai-request-audit-toggle ()
  "Toggle request auditing on/off."
  (interactive)
  (setq ai-request-audit-enabled (not ai-request-audit-enabled))
  (customize-save-variable 'ai-request-audit-enabled ai-request-audit-enabled)
  (message "AI request auditing %s" (if ai-request-audit-enabled "enabled" "disabled")))

(defun ai-request-audit-cleanup-project ()
  "Clean up all audit records for current project."
  (interactive)
  (if-let ((project-root (ai-project--get-project-root)))
      (let ((audit-dir (ai-request-audit--get-audit-directory project-root)))
        (when (and (file-directory-p audit-dir)
                   (yes-or-no-p (format "Delete all audit records for project %s? " project-root)))
          (ai-logging--verbose-message "Cleaning up all audit records for project %s" project-root)
          (delete-directory audit-dir t)
          (message "Audit records cleaned up for project: %s" project-root)))
    (message "Not in a project. Cannot cleanup audit records.")))

(provide 'ai-request-audit)

;;; ai-request-audit.el ends here
