;;; ai-debug.el --- Visual debug components for AI Mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib magit-section)
;; Keywords: help, tools, AI, debug

;; This file is part of GNU Emacs.

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
;; AI Debug provides comprehensive debugging and introspection capabilities
;; for AI Mode operations. This package offers visual debugging tools that
;; help users understand and troubleshoot AI context, prompts, and execution
;; flow within Emacs.
;;
;; Key Features:
;; - Visual context inspection with magit-style expandable sections
;; - Real-time AI context and prompt debugging
;; - Buffer and selection context analysis
;; - Model configuration and parameter display
;; - Context source tracking and visualization
;; - Project context with expandable file sections
;; - Performance-optimized content truncation
;; - Interactive debugging commands with keyboard shortcuts
;; - Buffer refresh capability for dynamic content updates
;;
;; The debugging interface provides detailed insights into:
;; - AI model configuration and parameters
;; - Buffer context and completion parameters
;; - Message structure and prompt composition
;; - Context sources including global prompts, memory, and buffer-bound data
;; - Project files with individual file sections
;; - Selection and cursor positioning information
;;
;; Usage:
;; Enable AI mode and use the following commands:
;; - `ai-debug-visual': Main debug interface showing current context
;; - `ai-debug-show-sources': Display all available context sources
;; - `ai-debug-completion-limited-context': Debug completion with limited context
;; - `ai-debug-completion-full-context': Debug completion with full buffer context
;;
;; Within debug buffers:
;; - C-c t: Toggle content truncation for performance optimization
;; - C-r: Refresh buffer content with current AI context
;;
;; The interface supports content truncation for performance, which can be
;; toggled with 'C-c t' within debug buffers. All debug information is
;; presented in collapsible sections for easy navigation and inspection.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'ai-common)
(require 'ai-utils)

(defgroup ai-debug nil
  "Debug and introspection tools for AI Mode."
  :prefix "ai-debug-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defvar ai-debug-buffer-name "*AI Debug Context*"
  "Buffer name for displaying AI debug context.")

(defvar ai-debug-sources-buffer-name "*AI Context Sources*"
  "Buffer name for displaying AI context sources.")

(defcustom ai-debug-truncate-content t
  "Whether to truncate long content in debug buffers for performance.
When non-nil, content is truncated to improve display performance.
When nil, full content is shown which may cause performance issues."
  :type 'boolean
  :group 'ai-debug)

(defcustom ai-debug-max-content-length 2000
  "Maximum length of content to display when truncation is enabled."
  :type 'integer
  :group 'ai-debug)

(defcustom ai-debug-max-recursion-depth 2
  "Maximum recursion depth for nested content display."
  :type 'integer
  :group 'ai-debug)

(defcustom ai-debug-max-list-items 100
  "Maximum number of list items to display."
  :type 'integer
  :group 'ai-debug)

(defcustom ai-debug-max-plist-lines 40
  "Maximum number of plist lines to display."
  :type 'integer
  :group 'ai-debug)

(defface ai-debug-context-header
  '((t :inherit magit-section-heading :foreground "brown"))
  "Face for top-level context section headers."
  :group 'ai-debug)

(defface ai-debug-category-header
  '((t :inherit magit-section-heading :foreground "brown" :weight bold))
  "Face for main category headers."
  :group 'ai-debug)

(defface ai-debug-context-type
  '((t :inherit magit-section-secondary-heading))
  "Face for context type labels."
  :group 'ai-debug)

(defface ai-debug-context-metadata
  '((t :inherit magit-dimmed))
  "Face for context metadata."
  :group 'ai-debug)

(defface ai-debug-empty-source
  '((t :inherit magit-dimmed :slant italic))
  "Face for empty context sources."
  :group 'ai-debug)

;; Buffer-local variables for refresh functionality
(defvar-local ai-debug--refresh-function nil
  "Function to call when refreshing the current debug buffer.")

(defvar-local ai-debug--refresh-args nil
  "Arguments to pass to the refresh function.")

(defun ai-debug--create-debug-buffer ()
  "Create and return the AI debug buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI Context Debug - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
                                       (if ai-debug-truncate-content "ON" "OFF")))
      ;; Add local keymap for debug buffer operations
      (local-set-key (kbd "C-c t") 'ai-debug-toggle-truncation)
      (local-set-key (kbd "C-r") 'ai-debug-refresh-buffer))
    buffer))

(defun ai-debug--create-sources-buffer ()
  "Create and return the AI context sources buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-sources-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI Context Sources - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
                                       (if ai-debug-truncate-content "ON" "OFF")))
      ;; Add local keymap for debug buffer operations
      (local-set-key (kbd "C-c t") 'ai-debug-toggle-truncation)
      (local-set-key (kbd "C-r") 'ai-debug-refresh-buffer))
    buffer))

(defun ai-debug-toggle-truncation ()
  "Toggle content truncation in AI debug buffers.
This affects how much content is displayed in the debug interface
to balance between completeness and performance."
  (interactive)
  (setq ai-debug-truncate-content (not ai-debug-truncate-content))
  (message "AI debug content truncation %s" (if ai-debug-truncate-content "enabled" "disabled"))
  ;; Refresh current buffer automatically after toggling truncation
  (ai-debug-refresh-buffer))

(defun ai-debug-refresh-buffer ()
  "Refresh the current debug buffer with updated AI context.
Calls the appropriate refresh function based on the current buffer type."
  (interactive)
  (cond
   ;; Context debug buffer
   ((string= (buffer-name) ai-debug-buffer-name)
    (message "Refreshing AI debug context...")
    (if ai-debug--refresh-function
        (apply ai-debug--refresh-function ai-debug--refresh-args)
      (ai-debug-show-context))
    (message "AI debug context refreshed"))

   ;; Sources debug buffer
   ((string= (buffer-name) ai-debug-sources-buffer-name)
    (message "Refreshing AI context sources...")
    (ai-debug-show-sources)
    (message "AI context sources refreshed"))

   ;; Unknown buffer
   (t
    (message "Cannot refresh: not in an AI debug buffer"))))

(defun ai-debug--format-context-metadata (context-item)
  "Format metadata for CONTEXT-ITEM as a readable string.
Extracts file, buffer, mode, position, and timestamp information
from the context item and formats them into a human-readable string."
  (condition-case-unless-debug nil
    (let ((metadata '()))
      (when-let ((file (plist-get context-item :file)))
        (push (format "file: %s" file) metadata))
      (when-let ((buffer (plist-get context-item :buffer)))
        (push (format "buffer: %s" buffer) metadata))
      (when-let ((mode (plist-get context-item :mode)))
        (push (format "mode: %s" mode) metadata))
      (when-let ((start-pos (plist-get context-item :start-pos)))
        (push (format "start: %d" start-pos) metadata))
      (when-let ((end-pos (plist-get context-item :end-pos)))
        (push (format "end: %d" end-pos) metadata))
      (when-let ((timestamp (plist-get context-item :timestamp)))
        (push (format "time: %s" timestamp) metadata))
      (when metadata
        (mapconcat #'identity (reverse metadata) " | ")))
    (error "metadata unavailable")))

(defun ai-debug--format-display-value (value &optional truncate-strings)
  "Format VALUE for display in debug buffer.
If TRUNCATE-STRINGS is non-nil, apply string truncation based on `ai-debug-max-content-length`."
  (cond
   ((stringp value)
    (if truncate-strings
        (let* ((max-len (if ai-debug-truncate-content 100 1000))
               (display-value (if (> (length value) max-len)
                                  (substring value 0 max-len)
                                value)))
          (format "\"%s%s\"" display-value (if (> (length value) max-len) "..." "")))
      (format "\"%s\"" value))) ; No truncation, just quote strings
   ((numberp value) (number-to-string value))
   ((booleanp value) (if value "t" "nil"))
   ;; Handle quoted symbols, e.g., 'function-name
   ((and (listp value) (eq (car value) 'quote) (symbolp (cadr value)))
    (symbol-name (cadr value)))
   ;; Handle keywords: remove leading colon for display
   ((keywordp value)
    (substring (symbol-name value) 1))
   ;; Handle other symbols
   ((symbolp value) (symbol-name value))
   ;; Handle lists/vectors (e.g., role-mapping) - show count
   ((listp value) (format "(%d items)" (length value)))
   ((vectorp value) (format "[%d items]" (length value)))
   ;; Fallback for other complex types (e.g. hash-tables, objects)
   (t (prin1-to-string value))))

(defun ai-debug--format-plist-as-table (plist &optional prefix)
  "Format PLIST as a readable table with optional PREFIX for keys.
Converts property list into human-readable key-value pairs with
proper formatting and truncation based on debug settings."
  (condition-case-unless-debug nil
    (let ((result '())
          (prefix-str (or prefix ""))
          (count 0)
          (max-iterations (if ai-debug-truncate-content 50 1000)))
      (while (and plist (< count max-iterations))
        (let ((key (car plist))
              (value (cadr plist)))
          (when (keywordp key)
            (let ((key-str (concat prefix-str (substring (symbol-name key) 1)))
                  (value-str (ai-debug--format-display-value value t))) ; Use helper with truncation
              (push (format "%-20s: %s" key-str value-str) result)))
          (setq plist (cddr plist))
          (setq count (1+ count))))
      (reverse result))
    (error (list "Error formatting plist"))))

(defun ai-debug--count-non-empty-items (items)
  "Count non-empty items in ITEMS list.
Used for statistics in debug interface headers. Handles various
data types and structures safely with error protection."
  (condition-case-unless-debug nil
    (if (listp items)
        (length (cl-remove-if (lambda (item)
                                (condition-case-unless-debug nil
                                  (or (null item)
                                      (and (listp item)
                                           (keywordp (car item))
                                           (let ((content (plist-get item :content)))
                                             (or (null content)
                                                 (and (stringp content) (string-empty-p content))))))
                                  (error t)))  ; Treat errors as empty
                              items))
      (if (and items
               (not (and (stringp items) (string-empty-p items))))
          1 0))
    (error 0)))

(defun ai-debug--is-empty-source (source-data)
  "Check if SOURCE-DATA is empty.
Handles various data types including lists, strings, and plists.
Returns t if the source contains no meaningful data."
  (condition-case-unless-debug nil
    (cond
     ((null source-data) t)
     ((and (listp source-data) (= (length source-data) 0)) t)
     ((and (stringp source-data) (string-empty-p source-data)) t)
     ((and (listp source-data) (keywordp (car source-data)))
      (let ((content (plist-get source-data :content)))
        (or (null content)
            (and (stringp content) (string-empty-p content)))))
     (t nil))
    (error t)))  ; Treat errors as empty

(defun ai-debug--safe-insert-content (content max-depth current-depth)
  "Safely insert CONTENT with recursion protection.
MAX-DEPTH limits recursion depth, CURRENT-DEPTH tracks current level.
Prevents infinite recursion and handles various content types safely
with comprehensive error handling and performance optimizations."
  (condition-case-unless-debug err
    (progn
      (let ((effective-max-depth (if ai-debug-truncate-content
                                     (min max-depth ai-debug-max-recursion-depth)
                                   (max max-depth 10))))
        (when (> current-depth effective-max-depth)
          (insert (propertize "  [Content too deeply nested - truncated]\n" 'face 'ai-debug-empty-source))
          (cl-return-from ai-debug--safe-insert-content)))

      (cond
       ;; Handle string content with length limits
       ((stringp content)
        (let* ((max-len (if ai-debug-truncate-content ai-debug-max-content-length 50000))
               (truncated-content (if (and ai-debug-truncate-content (> (length content) max-len))
                                      (format "%s\n[Content truncated - %d chars total]"
                                             (substring content 0 max-len)
                                             (length content))
                                    content)))
          (insert (propertize truncated-content 'face 'default))
          (insert "\n")))

       ;; Handle plist (single property list)
       ((and (listp content) (keywordp (car content)))
        (let* ((formatted-lines (ai-debug--format-plist-as-table content "  "))
               (max-lines (if ai-debug-truncate-content ai-debug-max-plist-lines 1000)))
          (dolist (line (cl-subseq formatted-lines 0 (min (length formatted-lines) max-lines)))
            (insert (propertize line 'face 'default))
            (insert "\n"))
          (when (and ai-debug-truncate-content (> (length formatted-lines) max-lines))
            (insert (propertize (format "[%d more lines not shown - toggle truncation with C-c t]\n"
                                       (- (length formatted-lines) max-lines))
                               'face 'ai-debug-context-metadata)))))

       ;; Handle list of items with item limits
       ((listp content)
        (let* ((max-items (if ai-debug-truncate-content ai-debug-max-list-items 100))
               (item-count (min (length content) max-items)))
          (when (> item-count 0)
            (when ai-debug-truncate-content
              (insert (propertize (format "  [showing %d of %d items]\n" item-count (length content)) 'face 'ai-debug-context-metadata)))
            (dotimes (i item-count)
              (let ((item (nth i content)))
                (insert (propertize (format "  Item %d: " (1+ i)) 'face 'ai-debug-context-metadata))
                (ai-debug--safe-insert-content item max-depth (1+ current-depth))))
            (when (and ai-debug-truncate-content (> (length content) item-count))
              (insert (propertize (format "[%d more items not shown - toggle truncation with C-c t]\n"
                                         (- (length content) item-count))
                                 'face 'ai-debug-context-metadata))))))

       ;; Handle other data types
       (t
        (insert (propertize (format "  %s\n" content) 'face 'default)))))
    (error
     (insert (propertize (format "  [Error displaying content: %s]\n" (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--insert-typed-struct (struct level &optional parent-section-name)
  "Insert a typed structure STRUCT at indentation LEVEL.
PARENT-SECTION-NAME provides context for nested structures.
Includes protection against infinite recursion and handles errors gracefully.
Creates expandable sections for complex data structures."
  (condition-case-unless-debug err
    (progn
      (let ((max-level (if ai-debug-truncate-content 3 10)))
        (when (> level max-level)
          (insert (propertize "  [Structure too deeply nested - truncated]\n" 'face 'ai-debug-empty-source))
          (cl-return-from ai-debug--insert-typed-struct)))

      (cond
       ;; Handle individual typed struct (plist with type information)
       ((and (listp struct) (keywordp (car struct)))
        (let* ((type (condition-case-unless-debug nil (plist-get struct :type) (error "unknown")))
               (content (condition-case-unless-debug nil (plist-get struct :content) (error nil)))
               (id (condition-case-unless-debug nil (plist-get struct :id) (error (format "id-%d" (random 10000)))))
               (source (condition-case-unless-debug nil (plist-get struct :source) (error nil)))
               (timestamp (condition-case-unless-debug nil (plist-get struct :timestamp) (error nil)))
               (file (condition-case-unless-debug nil (plist-get struct :file) (error nil)))
               ;; Use source as title if available and type is agent-instructions
               (section-title (if (and source (eq type 'agent-instructions))
                                  (format "%s" source)
                                (format "%s" (or type "unknown"))))
               (metadata-parts '()))

          ;; Build metadata parts safely
          (when type
            (push (format "type: %s" type) metadata-parts))
          (when source
            (push (format "source: %s" source) metadata-parts))
          ;; Show file path for file-context type
          (when (and file (eq type 'file-context))
            (push (format "file: %s" file) metadata-parts))
          (when timestamp
            (push (format "time: %s" timestamp) metadata-parts))

          (magit-insert-section (ai-typed-struct id)
            (magit-insert-heading
              (propertize section-title 'face 'ai-debug-context-type)
              (when metadata-parts
                (concat " " (propertize (format "(%s)" (mapconcat #'identity (reverse metadata-parts) " | "))
                                       'face 'ai-debug-context-metadata))))

            ;; Insert content with proper rendering and recursion protection
            (when content
              (let ((rendered-content (condition-case-unless-debug nil
                                        (ai-common--render-struct-to-string struct)
                                        (error content))))
                (ai-debug--safe-insert-content rendered-content ai-debug-max-recursion-depth 0))
              (insert "\n")))))

       ;; Handle list of typed structs with item limits
       ((and (listp struct) (> (length struct) 0))
        (let ((item-count (min (length struct) (if ai-debug-truncate-content ai-debug-max-list-items 50))))
          (dotimes (i item-count)
            (ai-debug--insert-typed-struct (nth i struct) level parent-section-name))
          (when (> (length struct) item-count)
            (insert (propertize (format "[%d more items not shown%s]\n\n"
                                       (- (length struct) item-count)
                                       (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                               'face 'ai-debug-empty-source)))))

       ;; Handle empty or invalid structures
       (t
        (insert (propertize "Invalid or empty structure" 'face 'ai-debug-empty-source))
        (insert "\n\n"))))
    (error
     (insert (propertize (format "[Error displaying structure: %s]\n\n" (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--insert-context-section (context-item level)
  "Insert a collapsible section for CONTEXT-ITEM at indentation LEVEL.
Creates a magit-style expandable section with metadata and content.
Provides comprehensive error handling and safe content insertion."
  (condition-case-unless-debug err
    (progn
      (let ((max-level (if ai-debug-truncate-content 3 10)))
        (when (> level max-level)
          (insert (propertize "  [Section too deeply nested - truncated]\n" 'face 'ai-debug-empty-source))
          (cl-return-from ai-debug--insert-context-section)))

      (let* ((type (condition-case-unless-debug nil (plist-get context-item :type) (error "unknown")))
             (content (condition-case-unless-debug nil (plist-get context-item :content) (error nil)))
             (id (condition-case-unless-debug nil (plist-get context-item :id) (error (format "id-%d" (random 10000)))))
             (source (condition-case-unless-debug nil (plist-get context-item :source) (error nil)))
             ;; Use source as title if available and type is agent-instructions
             (section-title (if (and source (eq type 'agent-instructions))
                                (format "%s" source)
                              (format "%s" (or type "unknown"))))
             (metadata (ai-debug--format-context-metadata context-item)))

        (magit-insert-section (ai-context-item id)
          (magit-insert-heading
            (propertize section-title 'face 'ai-debug-context-type)
            (when metadata
              (concat " " (propertize (format "(%s)" metadata) 'face 'ai-debug-context-metadata))))

          (when content
            (let ((rendered-content (condition-case-unless-debug nil
                                      (ai-common--render-struct-to-string context-item)
                                      (error content))))
              (ai-debug--safe-insert-content rendered-content ai-debug-max-recursion-depth 0))
            (insert "\n")))))
    (error
     (insert (propertize (format "[Error displaying section: %s]\n\n" (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--group-context-by-source (context-list)
  "Group CONTEXT-LIST by :source field preserving execution order.
Returns an association list of (source-name . items-list) pairs.
Handles large context lists efficiently with safety limits."
  (condition-case-unless-debug nil
    (let ((source-groups '())
          (seen-sources (make-hash-table :test 'equal))
          (count 0)
          (max-items (if ai-debug-truncate-content 50 500)))

      ;; Limit processing to avoid hangs
      (dolist (item (cl-subseq context-list 0 (min (length context-list) max-items)))
        (when (< count (* max-items 2))  ; Hard limit
          (let* ((source (condition-case-unless-debug nil
                           (or (plist-get item :source) 'unknown)
                           (error 'unknown)))
                 (source-key (condition-case-unless-debug nil
                               (if (symbolp source) (symbol-name source) (format "%s" source))
                               (error "unknown"))))

            ;; If we haven't seen this source before, create new group
            (unless (gethash source-key seen-sources)
              (puthash source-key t seen-sources)
              (push (cons source-key '()) source-groups))

            ;; Add item to the appropriate group
            (let ((group-entry (assoc source-key source-groups)))
              (when group-entry
                (setcdr group-entry (append (cdr group-entry) (list item))))))
          (setq count (1+ count))))

      ;; Return groups in reverse order (since we built them backwards)
      (reverse source-groups))
    (error '(("error" . nil)))))

(defun ai-debug--get-project-context-structs ()
  "Get project context as list of typed structs for debug display.
Returns filtered project files as individual structs without container wrapping.
Similar to `ai--get-project-context` but returns raw structs for debugging."
  (condition-case-unless-debug nil
    (when-let ((project-files (ai-common--get-filtered-project-files-as-structs)))
      project-files)
    (error nil)))

(defun ai-debug--deduplicate-project-files (project-files)
  "Remove duplicate project files from PROJECT-FILES based on absolute file paths.
Returns a list with unique absolute file paths only."
  (let ((seen-paths (make-hash-table :test 'equal))
        (unique-files '()))
    (dolist (file-struct project-files)
      (let ((file-path (condition-case-unless-debug nil
                         (plist-get file-struct :file)
                         (error nil))))
        (when file-path
          ;; Ensure we use absolute path
          (let ((absolute-path (expand-file-name file-path)))
            (unless (gethash absolute-path seen-paths)
              (puthash absolute-path t seen-paths)
              ;; Update the struct with absolute path
              (setf (plist-get file-struct :file) absolute-path)
              (push file-struct unique-files))))))
    (reverse unique-files)))

(defun ai-debug--insert-project-file-section (file-struct)
  "Insert a collapsible section for a single project FILE-STRUCT.
Creates an expandable section showing individual project file content
with filename in the header and absolute path in metadata."
  (condition-case-unless-debug err
    (let* ((file-path (condition-case-unless-debug nil (plist-get file-struct :file) (error "unknown")))
           (file-size (condition-case-unless-debug nil (plist-get file-struct :file-size) (error 0)))
           (timestamp (condition-case-unless-debug nil (plist-get file-struct :timestamp) (error nil)))
           (content (condition-case-unless-debug nil (plist-get file-struct :content) (error nil)))
           (file-name (if file-path (file-name-nondirectory file-path) "unknown"))
           ;; Use absolute path for consistency
           (absolute-path (if file-path (expand-file-name file-path) "unknown"))
           (id (condition-case-unless-debug nil (plist-get file-struct :id) (error (format "file-%d" (random 10000)))))
           (metadata-parts '()))

      ;; Build metadata parts with absolute path
      (when absolute-path
        (push (format "path: %s" absolute-path) metadata-parts))
      (when (and file-size (> file-size 0))
        (push (format "size: %d bytes" file-size) metadata-parts))
      (when timestamp
        (push (format "scanned: %s" timestamp) metadata-parts))

      (magit-insert-section (ai-project-file id)
        (magit-insert-heading
          (propertize file-name 'face 'ai-debug-context-type)
          (when metadata-parts
            (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                   'face 'ai-debug-context-metadata))))

        (when content
          (let ((rendered-content (condition-case-unless-debug nil
                                    (ai-common--render-struct-to-string file-struct)
                                    (error content))))
            (ai-debug--safe-insert-content rendered-content ai-debug-max-recursion-depth 0))
          (insert "\n"))))
    (error
     (insert (propertize (format "[Error displaying project file: %s]\n\n" (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--insert-source-section (source-name source-data description)
  "Insert a section for SOURCE-NAME with SOURCE-DATA and DESCRIPTION.
Creates a collapsible section showing context source information
with comprehensive statistics and safe content handling."
  (condition-case-unless-debug err
    (let* ((is-empty (ai-debug--is-empty-source source-data))
           (total-count (condition-case-unless-debug nil
                          (cond
                           ((null source-data) 0)
                           ((and (listp source-data) (keywordp (car source-data))) 1)
                           ((listp source-data) (min (length source-data) (if ai-debug-truncate-content 50 1000)))
                           (t 1))
                          (error 0)))
           (non-empty-count (ai-debug--count-non-empty-items source-data))
           (count-info (if (> total-count 0)
                           (format " [%d/%d items]" non-empty-count total-count)
                         " [empty]")))

      (magit-insert-section (ai-source-section source-name is-empty)
        (magit-insert-heading
          (propertize (format "%s%s" source-name count-info) 'face 'ai-debug-context-header)
          (when description
            (concat " " (propertize (format "- %s" description) 'face 'ai-debug-context-metadata))))

        (cond
         ;; Empty or nil data
         (is-empty
          (insert (propertize "  (empty)\n\n" 'face 'ai-debug-empty-source)))

         ;; Special handling for project context (list of file structs)
         ((and (string= source-name "Project Context") (listp source-data))
          (let* ((deduplicated-files (ai-debug--deduplicate-project-files source-data))
                 (file-count (min (length deduplicated-files) (if ai-debug-truncate-content 10 100))))
            (dotimes (i file-count)
              (ai-debug--insert-project-file-section (nth i deduplicated-files)))
            (when (> (length deduplicated-files) file-count)
              (insert (propertize (format "  [%d more files not shown%s]\n\n"
                                         (- (length deduplicated-files) file-count)
                                         (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                                 'face 'ai-debug-empty-source)))))

         ;; Single typed struct - render using ai-common--render-struct-to-string
         ((and (listp source-data) (keywordp (car source-data)))
          (let ((rendered-content (condition-case-unless-debug nil
                                    (ai-common--render-struct-to-string source-data)
                                    (error source-data))))
            (ai-debug--safe-insert-content rendered-content 1 0)
            (insert "\n")))

         ;; List of typed structs with safety limits - render each element
         ((and (listp source-data) (> (length source-data) 0))
          (let ((item-count (min (length source-data) (if ai-debug-truncate-content 3 20))))
            (dotimes (i item-count)
              (let* ((item (nth i source-data))
                     (rendered-item (condition-case-unless-debug nil
                                      (ai-common--render-struct-to-string item)
                                      (error item))))
                (ai-debug--safe-insert-content rendered-item 1 0)))
            (when (> (length source-data) item-count)
              (insert (propertize (format "  [%d more items not shown%s]\n\n"
                                         (- (length source-data) item-count)
                                         (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                                 'face 'ai-debug-empty-source)))))

         ;; Other data types
         (t
          (ai-debug--safe-insert-content source-data 1 0)
          (insert "\n")))))
    (error
     (insert (propertize (format "[Error displaying source %s: %s]\n\n" source-name (error-message-string err)) 'face 'ai-debug-empty-source)))))


(defun ai-debug--capitalize-category-name (name)
  "Capitalize and format category NAME for display.
Converts names like 'agent-instructions' to 'AGENT INSTRUCTIONS'."
  (let ((formatted-name (replace-regexp-in-string "-" " " (format "%s" name))))
    (upcase formatted-name)))


(defun ai-debug--insert-project-context-subsection (content subsection-name)
  "Insert a subsection for project context CONTENT with SUBSECTION-NAME."
  (magit-insert-section (ai-project-subsection subsection-name)
    (magit-insert-heading
      (propertize subsection-name 'face 'ai-debug-context-type))

    ;; Handle the content based on its type
    (cond
     ;; Files list content
     ((and (listp content) (keywordp (car content)) (eq (plist-get content :type) 'files-list))
      (let ((files-content (plist-get content :content))
            (files-count (plist-get content :count)))
        (when files-content
          (ai-debug--safe-insert-content
           (format "Project files (%d total):\n%s" (or files-count 0) files-content)
           ai-debug-max-recursion-depth 0)
          (insert "\n"))))

     ;; Files content (list of file structures)
     ((and (listp content) (keywordp (car content)) (eq (plist-get content :type) 'files))
      (let ((files-data (plist-get content :content)))
        (when (and files-data (listp files-data))
          (let* ((deduplicated-files (ai-debug--deduplicate-project-files files-data))
                 (total-files (length deduplicated-files))
                 (file-count (min total-files (if ai-debug-truncate-content 5 50))))
            (insert (propertize (format "Project file contents (%d files):\n" total-files) 'face 'ai-debug-context-metadata))
            (dotimes (i file-count)
              (ai-debug--insert-project-file-section (nth i deduplicated-files)))
            (when (> total-files file-count)
              (insert (propertize (format "  [%d more files not shown%s]\n\n"
                                         (- total-files file-count)
                                         (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                                 'face 'ai-debug-empty-source)))))))

     ;; Handle other or unexpected content types gracefully
     (t
      (insert (propertize (format "  [Unhandled project context subsection content type: %s]\n"
                                 (type-of content))
                         'face 'ai-debug-empty-source))
      (when content
        (ai-debug--safe-insert-content content ai-debug-max-recursion-depth 0))))

    (insert "\n")))

(defun ai-debug--insert-ai-mode-settings ()
  "Insert AI Mode settings section with current configuration state."
  (magit-insert-section (ai-settings)
    (magit-insert-heading
      (propertize "AI Mode Settings" 'face 'ai-debug-category-header))

    ;; Core AI Mode Settings
    (let ((settings `(("Extended Instructions" . ,(if (boundp 'ai--extended-instructions-enabled)
                                                      ai--extended-instructions-enabled
                                                      "unbound"))
                      ("Global Prompts" . ,(if (boundp 'ai--global-prompts-enabled)
                                              ai--global-prompts-enabled
                                              "unbound"))
                      ("Project File Instructions" . ,(if (boundp 'ai--project-file-instructions-enabled)
                                                         ai--project-file-instructions-enabled
                                                         "unbound"))
                      ("Current Buffer Context" . ,(if (boundp 'ai--current-buffer-additional-context)
                                                      ai--current-buffer-additional-context
                                                      "unbound"))
                      ("Project Context Mode" . ,(if (boundp 'ai--project-context-mode)
                                                    ai--project-context-mode
                                                    "unbound"))
                      ("User Input Method" . ,(if (boundp 'ai--user-input-method)
                                                 ai--user-input-method
                                                 "unbound"))
                      ("Progress Indicator" . ,(if (boundp 'ai--progress-indicator-enabled)
                                                  ai--progress-indicator-enabled
                                                  "unbound"))
                      ("Progress Style" . ,(if (boundp 'ai--progress-indicator-style)
                                              ai--progress-indicator-style
                                              "unbound"))
                      ("Preceding Context Size" . ,(if (boundp 'ai--current-precending-context-size)
                                                      ai--current-precending-context-size
                                                      "unbound"))
                      ("Following Context Size" . ,(if (boundp 'ai--current-forwarding-context-size)
                                                      ai--current-forwarding-context-size
                                                      "unbound")))))

      (dolist (setting settings)
        (let ((name (car setting))
              (value (cdr setting)))
          (insert (format "%-25s: %s\n" name
                         (ai-debug--format-display-value value nil)))))) ; Use helper, no truncation for settings
    (insert "\n")))

(defun ai-debug--insert-model-configuration (context)
  "Insert detailed model configuration section with key parameters highlighted.
Uses model information from CONTEXT's :model-context key if available."
  (magit-insert-section (ai-model-config)
    (magit-insert-heading
      (propertize "Model Configuration" 'face 'ai-debug-category-header))

    (let ((model (condition-case-unless-debug nil
                   (plist-get context :model-context)
                   (error nil))))
      (if model
          (progn
            ;; Key parameters section - display important parameters separately
            (magit-insert-section (ai-model-key-params)
              (magit-insert-heading
                (propertize "Key Parameters" 'face 'ai-debug-context-type))

              (let ((key-params '((:name . "Model Name")
                                  (:provider . "Provider")
                                  (:version . "Model Version")
                                  (:max-tokens . "Max Tokens")
                                  (:temperature . "Temperature")
                                  (:num-choices . "Number of Choices")
                                  (:execution-backend . "Execution Backend")
                                  (:setup-function . "Setup Function"))))
                ;; Use cl-loop for clearer iteration over key-label pairs,
                ;; which can sometimes resolve subtle variable binding issues.
                (cl-loop for (param-key . label) in key-params
                         do
                         (let ((value (map-elt model param-key)))
                           (when value
                             (insert (format "%-20s: %s\n" label (ai-debug--format-display-value value nil)))))))
              (insert "\n"))

            ;; REST parameters section - show additional provider-specific parameters
            (when-let ((rest-params (map-elt model :rest-params)))
              (magit-insert-section (ai-model-rest-params)
                (magit-insert-heading
                  (propertize "Provider-specific Parameters" 'face 'ai-debug-context-type))

                ;; Correctly iterate over plist key-value pairs
                (if (plistp rest-params)
                    (let ((temp-rest-params rest-params))
                      (while temp-rest-params
                        (let ((key (pop temp-rest-params))
                              (value (pop temp-rest-params)))
                          (insert (format "%-20s: %s\n"
                                          (ai-debug--format-display-value key nil) ; Use helper for key
                                          (ai-debug--format-display-value value nil)))))) ; Use helper for value
                  ;; Fallback if rest-params is not a plist but exists
                  (insert (format "%-20s: %s\n" "rest-params" (ai-debug--format-display-value rest-params nil)))) ; Use helper
                (insert "\n")))

            ;; Role mapping section
            (when-let ((role-mapping (map-elt model :role-mapping)))
              (magit-insert-section (ai-model-role-mapping)
                (magit-insert-heading
                  (propertize "Role Mapping" 'face 'ai-debug-context-type))

                (dolist (mapping role-mapping)
                  (let ((from (car mapping))
                        (to (cdr mapping)))
                    (insert (format "%-15s -> %s\n"
                                    (ai-debug--format-display-value from nil) ; Use helper for 'from'
                                    (ai-debug--format-display-value to nil))))) ; Use helper for 'to'
                (insert "\n")))

            ;; Complete configuration section (collapsed by default)
            (magit-insert-section (ai-model-full-config)
              (magit-insert-heading
                (propertize "Complete Configuration (Raw)" 'face 'ai-debug-context-type))

              (let ((model-details (ai-debug--format-plist-as-table (copy-sequence model))))
                (dolist (detail model-details)
                  (insert (format "%s\n" detail))))
              (insert "\n")))

        (insert (propertize "No model configuration found in context\n" 'face 'ai-debug-empty-source))))
    (insert "\n")))

(defun ai-debug--insert-messages-by-category (messages)
  "Insert MESSAGES grouped by type/category in expandable sections.
Creates organized categories for better navigation and understanding."
  (let* ((category-groups (make-hash-table :test 'equal))
         (category-order '()))

    ;; Group messages by their type
    (dolist (message messages)
      (let* ((type (condition-case-unless-debug nil
                       (plist-get message :type)
                     (error 'unknown)))
             (category-key (format "%s" type)))

        ;; Track order of categories
        (unless (gethash category-key category-groups)
          (push category-key category-order))

        ;; Add message to category
        (let ((existing (gethash category-key category-groups)))
          (puthash category-key (append existing (list message)) category-groups))))

    ;; Insert each category with file counts in headers
    (dolist (category (reverse category-order))
      (let* ((items (gethash category category-groups))
             (category-display-name (ai-debug--capitalize-category-name category))
             (item-count (length items))
             ;; Count files if this is project-context category
             (file-count (when (string= category "project-context")
                           (condition-case-unless-debug nil
                               (let ((total-files 0))
                                 (dolist (item items)
                                   (let ((content (plist-get item :content)))
                                     (when (and content (listp content))
                                       (dolist (sub-element content)
                                         (when (and (listp sub-element)
                                                    (keywordp (car sub-element))
                                                    (eq (plist-get sub-element :type) 'files))
                                           (let ((files-data (plist-get sub-element :content)))
                                             (when (and files-data (listp files-data))
                                               (setq total-files (+ total-files (length files-data)))))))))
                                   total-files)
                                 (error 0)))))
             ;; Format header with counts
             (header-text (if file-count
                              (format "%s (%d items, %d files)" category-display-name item-count file-count)
                            (format "%s (%d items)" category-display-name item-count))))

        (magit-insert-section (ai-message-category category)
          (magit-insert-heading
            (propertize header-text 'face 'ai-debug-category-header))

          ;; Special handling for project-context type
          (if (string= category "project-context")
              ;; For project-context, decompose and show subsections
              (dolist (item items)
                (let ((content (plist-get item :content)))
                  (when (and content (listp content))
                    (dolist (sub-element content)
                      (when (and (listp sub-element) (keywordp (car sub-element)))
                        (let* ((sub-type (plist-get sub-element :type))
                               (subsection-name (ai-debug--capitalize-category-name (symbol-name sub-type))))
                          (ai-debug--insert-project-context-subsection sub-element subsection-name)))))))

            ;; Normal handling for other categories - with child sections collapsed by default
            (let ((max-items (if ai-debug-truncate-content 5 50)))
              (dotimes (i (min item-count max-items))
                (let ((item (nth i items)))
                  (ai-debug--insert-context-section item 2)))

              (when (and ai-debug-truncate-content (> item-count max-items))
                (insert (propertize
                         (format "  [%d more items not shown - toggle truncation with C-c t]\n\n"
                                 (- item-count max-items))
                         'face 'ai-debug-context-metadata))))))))))

(defun ai-debug-show-context-debug (context)
    "Display CONTEXT in a visual magit-like interface.
Creates a comprehensive debug view showing all AI context information
including model configuration, buffer context, and messages.
The interface provides expandable sections for detailed inspection."
    (interactive)
    (condition-case-unless-debug err
        (let ((buffer (ai-debug--create-debug-buffer))
              (messages (condition-case-unless-debug nil (plist-get context :messages) (error nil))))

          (with-current-buffer buffer
            (setq buffer-read-only nil)

            ;; Store refresh function and args for later use
            (setq-local ai-debug--refresh-function 'ai-debug-show-context-debug)
            (setq-local ai-debug--refresh-args (list context))

            ;; Insert main header
            (magit-insert-section (ai-debug-root)
              (magit-insert-heading
                (propertize "AI Context Debug" 'face 'ai-debug-context-header))

              ;; AI Mode Settings Category (new top-level section)
              (ai-debug--insert-ai-mode-settings)

              ;; Model Configuration Category (enhanced to use context)
              (ai-debug--insert-model-configuration context)

              ;; Buffer Context Category
              (magit-insert-section (ai-buffer-info)
                (magit-insert-heading
                  (propertize "Buffer Context" 'face 'ai-debug-category-header))
                (let ((buffer-keys '(:buffer-name :buffer-language :buffer :file-path :project-root :file-name)))
                  (dolist (key buffer-keys)
                    (when-let ((value (condition-case-unless-debug nil (plist-get context key) (error nil))))
                      (let ((key-str (substring (symbol-name key) 1))
                            (value-str (ai-debug--format-display-value value nil))) ; Use helper
                        (insert (format "%-15s: %s\n" key-str value-str))))))
                (insert "\n"))

              ;; Completion Context Category
              (magit-insert-section (ai-completion-info)
                (magit-insert-heading
                  (propertize "Completion Context" 'face 'ai-debug-category-header))
                (let ((completion-keys '(:cursor-point :cursor-offset :region-content
                                                       :preceding-context-beginning :preceding-context-end
                                                       :preceding-context-content :preceding-context-size
                                                       :following-context-beginning :following-context-end
                                                       :following-context-content :following-context-size
                                                       :cursor-line-number :cursor-column-number)))
                  (dolist (key completion-keys)
                    (when-let ((value (condition-case-unless-debug nil (plist-get context key) (error nil))))
                      (let ((key-str (substring (symbol-name key) 1))
                            (value-str (ai-debug--format-display-value value t))) ; Use helper with truncation
                        (insert (format "%-25s: %s\n" key-str value-str))))))
                (insert "\n"))

              ;; Messages Category (organized by type)
              (when messages
                (magit-insert-section (ai-messages-root)
                  (magit-insert-heading
                    (propertize (format "AI Messages (%d total)" (length messages))
                                'face 'ai-debug-category-header))

                  (ai-debug--insert-messages-by-category messages))))

            (setq buffer-read-only t)
            (goto-char (point-min))
            ;; Show only level-1 sections, keep child sections collapsed by default
            (condition-case-unless-debug nil
                (progn
                  (magit-section-show-level-1-all)
                  ;; Hide child sections within categories to ensure they start collapsed
                  (save-excursion
                    (goto-char (point-min))
                    (let ((section (magit-current-section)))
                      (when section
                        (magit-section-children-map
                         (lambda (child)
                           (when (memq (oref child type) '(ai-message-category ai-project-subsection ai-context-item ai-typed-struct ai-project-file ai-model-key-params ai-model-rest-params ai-model-role-mapping ai-model-full-config))
                             (magit-section-hide child)))
                         section t)))))
              (error nil)))

          (pop-to-buffer buffer))
      (error
       (message "Error in ai-debug-show-context-debug: %s" (error-message-string err)))))

(defun ai-debug-show-sources ()
  "Display all AI context sources in a visual magit-like interface.
Shows global prompts, memory, buffer-bound prompts, context pool,
project context, and current buffer/selection information in expandable sections."
  (interactive)
  (condition-case-unless-debug err
    (let ((buffer (ai-debug--create-sources-buffer)))
      (with-current-buffer buffer
        (setq buffer-read-only nil)

        ;; Set up refresh capability for sources buffer
        (setq-local ai-debug--refresh-function 'ai-debug-show-sources)
        (setq-local ai-debug--refresh-args nil)

        ;; Safely collect sources with comprehensive error handling
        (let* ((global-system-prompts (condition-case-unless-debug nil
                                          (ai-common--get-global-system-prompts)
                                        (error nil)))
               (global-memory (condition-case-unless-debug nil
                                  (ai-common--get-global-memory)
                                (error nil)))
               (buffer-bound-prompts (condition-case-unless-debug nil
                                         (ai-common--get-buffer-bound-prompts)
                                       (error nil)))
               (context-pool (condition-case-unless-debug nil
                                 (ai-common--get-context-pool)
                               (error nil)))
               (project-context (condition-case-unless-debug nil
                                    (ai-debug--get-project-context-structs)
                                  (error nil)))
               (current-buffer-context (condition-case-unless-debug nil
                                           (unless (use-region-p) (ai--get-current-buffer-context))
                                         (error nil)))
               (current-selection (condition-case-unless-debug nil
                                      (when (use-region-p) (ai-common--make-snippet-from-region 'selection))
                                    (error nil)))

               ;; Count sources safely
               (all-sources (list global-system-prompts global-memory buffer-bound-prompts
                                 context-pool project-context current-buffer-context current-selection))
               (total-sources (length (cl-remove-if #'null all-sources)))
               (non-empty-sources (length (cl-remove-if #'ai-debug--is-empty-source all-sources)))
               (project-files-count (if (and project-context (listp project-context))
                                        (length project-context)
                                      0)))

          ;; Insert main header with counts
          (magit-insert-section (ai-sources-root)
            (magit-insert-heading
              (propertize (format "AI Context Sources [%d/%d active]"
                                 non-empty-sources total-sources)
                         'face 'ai-debug-category-header))

            ;; Insert each source section with error protection
            (ai-debug--insert-source-section
             "Global System Prompts"
             global-system-prompts
             "System-wide instructions that define AI behavior")

            (ai-debug--insert-source-section
             "Global Memory"
             global-memory
             "Persistent context that spans across all buffers")

            (ai-debug--insert-source-section
             "Buffer-bound Prompts"
             buffer-bound-prompts
             "Instructions specific to current buffer")

            (ai-debug--insert-source-section
             "Context Pool"
             context-pool
             "Temporary context for current interaction session")

            (ai-debug--insert-source-section
             (format "Project Context (%d files)" project-files-count)
             project-context
             "Filtered project files and structure")

            (when current-buffer-context
              (ai-debug--insert-source-section
               "Current Buffer Content"
               current-buffer-context
               "Full content of current buffer"))

            (when current-selection
              (ai-debug--insert-source-section
               "Current Selection"
               current-selection
               "Currently selected text region"))))

        (setq buffer-read-only t)
        (goto-char (point-min))
        ;; Safely expand sections - show only top-level sources, hide their children
        (condition-case-unless-debug nil
          (progn
            (magit-section-show-level-1-all)
            ;; Hide child sections to ensure they start collapsed
            (save-excursion
              (goto-char (point-min))
              (let ((section (magit-current-section)))
                (when section
                  (magit-section-children-map
                   (lambda (child)
                     (when (memq (oref child type) '(ai-project-file ai-context-item ai-typed-struct))
                       (magit-section-hide child)))
                   section t)))))
          (error nil))
        ;; Hide empty sections with comprehensive error protection
        (condition-case-unless-debug nil
            (save-excursion
              (goto-char (point-min))
              (let ((count 0)
                    (max-iterations (if ai-debug-truncate-content 100 1000)))
                (while (and (< count max-iterations) (re-search-forward "^" nil t))
                  (setq count (1+ count))
                  (when-let ((section (condition-case-unless-debug nil (magit-current-section) (error nil))))
                    (when (and (condition-case-unless-debug nil (eq (oref section type) 'ai-source-section) (error nil))
                               (condition-case-unless-debug nil (oref section value) (error nil)))  ; value is is-empty flag
                      (condition-case-unless-debug nil (magit-section-hide section) (error nil)))))))
          (error nil)))  ; Ignore errors in section hiding

      (pop-to-buffer buffer))
    (error
     (message "Error in ai-debug-show-sources: %s" (error-message-string err)))))

(defun ai-debug-show-context ()
  "Show debug context for the current AI operation.
Interactive command that displays the current AI context in a debug buffer.
Provides comprehensive information about model configuration, context,
and message structure for troubleshooting AI operations."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((query-type (condition-case-unless-debug nil
                           (or (when (fboundp 'ai--get-command-unrestricted)
                                 (ai--get-command-unrestricted))
                               "explain")
                           (error "explain")))
             (context (condition-case-unless-debug nil
                          (when (fboundp 'ai--get-executions-context-for-command)
                            (ai--get-executions-context-for-command
                             query-type
                             :model (when (fboundp 'ai--get-current-model)
                                      (ai--get-current-model))))

                        (error nil))))
        (if context
            (ai-debug-show-context-debug context)
          (message "Could not retrieve AI context"))))
    (error
     (message "Error in ai-debug-show-context: %s" (error-message-string err)))))

(defun ai-debug-completion-limited-context ()
  "Debug context for completion with limited context.
Shows completion context with restricted preceding and following context sizes.
Useful for debugging completion performance and context truncation issues."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((context (condition-case-unless-debug nil

                          (when (and (fboundp 'ai--get-execution-context)
                                     (fboundp 'ai--get-command-config-by-type)
                                     (fboundp 'ai--get-current-model))
                            (ai--get-execution-context
                             (current-buffer)
                             (ai--get-command-config-by-type "complete")
                             "complete"
                             :preceding-context-size (if (boundp 'ai-completions--current-precending-context-size)
                                                         ai-completions--current-precending-context-size
                                                       20)
                             :following-context-size (if (boundp 'ai-completions--current-forwarding-context-size)
                                                        ai-completions--current-forwarding-context-size
                                                      20)
                             :model (ai--get-current-model)))
                        (error nil))))
        (if context
            (ai-debug-show-context-debug context)
          (message "Could not retrieve completion context"))))
    (error
     (message "Error in ai-debug-completion-limited-context: %s" (error-message-string err)))))

(defun ai-debug-completion-full-context ()
  "Debug context for completion with full file context.
Shows completion context including the entire buffer content.
Useful for debugging issues with full-buffer completion strategies."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((context (condition-case-unless-debug nil
                          (when (and (fboundp 'ai--get-execution-context)
                                     (fboundp 'ai--get-command-config-by-type)
                                     (fboundp 'ai--get-current-model))
                            (ai--get-execution-context
                             (current-buffer)
                             (ai--get-command-config-by-type "complete")
                             "complete"
                             :preceding-context-size nil
                             :following-context-size nil
                             :model (ai--get-current-model)))
                        (error nil))))
        (if context
            (ai-debug-show-context-debug context)
          (message "Could not retrieve full completion context"))))
    (error
     (message "Error in ai-debug-completion-full-context: %s" (error-message-string err)))))

(defun ai-debug-visual ()
  "Enhanced debug function with visual interface.
Main entry point for AI context debugging with visual representation.
Provides a comprehensive view of AI context, configuration, and data flow."
  (interactive)
  (ai-debug-show-context))

;; Add debug commands to ai-command-map when ai-mode is loaded
(with-eval-after-load 'ai-mode
  (define-key ai-command-map (kbd "d") 'ai-debug-visual)
  (define-key ai-command-map (kbd "D") 'ai-debug-show-sources)
  (define-key ai-command-map (kbd "C-d l") 'ai-debug-completion-limited-context)
  (define-key ai-command-map (kbd "C-d f") 'ai-debug-completion-full-context))

(provide 'ai-debug)

;;; ai-debug.el ends here
