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
;; help users understand and troubleshoot AI context, prompts, execution
;; flow, and completion processes within Emacs.
;;
;; Key Features:
;; - Visual context inspection with magit-style expandable sections
;; - Real-time AI context and prompt debugging
;; - Buffer and selection context analysis
;; - AI completion session state debugging
;; - Model configuration and parameter display
;; - Context source tracking and visualization
;; - Project context with expandable file sections
;; - Raw typed structures view in sequential order
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
;; - Raw typed structures in their original sequence
;; - Selection and cursor positioning information
;; - AI completion session state and candidates
;; - Completion context and strategy information
;;
;; Usage:
;; Enable AI mode and use the following commands:
;; - `ai-debug-visual': Main debug interface showing current context
;; - `ai-debug-show-raw-structures': Display typed structures in original sequence
;; - `ai-debug-show-completions': Display AI completion session state
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
(require 'ai-project)
(require 'ai-utils)
(require 'ai-context-management)
(require 'ai-command-management)
(require 'ai-prompt-management)
(require 'ai-model-management)
(require 'ai-mode-adapter-api)
(require 'ai-structs)
(require 'ai-completions)

(defgroup ai-debug nil
  "Debug and introspection tools for AI Mode."
  :prefix "ai-debug-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defvar ai-debug-buffer-name "*AI Debug Context*"
  "Buffer name for displaying AI debug context.")

(defvar ai-debug-raw-structures-buffer-name "*AI Raw Structures*"
  "Buffer name for displaying AI typed structures in raw format.")

(defvar ai-debug-completions-buffer-name "*AI Completions Debug*"
  "Buffer name for displaying AI completions debug information.")

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

(defface ai-debug-command-location
  '((t :inherit magit-tag :foreground "cyan"))
  "Face for command location indicators."
  :group 'ai-debug)

(defface ai-debug-completion-active
  '((t :inherit success :weight bold))
  "Face for active completion session indicators."
  :group 'ai-debug)

(defface ai-debug-completion-inactive
  '((t :inherit shadow))
  "Face for inactive completion session indicators."
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

(defun ai-debug--create-raw-structures-buffer ()
  "Create and return the AI raw structures buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-raw-structures-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI Raw Structures - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
                                       (if ai-debug-truncate-content "ON" "OFF")))
      ;; Add local keymap for debug buffer operations
      (local-set-key (kbd "C-c t") 'ai-debug-toggle-truncation)
      (local-set-key (kbd "C-r") 'ai-debug-refresh-buffer))
    buffer))

(defun ai-debug--create-completions-buffer ()
  "Create and return the AI completions debug buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-completions-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI Completions Debug - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
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

   ;; Raw structures debug buffer
   ((string= (buffer-name) ai-debug-raw-structures-buffer-name)
    (message "Refreshing AI raw structures...")
    (if ai-debug--refresh-function
        (apply ai-debug--refresh-function ai-debug--refresh-args)
      (ai-debug-show-raw-structures))
    (message "AI raw structures refreshed"))

   ;; Completions debug buffer
   ((string= (buffer-name) ai-debug-completions-buffer-name)
    (message "Refreshing AI completions debug...")
    (ai-debug-show-completions)
    (message "AI completions debug refreshed"))

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
       ((and (listp struct) (not (null struct)) (keywordp (car struct)))
        (let* ((type (condition-case-unless-debug nil (plist-get struct :type) (error "unknown")))
               (content (condition-case-unless-debug nil (plist-get struct :content) (error nil)))
               (id (condition-case-unless-debug nil (plist-get struct :id) (error (format "id-%d" (random 10000)))))
               (source (condition-case-unless-debug nil (plist-get struct :source) (error nil)))
               (timestamp (condition-case-unless-debug nil (plist-get struct :timestamp) (error nil)))
               (file (condition-case-unless-debug nil (plist-get struct :file) (error nil)))
               (relative-path (condition-case-unless-debug nil (plist-get struct :relative-path) (error nil)))
               ;; Use source as title if available and type is agent-instructions
               (section-title (cond
                               ((eq type 'agent-instructions) (format "%s" (or source "Agent Instructions")))
                               ((eq type 'file-content) (format "%s" (or file relative-path "File Content"))) ; Prefer file (absolute path)
                               ((eq type 'file-summary) (format "%s" (or file relative-path "File Summary"))) ; Prefer file (absolute path)
                               (t (format "%s" (or type "unknown")))))
               (metadata-parts '()))

          ;; Build metadata parts safely
          (when type
            (push (format "type: %s" type) metadata-parts))
          (when source
            (push (format "source: %s" source) metadata-parts))
          ;; Show file path for file-context type (original file path)
          (when (and file (eq type 'file-context))
            (push (format "file: %s" file) metadata-parts))
          ;; For file-content and file-summary, prefer file in metadata, fall back to relative-path
          (when (and (or (eq type 'file-content) (eq type 'file-summary)) (or file relative-path))
            (push (format "path: %s" (or file relative-path)) metadata-parts))
          (when timestamp
            (push (format "time: %s" timestamp) metadata-parts))

          ;; Determine the section type based on the struct type
          (let ((section-type (cond
                               ((eq type 'file-content) 'ai-project-file)
                               ((eq type 'file-summary) 'ai-file-summary-item) ; New section type
                               (t 'ai-typed-struct)))) ; Generic type for others
            (magit-insert-section (section-type id t) ; Pass 't' as hidden flag
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
                (insert "\n"))))))

        ;; Handle list of typed structs with item limits
        ((and (listp struct) (not (null struct)))
         (let ((item-count (min (length struct) (if ai-debug-truncate-content 10 100))))
           (dotimes (i item-count)
             (ai-debug--insert-typed-struct (nth i struct) level (or parent-section-name "List Item"))) ; Pass item's type as parent-section-name?
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

(defun ai-debug--insert-raw-typed-struct (struct index)
  "Insert a single typed structure STRUCT at INDEX in raw sequential format.
Creates an expandable section with detailed type information and metadata."
  (condition-case-unless-debug err
    (let* ((type (condition-case-unless-debug nil (plist-get struct :type) (error "unknown")))
           (source (condition-case-unless-debug nil (plist-get struct :source) (error nil)))
           (id (condition-case-unless-debug nil (plist-get struct :id) (error (format "id-%d" (random 10000)))))
           (content (condition-case-unless-debug nil (plist-get struct :content) (error nil)))
           (file (condition-case-unless-debug nil (plist-get struct :file) (error nil)))
           (relative-path (condition-case-unless-debug nil (plist-get struct :relative-path) (error nil)))
           (timestamp (condition-case-unless-debug nil (plist-get struct :timestamp) (error nil)))
           (count (condition-case-unless-debug nil (plist-get struct :count) (error nil)))
           (root (condition-case-unless-debug nil (plist-get struct :root) (error nil)))
           (render-ignore-fields (condition-case-unless-debug nil (plist-get struct :render-ignore-fields) (error nil)))
           ;; Calculate TTL using adapter API
           (cache-ttl (condition-case-unless-debug nil
                        (when (fboundp 'ai-mode-adapter-api-get-cache-ttl)
                          (ai-mode-adapter-api-get-cache-ttl struct))
                        (error nil)))
           (metadata-parts '()))

      ;; Build comprehensive metadata
      (when type
        (push (format "type: %s" type) metadata-parts))
      (when source
        (push (format "source: %s" source) metadata-parts))
      (when id
        (push (format "id: %s" id) metadata-parts))
      (when (or file relative-path)
        (push (format "file: %s" (or file relative-path)) metadata-parts))
      (when timestamp
        (push (format "time: %s" timestamp) metadata-parts))
      (when count
        (push (format "count: %s" count) metadata-parts))
      (when root
        (push (format "root: %s" root) metadata-parts))
      (when render-ignore-fields
        (push (format "ignore: %s" render-ignore-fields) metadata-parts))
      (when cache-ttl
        (push (format "ttl: %s" cache-ttl) metadata-parts))

      ;; Create section with detailed header
      (magit-insert-section (ai-raw-struct (format "%d-%s" index id) t) ; Pass 't' as hidden flag
        (magit-insert-heading
          (propertize (format "[%d] %s" (1+ index) type) 'face 'ai-debug-context-type)
          (when metadata-parts
            (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                   'face 'ai-debug-context-metadata))))

        ;; Handle nested content structures
        (cond
         ;; If content is itself a list of typed structures
         ((and content (listp content) (not (null content))
               (listp (car content)) (keywordp (car (car content))))
          (insert (propertize (format "  Nested structures (%d items):\n" (length content)) 'face 'ai-debug-context-metadata))
          (let ((sub-index 0))
            (dolist (sub-struct content)
              (when (and (listp sub-struct) (keywordp (car sub-struct)))
                (ai-debug--insert-raw-typed-struct-nested sub-struct sub-index (1+ 1)))
              (setq sub-index (1+ sub-index)))))

         ;; Regular content
         (content
          (let ((rendered-content (condition-case-unless-debug nil
                                    (ai-common--render-struct-to-string struct)
                                    (error content))))
            (ai-debug--safe-insert-content rendered-content ai-debug-max-recursion-depth 0)))

         ;; No content
         (t
          (insert (propertize "  (no content)\n" 'face 'ai-debug-empty-source))))

        (insert "\n")))
    (error
     (insert (propertize (format "[Error displaying raw structure %d: %s]\n\n" index (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--insert-raw-typed-struct-nested (struct index level)
  "Insert nested typed structure STRUCT at INDEX with indentation LEVEL."
  (condition-case-unless-debug err
    (let* ((type (condition-case-unless-debug nil (plist-get struct :type) (error "unknown")))
           (source (condition-case-unless-debug nil (plist-get struct :source) (error nil)))
           (id (condition-case-unless-debug nil (plist-get struct :id) (error (format "nested-id-%d" (random 10000)))))
           (content (condition-case-unless-debug nil (plist-get struct :content) (error nil)))
           (indent (make-string (* level 2) ? ))
           (metadata-parts '()))

      ;; Build metadata for nested structure
      (when type
        (push (format "type: %s" type) metadata-parts))
      (when source
        (push (format "source: %s" source) metadata-parts))

      ;; Create nested section
      (magit-insert-section (ai-raw-nested-struct (format "nested-%d-%s" index id) t) ; Pass 't' as hidden flag
        (magit-insert-heading
          (concat indent
                  (propertize (format "[%d.%d] %s" level (1+ index) type) 'face 'ai-debug-context-type)
                  (when metadata-parts
                    (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                           'face 'ai-debug-context-metadata)))))

        ;; Insert nested content with additional indentation
        (when content
          (let ((rendered-content (condition-case-unless-debug nil
                                    (ai-common--render-struct-to-string struct)
                                    (error content))))
            ;; Indent the content
            (let ((content-lines (split-string (format "%s" rendered-content) "\n")))
              (dolist (line content-lines)
                (unless (string-empty-p line)
                  (insert (concat indent "  " (propertize line 'face 'default) "\n"))))))
          (insert "\n"))))
    (error
     (insert (propertize (format "%s[Error displaying nested structure %d: %s]\n"
                                (make-string (* level 2) ? ) index (error-message-string err))
                        'face 'ai-debug-empty-source)))))

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
Similar to `ai-context-management--get-project-context` but returns raw structs for debugging."
  (condition-case-unless-debug nil
    (when-let ((project-files (ai-project--get-filtered-project-files-as-structs)))
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

      (magit-insert-section (ai-project-file id t) ; Pass 't' as hidden flag
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

(defun ai-debug--insert-ignore-patterns-section ()
  "Insert file ignore patterns section with subsections for each source.
Shows patterns from global hardcoded, global files, project .gitignore, and project .ai-ignore,
followed by an aggregated result section."
  (condition-case-unless-debug err
      (let ((project-root (condition-case-unless-debug nil
                            (ai-project--get-project-root)
                            (error nil))))
        (magit-insert-section (ai-ignore-patterns nil t) ; Pass 't' as hidden flag
          (magit-insert-heading
            (propertize "File Ignore Patterns" 'face 'ai-debug-category-header))

          ;; Global hardcoded patterns subsection
          (let ((hardcoded-patterns (condition-case-unless-debug nil
                                      (ai-project--get-global-hardcoded-patterns)
                                      (error nil))))
            (magit-insert-section (ai-ignore-hardcoded nil t) ; Pass 't' as hidden flag
              (magit-insert-heading
                (propertize (format "Global Hardcoded Patterns (%d patterns)"
                                   (if hardcoded-patterns (length hardcoded-patterns) 0))
                           'face 'ai-debug-context-type))

              (if hardcoded-patterns
                  (dolist (pattern-cons hardcoded-patterns)
                    (let ((pattern (car pattern-cons))
                          (is-negated (cdr pattern-cons)))
                        (insert (format "  %s%s\n"
                                       (if is-negated "!" "")
                                       pattern))))
                (insert (propertize "  (no patterns)\n" 'face 'ai-debug-empty-source)))
              (insert "\n")))

          ;; Global ignore files patterns subsection
          (let ((global-file-patterns (condition-case-unless-debug nil
                                        (ai-project--get-global-ignore-file-patterns)
                                        (error nil))))
            (magit-insert-section (ai-ignore-global-files nil t) ; Pass 't' as hidden flag
              (magit-insert-heading
                (propertize (format "Global Ignore Files Patterns (%d patterns)"
                                   (if global-file-patterns (length global-file-patterns) 0))
                           'face 'ai-debug-context-type))

              (if global-file-patterns
                  (dolist (pattern-cons global-file-patterns)
                    (let ((pattern (car pattern-cons))
                          (is-negated (cdr pattern-cons)))
                        (insert (format "  %s%s\n"
                                       (if is-negated "!" "")
                                       pattern))))
                (insert (propertize "  (no patterns)\n" 'face 'ai-debug-empty-source)))
              (insert "\n")))

          ;; Project .gitignore patterns subsection
          (when project-root
            (let ((gitignore-patterns (condition-case-unless-debug nil
                                        (ai-project--get-project-gitignore-patterns project-root)
                                        (error nil))))
              (magit-insert-section (ai-ignore-gitignore nil t) ; Pass 't' as hidden flag
                (magit-insert-heading
                  (propertize (format "Project .gitignore Patterns (%d patterns)"
                                     (if gitignore-patterns (length gitignore-patterns) 0))
                             'face 'ai-debug-context-type))

                (if gitignore-patterns
                    (dolist (pattern-cons gitignore-patterns)
                      (let ((pattern (car pattern-cons))
                            (is-negated (cdr pattern-cons)))
                          (insert (format "  %s%s\n"
                                         (if is-negated "!" "")
                                         pattern))))
                  (insert (propertize "  (no patterns)\n" 'face 'ai-debug-empty-source)))
                (insert "\n"))))

          ;; Project .ai-ignore patterns subsection
          (when project-root
            (let ((ai-ignore-patterns (condition-case-unless-debug nil
                                        (ai-project--get-project-ai-ignore-patterns project-root)
                                        (error nil))))
              (magit-insert-section (ai-ignore-ai-ignore nil t) ; Pass 't' as hidden flag
                (magit-insert-heading
                  (propertize (format "Project .ai-ignore Patterns (%d patterns)"
                                     (if ai-ignore-patterns (length ai-ignore-patterns) 0))
                             'face 'ai-debug-context-type))

                (if ai-ignore-patterns
                    (dolist (pattern-cons ai-ignore-patterns)
                      (let ((pattern (car pattern-cons))
                            (is-negated (cdr pattern-cons)))
                          (insert (format "  %s%s\n"
                                         (if is-negated "!" "")
                                         pattern))))
                  (insert (propertize "  (no patterns)\n" 'face 'ai-debug-empty-source)))
                (insert "\n"))))

          ;; Aggregated patterns section
          (when project-root
            (let ((all-patterns (condition-case-unless-debug nil
                                  (ai-project--get-all-ignore-patterns project-root)
                                  (error nil))))
              (magit-insert-section (ai-ignore-aggregated nil t) ; Pass 't' as hidden flag
                (magit-insert-heading
                  (propertize (format "Aggregated All Patterns (%d patterns total)"
                                     (if all-patterns (length all-patterns) 0))
                             'face 'ai-debug-context-type))

                (if all-patterns
                    (progn
                      (insert (propertize "  Pattern processing order (later patterns can override earlier ones):\n"
                                         'face 'ai-debug-context-metadata))
                      (dolist (pattern-cons all-patterns)
                        (let ((pattern (car pattern-cons))
                              (is-negated (cdr pattern-cons)))
                          (insert (format "  %s%s\n"
                                         (if is-negated "!" "")
                                         pattern)))))
                  (insert (propertize "  (no patterns)\n" 'face 'ai-debug-empty-source)))
                (insert "\n"))))

          (insert "\n")))
    (error
     (insert (propertize (format "[Error displaying ignore patterns: %s]\n\n" (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--capitalize-category-name (name)
  "Capitalize and format category NAME for display.
Converts names like 'agent-instructions' to 'AGENT INSTRUCTIONS'."
  (let ((formatted-name (replace-regexp-in-string "-" " " (format "%s" name))))
    (upcase formatted-name)))


(defun ai-debug--insert-project-context-subsection (content subsection-name)
  "Insert a subsection for project context CONTENT with SUBSECTION-NAME."
  (magit-insert-section (ai-project-subsection subsection-name t) ; Pass 't' as hidden flag
    (cond
     ;; Files list content
     ((and (listp content) (keywordp (car content)) (eq (plist-get content :type) 'files-list))
      (let* ((files-content (plist-get content :content))
             (files-count (or (plist-get content :count) 0))
             (header-text (format "Project Files List (%d total)" files-count)))
        (magit-insert-heading
          (propertize header-text 'face 'ai-debug-context-type))
        (when files-content
          (ai-debug--safe-insert-content files-content ai-debug-max-recursion-depth 0))
        (insert "\n")))

     ;; Files content (list of file structures)
     ((and (listp content) (keywordp (car content)) (eq (plist-get content :type) 'files))
      (let* ((files-data (plist-get content :content))
             (deduplicated-files (if (and files-data (listp files-data))
                                     (ai-debug--deduplicate-project-files files-data)
                                   nil))
             (total-files (if deduplicated-files (length deduplicated-files) 0))
             (header-text (format "Project File Contents (%d files)" total-files))
             (file-count (min total-files (if ai-debug-truncate-content 5 50))))
        (magit-insert-heading
          (propertize header-text 'face 'ai-debug-context-type))
        (when (> total-files 0)
          (dotimes (i file-count)
            (ai-debug--insert-project-file-section (nth i deduplicated-files)))
          (when (> total-files file-count)
            (insert (propertize (format "  [%d more files not shown%s]\n\n"
                                         (- total-files file-count)
                                         (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                                'face 'ai-debug-empty-source))))
       (when (= total-files 0)
         (insert (propertize "  (no files)\n\n" 'face 'ai-debug-empty-source)))))

     ;; Handle other or unexpected content types gracefully
     (t
      (magit-insert-heading
        (propertize subsection-name 'face 'ai-debug-context-type))
      (insert (propertize (format "  [Unhandled project context subsection content type: %s]\n"
                                 (type-of content))
                         'face 'ai-debug-empty-source))
      (when content
        (ai-debug--safe-insert-content content ai-debug-max-recursion-depth 0))))

    (insert "\n")))

(defun ai-debug--insert-ai-mode-settings ()
  "Insert AI Mode settings section with current configuration state."
  (magit-insert-section (ai-settings nil t) ; Pass 't' as hidden flag
    (magit-insert-heading
      (propertize "AI Mode Settings" 'face 'ai-debug-category-header))

    ;; Core AI Mode Settings
    (let ((settings `(("Extended Instructions" . ,(if (boundp 'ai-context-management--extended-instructions-enabled)
                                                      ai-context-management--extended-instructions-enabled
                                                      "unbound"))
                      ("Current Buffer Context" . ,(if (boundp 'ai-context-management--current-buffer-additional-context)
                                                      ai-context-management--current-buffer-additional-context
                                                      "unbound"))
                      ("Project Context Mode" . ,(if (boundp 'ai-context-management--project-context-mode)
                                                    ai-context-management--project-context-mode
                                                    "unbound"))
                      ("User Input Method" . ,(if (boundp 'ai-context-management--user-input-method)
                                                 ai-context-management--user-input-method
                                                 "unbound"))
                      ("Preceding Context Size" . ,(if (boundp 'ai-context-management--current-precending-context-size)
                                                      ai-context-management--current-precending-context-size
                                                      "unbound"))
                      ("Following Context Size" . ,(if (boundp 'ai-context-management--current-forwarding-context-size)
                                                      ai-context-management--current-forwarding-context-size
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
  (magit-insert-section (ai-model-config nil t) ; Pass 't' as hidden flag
    (magit-insert-heading
      (propertize "Model Configuration" 'face 'ai-debug-category-header))

    (let ((model (condition-case-unless-debug nil
                   (plist-get context :model-context)
                   (error nil))))
      (if model
          (progn
            ;; Key parameters section - display important parameters separately
            (magit-insert-section (ai-model-key-params nil t) ; Pass 't' as hidden flag
              (magit-insert-heading
                (propertize "Key Parameters" 'face 'ai-debug-context-type))

              (let ((key-params '((:name . "Model Name")
                                  (:provider . "Provider")
                                  (:version . "Model Version")
                                  (:max-tokens . "Max Tokens")
                                  (:temperature . "Temperature")
                                  (:num-choices . "Number of Choices"))))
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
              (magit-insert-section (ai-model-rest-params nil t) ; Pass 't' as hidden flag
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
              (magit-insert-section (ai-model-role-mapping nil t) ; Pass 't' as hidden flag
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
            (magit-insert-section (ai-model-full-config nil t) ; Pass 't' as hidden flag
              (magit-insert-heading
                (propertize "Complete Configuration (Raw)" 'face 'ai-debug-context-type))

              (let ((model-details (ai-debug--format-plist-as-table (copy-sequence model))))
                (dolist (detail model-details)
                  (insert (format "%s\n" detail))))
              (insert "\n")))

        (insert (propertize "No model configuration found in context\n" 'face 'ai-debug-empty-source))))
    (insert "\n")))

(defun ai-debug--group-messages-by-type (messages)
  "Group MESSAGES by their :type field.
Returns a hash table where keys are type strings and values are lists of messages."
  (let ((category-groups (make-hash-table :test 'equal))
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

    (cl-values category-groups (reverse category-order))))

(defun ai-debug--calculate-category-file-count (category items)
  "Calculate file count for project-context CATEGORY from ITEMS.
Returns the total number of files found in project-context items, or nil for other categories."
  (when (string= category "project-context")
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
                        (setq total-files (+ total-files (length files-data))))))))))
          total-files)
      (error 0))))

(defun ai-debug--format-category-header (category item-count file-count)
  "Format header text for a message category.
CATEGORY is the category name, ITEM-COUNT is the number of items,
FILE-COUNT is the number of files (if applicable, nil otherwise)."
  (let ((category-display-name (ai-debug--capitalize-category-name category)))
    (if file-count
        (format "%s (%d items, %d files)" category-display-name item-count file-count)
      (format "%s (%d items)" category-display-name item-count))))

(defun ai-debug--insert-project-context-category (items)
  "Insert project-context category items with special subsection handling.
ITEMS should be a list of project-context type messages."
  (dolist (item items)
    (let ((content (plist-get item :content)))
      (when (and content (listp content))
        (dolist (sub-element content)
          (when (and (listp sub-element) (keywordp (car sub-element)))
            (let* ((sub-type (plist-get sub-element :type))
                   (subsection-name (ai-debug--capitalize-category-name (symbol-name sub-type))))
              (ai-debug--insert-project-context-subsection sub-element subsection-name))))))))

(defun ai-debug--insert-context-section (item level)
  "Insert a context section for ITEM at indentation LEVEL."
  (ai-debug--insert-typed-struct item level))

(defun ai-debug--insert-regular-category (items item-count)
  "Insert regular category items (non-project-context) with truncation handling.
ITEMS is the list of category items, ITEM-COUNT is the total number of items."
  (let ((max-items (if ai-debug-truncate-content 5 50)))
    (dotimes (i (min item-count max-items))
      (let ((item (nth i items)))
        (ai-debug--insert-context-section item 2)))

    (when (and ai-debug-truncate-content (> item-count max-items))
      (insert (propertize
               (format "  [%d more items not shown - toggle truncation with C-c t]\n\n"
                       (- item-count max-items))
               'face 'ai-debug-context-metadata)))))

(defun ai-debug--insert-messages-by-category (messages)
  "Insert MESSAGES grouped by type/category in expandable sections.
Creates organized categories for better navigation and understanding."
  (cl-multiple-value-bind (category-groups category-order)
      (ai-debug--group-messages-by-type messages)

    ;; Insert each category with file counts in headers
    (dolist (category category-order)
      (let* ((items (gethash category category-groups))
             (item-count (length items))
             (file-count (ai-debug--calculate-category-file-count category items))
             (header-text (ai-debug--format-category-header category item-count file-count)))

        (magit-insert-section (ai-message-category category t) ; Pass 't' as hidden flag
          (magit-insert-heading
            (propertize header-text 'face 'ai-debug-category-header))

          ;; Choose appropriate insertion method based on category
          (if (string= category "project-context")
              (ai-debug--insert-project-context-category items)
            (ai-debug--insert-regular-category items item-count)))))))

(defun ai-debug--hide-child-sections (section types-to-hide)
  "Hide child sections of SECTION that match TYPES-TO-HIDE list."
  (when section
    (dolist (child (oref section children))
      (when (memq (oref child type) types-to-hide)
        (magit-section-hide child)))))

(defun ai-debug--hide-empty-sections ()
  "Hide empty sections in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (max-iterations (if ai-debug-truncate-content 100 1000)))
      (while (and (< count max-iterations) (re-search-forward "^" nil t))
        (setq count (1+ count))
        (when-let ((section (condition-case-unless-debug nil (magit-current-section) (error nil))))
          (when (and (condition-case-unless-debug nil (eq (oref section type) 'ai-source-section) (error nil))
                     (condition-case-unless-debug nil (oref section value) (error nil)))
            (condition-case-unless-debug nil (magit-section-hide section) (error nil))))))))

(defun ai-debug--collapse-all-sections-recursive ()
  "Recursively collapse all sections in the current buffer except the root."
  (save-excursion
    (goto-char (point-min))
    (condition-case-unless-debug nil
        (let ((root-section (magit-current-section)))
          (when root-section
            ;; Recursively collapse all sections
            (ai-debug--collapse-section-recursive root-section)))
      (error nil))))

(defun ai-debug--collapse-section-recursive (section)
  "Recursively collapse SECTION and all its children."
  (when section
    ;; First recursively collapse all children
    (dolist (child (oref section children))
      (ai-debug--collapse-section-recursive child)
      (magit-section-hide child))
    ;; Don't hide the root section itself
    (unless (eq (oref section type) 'ai-debug-root)
      (magit-section-hide section))))

;; ============================================================================
;; AI Completions Debug Functions
;; ============================================================================

(defun ai-debug--collect-completions-state ()
  "Collect current AI completions state from the current buffer.
Returns a plist with completion session information."
  (let ((active (when (boundp 'ai-completions--active)
                  ai-completions--active))
        (candidates (when (boundp 'ai-completions--candidates)
                      ai-completions--candidates))
        (current-candidate (when (boundp 'ai-completions--current-candidate)
                             ai-completions--current-candidate))
        (complete-at-point (when (boundp 'ai-completions--complete-at-point)
                             ai-completions--complete-at-point))
        (preview-at-point (when (boundp 'ai-completions--preview-at-point)
                            ai-completions--preview-at-point))
        (strategy (when (boundp 'ai-completions--strategy)
                    ai-completions--strategy))
        (current-command-struct (when (boundp 'ai-completions--current-command-struct)
                                  ai-completions--current-command-struct))
        (preceding-context-size (when (boundp 'ai-completions--current-precending-context-size)
                                  ai-completions--current-precending-context-size))
        (following-context-size (when (boundp 'ai-completions--current-forwarding-context-size)
                                  ai-completions--current-forwarding-context-size))
        (use-full-context (when (boundp 'ai-completions--use-full-context)
                            ai-completions--use-full-context))
        (buffer-clone (when (boundp 'ai-completions--current-buffer-clone)
                        ai-completions--current-buffer-clone)))

    `(:active ,active
      :candidates ,candidates
      :current-candidate ,current-candidate
      :complete-at-point ,complete-at-point
      :preview-at-point ,preview-at-point
      :strategy ,strategy
      :current-command-struct ,current-command-struct
      :preceding-context-size ,preceding-context-size
      :following-context-size ,following-context-size
      :use-full-context ,use-full-context
      :buffer-clone ,buffer-clone)))

(defun ai-debug--insert-completion-session-state (state)
  "Insert completion session state information from STATE plist."
  (magit-insert-section (ai-completion-session nil t)
    (magit-insert-heading
      (propertize (format "Completion Session State (%s)"
                         (if (plist-get state :active) "ACTIVE" "INACTIVE"))
                 'face (if (plist-get state :active)
                           'ai-debug-completion-active
                         'ai-debug-completion-inactive)))

    ;; Session status
    (let ((active (plist-get state :active))
          (strategy (plist-get state :strategy))
          (complete-at-point (plist-get state :complete-at-point))
          (preview-at-point (plist-get state :preview-at-point))
          (use-full-context (plist-get state :use-full-context)))

      (insert (format "%-25s: %s\n" "Session Active"
                     (propertize (if active "YES" "NO")
                                'face (if active 'ai-debug-completion-active 'ai-debug-completion-inactive))))
      (when strategy
        (insert (format "%-25s: %s\n" "Strategy" (ai-debug--format-display-value strategy nil))))
      (when complete-at-point
        (insert (format "%-25s: %d\n" "Complete At Point" complete-at-point)))
      (when preview-at-point
        (insert (format "%-25s: %d\n" "Preview At Point" preview-at-point)))
      (insert (format "%-25s: %s\n" "Use Full Context"
                     (ai-debug--format-display-value use-full-context nil))))

    (insert "\n")))

(defun ai-debug--insert-completion-context-settings (state)
  "Insert completion context settings from STATE plist."
  (magit-insert-section (ai-completion-context nil t)
    (magit-insert-heading
      (propertize "Context Settings" 'face 'ai-debug-context-type))

    (let ((preceding-size (plist-get state :preceding-context-size))
          (following-size (plist-get state :following-context-size))
          (use-full-context (plist-get state :use-full-context)))

      (if use-full-context
          (insert (propertize "Using full buffer context (no size limits)\n" 'face 'ai-debug-context-metadata))
        (progn
          (insert (format "%-25s: %s\n" "Preceding Context Size"
                         (if preceding-size (number-to-string preceding-size) "default")))
          (insert (format "%-25s: %s\n" "Following Context Size"
                         (if following-size (number-to-string following-size) "default")))))

      ;; Show global context settings for reference
      (insert (propertize "\nGlobal Context Settings (for reference):\n" 'face 'ai-debug-context-metadata))
      (when (boundp 'ai-context-management--current-precending-context-size)
        (insert (format "%-25s: %d\n" "Global Preceding Size"
                       ai-context-management--current-precending-context-size)))
      (when (boundp 'ai-context-management--current-forwarding-context-size)
        (insert (format "%-25s: %d\n" "Global Following Size"
                       ai-context-management--current-forwarding-context-size))))

    (insert "\n")))

(defun ai-debug--insert-completion-candidates (state)
  "Insert completion candidates information from STATE plist."
  (let ((candidates (plist-get state :candidates))
        (current-candidate (plist-get state :current-candidate)))

    (magit-insert-section (ai-completion-candidates nil t)
      (magit-insert-heading
        (propertize (format "Completion Candidates (%d total, current: %d)"
                           (if candidates (length candidates) 0)
                           (if (and current-candidate candidates) (1+ current-candidate) 0))
                   'face 'ai-debug-context-type))

      (if (and candidates (> (length candidates) 0))
          (let ((max-candidates (if ai-debug-truncate-content 3 10)))
            (dotimes (i (min (length candidates) max-candidates))
              (let* ((candidate (nth i candidates))
                     (content (ai-common--get-text-content-from-struct candidate))
                     (is-current (and current-candidate (= i current-candidate)))
                     (truncated-content (if (and content (> (length content) 100))
                                            (concat (substring content 0 100) "...")
                                          content)))

                (magit-insert-section (ai-completion-candidate i t)
                  (magit-insert-heading
                    (propertize (format "Candidate %d%s" (1+ i) (if is-current " [CURRENT]" ""))
                               'face (if is-current 'ai-debug-completion-active 'ai-debug-context-type)))

                  (when content
                    (insert (propertize (format "Content (%d chars):\n" (length content))
                                       'face 'ai-debug-context-metadata))
                    (ai-debug--safe-insert-content truncated-content ai-debug-max-recursion-depth 0))
                  (insert "\n"))))

            (when (> (length candidates) max-candidates)
              (insert (propertize (format "  [%d more candidates not shown - toggle truncation with C-c t]\n\n"
                                         (- (length candidates) max-candidates))
                                 'face 'ai-debug-context-metadata))))
        (insert (propertize "  No candidates available\n\n" 'face 'ai-debug-empty-source))))))

(defun ai-debug--insert-completion-command-struct (state)
  "Insert current command struct information from STATE plist."
  (let ((command-struct (plist-get state :current-command-struct)))

    (magit-insert-section (ai-completion-command nil t)
      (magit-insert-heading
        (propertize "Current Command Structure" 'face 'ai-debug-context-type))

      (if (and command-struct (ai-command-p command-struct))
          (let ((name (ai-structs--get-command-name command-struct))
                (canonical-name (ai-structs--get-command-canonical-name command-struct))
                (source (ai-structs--get-command-source command-struct))
                (location (ai-structs--get-command-location command-struct))
                (result-action (ai-structs--get-result-action command-struct))
                (needs-user-input (ai-structs--command-needs-user-input-p command-struct))
                (needs-buffer-context (ai-structs--command-needs-buffer-context-p command-struct))
                (needs-project-context (ai-structs--command-needs-project-context-p command-struct))
                (file-path (ai-structs--get-command-file-path command-struct)))

            (insert (format "%-25s: %s\n" "Name" name))
            (insert (format "%-25s: %s\n" "Canonical Name" canonical-name))
            (insert (format "%-25s: %s\n" "Source" source))
            (insert (format "%-25s: %s\n" "Location" location))
            (insert (format "%-25s: %s\n" "Result Action" result-action))
            (insert (format "%-25s: %s\n" "Needs User Input" needs-user-input))
            (insert (format "%-25s: %s\n" "Needs Buffer Context" needs-buffer-context))
            (insert (format "%-25s: %s\n" "Needs Project Context" needs-project-context))
            (when file-path
              (insert (format "%-25s: %s\n" "File Path" file-path))))
        (insert (propertize "  No command structure available\n" 'face 'ai-debug-empty-source)))

      (insert "\n"))))

(defun ai-debug--insert-completion-buffer-state (state)
  "Insert buffer state information from STATE plist."
  (let ((buffer-clone (plist-get state :buffer-clone)))

    (magit-insert-section (ai-completion-buffer nil t)
      (magit-insert-heading
        (propertize "Buffer State" 'face 'ai-debug-context-type))

      (insert (format "%-25s: %s\n" "Current Buffer" (buffer-name)))
      (insert (format "%-25s: %s\n" "Buffer Clone"
                     (if (and buffer-clone (buffer-live-p buffer-clone))
                         (buffer-name buffer-clone)
                       "None")))
      (insert (format "%-25s: %s\n" "Point" (point)))
      (insert (format "%-25s: %s\n" "Region Active" (use-region-p)))
      (when (use-region-p)
        (insert (format "%-25s: %d - %d\n" "Region" (region-beginning) (region-end))))

      (insert "\n"))))

(defun ai-debug-show-completions ()
  "Display AI completions debug information in a visual interface.
Shows current completion session state, candidates, context settings,
and command structure for debugging completion behavior."
  (interactive)
  (condition-case-unless-debug err
    (let ((buffer (ai-debug--create-completions-buffer)))
      (with-current-buffer buffer
        (setq buffer-read-only nil)

        ;; Set up refresh capability for completions buffer
        (setq-local ai-debug--refresh-function 'ai-debug-show-completions)
        (setq-local ai-debug--refresh-args nil)

        ;; Collect completions state from the original buffer
        (let ((state (with-current-buffer (or (get-buffer "*ai-completions-debug-source*")
                                              (current-buffer))
                       (ai-debug--collect-completions-state))))

          ;; Insert main header
          (magit-insert-section (ai-completions-root)
            (magit-insert-heading
              (propertize "AI Completions Debug" 'face 'ai-debug-category-header))

            ;; Insert different sections of completion debug info
            (ai-debug--insert-completion-session-state state)
            (ai-debug--insert-completion-context-settings state)
            (ai-debug--insert-completion-candidates state)
            (ai-debug--insert-completion-command-struct state)
            (ai-debug--insert-completion-buffer-state state)))

        ;; Set up buffer display
        (setq buffer-read-only t)
        (goto-char (point-min))
        ;; Collapse all sections by default
        (condition-case-unless-debug nil
          (ai-debug--collapse-all-sections-recursive)
          (error nil)))

      (pop-to-buffer buffer))
    (error
     (message "Error in ai-debug-show-completions: %s" (error-message-string err)))))

(defun ai-debug-completions-from-buffer (&optional source-buffer)
  "Debug completions state from a specific SOURCE-BUFFER.
If SOURCE-BUFFER is not provided, uses current buffer."
  (interactive)
  (let ((source (or source-buffer (current-buffer))))
    (with-current-buffer source
      ;; Create a temporary buffer marker for state collection
      (let ((debug-source-buffer (get-buffer-create "*ai-completions-debug-source*")))
        (with-current-buffer debug-source-buffer
          ;; Copy relevant completion variables from source buffer
          (when (boundp 'ai-completions--active)
            (setq-local ai-completions--active
                        (with-current-buffer source ai-completions--active)))
          (when (boundp 'ai-completions--candidates)
            (setq-local ai-completions--candidates
                        (with-current-buffer source ai-completions--candidates)))
          (when (boundp 'ai-completions--current-candidate)
            (setq-local ai-completions--current-candidate
                        (with-current-buffer source ai-completions--current-candidate)))
          ;; Copy other completion state variables...
          )
        ;; Now show the debug interface
        (ai-debug-show-completions)))))

(defun ai-debug-show-raw-structures (context)
  "Display typed structures from CONTEXT in raw sequential format.
Shows all typed structures in the order they appear in the context
with detailed type information and nested structure support."
  (interactive
   (list (when (bound-and-true-p ai-mode)
           (let* ((command-struct (when (fboundp 'ai-command-management--get-unrestricted-command)
                                    (ai-command-management--get-unrestricted-command)))
                  (context (when (fboundp 'ai-context-management--get-executions-context-for-command)
                             (ai-context-management--get-executions-context-for-command
                              command-struct
                              :model (when (fboundp 'ai-model-management-get-current)
                                       (ai-model-management-get-current))))))
             context))))
  (condition-case-unless-debug err
    (let ((buffer (ai-debug--create-raw-structures-buffer))
          (raw-messages (condition-case-unless-debug nil (plist-get context :messages) (error nil)))
          (processed-messages nil))

      ;; Process messages through adapter API if available
      (setq processed-messages
            (condition-case-unless-debug nil
              (if (and raw-messages (fboundp 'ai-mode-adapter-api-prepare-messages))
                  (ai-mode-adapter-api-prepare-messages raw-messages)
                raw-messages)
              (error raw-messages)))

      (with-current-buffer buffer
        (setq buffer-read-only nil)

        ;; Store refresh function and args for later use
        (setq-local ai-debug--refresh-function 'ai-debug-show-raw-structures)
        (setq-local ai-debug--refresh-args (list context))

        ;; Insert main header
        (magit-insert-section (ai-raw-structures-root)
          (magit-insert-heading
            (propertize (format "AI Raw Typed Structures (%d total, %d processed)"
                               (if raw-messages (length raw-messages) 0)
                               (if processed-messages (length processed-messages) 0))
                       'face 'ai-debug-category-header))

          (if processed-messages
              (progn
                (insert (propertize "Structures are displayed after processing through ai-mode-adapter-api-prepare-messages.\n"
                                   'face 'ai-debug-context-metadata))
                (insert (propertize "Each structure shows its type, metadata, TTL, and content in expandable sections.\n\n"
                                   'face 'ai-debug-context-metadata))

                ;; Insert each structure sequentially
                (let ((index 0))
                  (dolist (struct processed-messages)
                    (when (and (listp struct) (keywordp (car struct)))
                      (ai-debug--insert-raw-typed-struct struct index))
                    (setq index (1+ index)))))

            ;; No messages case
            (insert (propertize "No typed structures found in context.\n" 'face 'ai-debug-empty-source))
            (insert (propertize "This could indicate:\n" 'face 'ai-debug-context-metadata))
            (insert (propertize "- Empty or invalid context\n" 'face 'ai-debug-context-metadata))
            (insert (propertize "- Context not properly loaded\n" 'face 'ai-debug-context-metadata))
            (insert (propertize "- Error in context generation\n" 'face 'ai-debug-context-metadata))))

        ;; Set up buffer display
        (setq buffer-read-only t)
        (goto-char (point-min))
        ;; Collapse all sections by default
        (condition-case-unless-debug nil
          (ai-debug--collapse-all-sections-recursive)
          (error nil)))

      (pop-to-buffer buffer))
    (error
     (message "Error in ai-debug-show-raw-structures: %s" (error-message-string err)))))

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

              ;; File Ignore Patterns Category
              (ai-debug--insert-ignore-patterns-section)

              ;; Buffer Context Category - using buffer-state from context
              (when-let ((buffer-state (plist-get context :buffer-state)))
                (magit-insert-section (ai-buffer-info nil t) ; Pass 't' as hidden flag
                  (magit-insert-heading
                    (propertize "Buffer Context" 'face 'ai-debug-category-header))
                  ;; Extract buffer information from buffer-state
                  (let ((buffer-name (ai-buffer-state-buf-name buffer-state))
                        (file-name (ai-buffer-state-buf-filename buffer-state))
                        (major-mode (ai-buffer-state-buf-major-mode buffer-state))
                        (current-point (ai-buffer-state-cur-point buffer-state))
                        (current-line (ai-buffer-state-cur-line buffer-state))
                        (current-column (ai-buffer-state-cur-column buffer-state))
                        (region-active (ai-buffer-state-reg-active buffer-state))
                        (region-content (ai-buffer-state-reg-content buffer-state)))
                    ;; Display buffer information
                    (insert (format "%-15s: %s\n" "buffer-name" (ai-debug--format-display-value buffer-name nil)))
                    (when file-name
                      (insert (format "%-15s: %s\n" "file-name" (ai-debug--format-display-value file-name nil))))
                    (insert (format "%-15s: %s\n" "major-mode" (ai-debug--format-display-value major-mode nil)))
                    (insert (format "%-15s: %s\n" "cursor-point" (ai-debug--format-display-value current-point nil)))
                    (insert (format "%-15s: %s\n" "cursor-line" (ai-debug--format-display-value current-line nil)))
                    (insert (format "%-15s: %s\n" "cursor-column" (ai-debug--format-display-value current-column nil)))
                    (insert (format "%-15s: %s\n" "region-active" (ai-debug--format-display-value region-active nil)))
                    (when region-content
                      (insert (format "%-15s: %s\n" "region-content" (ai-debug--format-display-value region-content t)))))
                  (insert "\n")))

              ;; Messages Category (organized by type)
              (when messages
                (magit-insert-section (ai-messages-root nil t) ; Pass 't' as hidden flag
                  (magit-insert-heading
                    (propertize (format "AI Messages (%d total)" (length messages))
                                'face 'ai-debug-category-header))
                  (ai-debug--insert-messages-by-category messages))))

            (setq buffer-read-only t)
            (goto-char (point-min))
            ;; Use the new recursive collapse function
            (condition-case-unless-debug nil
                (ai-debug--collapse-all-sections-recursive)
              (error nil)))

          (pop-to-buffer buffer))
      (error
       (message "Error in ai-debug-show-context-debug: %s" (error-message-string err)))))

(defun ai-debug-show-context ()
  "Show debug context for the current AI operation.
Interactive command that displays the current AI context in a debug buffer.
Provides comprehensive information about model configuration, context,
and message structure for troubleshooting AI operations."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((command-struct (condition-case-unless-debug nil
                               (when (fboundp 'ai-command-management--get-unrestricted-command)
                                 (ai-command-management--get-unrestricted-command))
                               (error nil)))
             (context (condition-case-unless-debug nil
                        (when (fboundp 'ai-context-management--get-executions-context-for-command)
                          (ai-context-management--get-executions-context-for-command
                           command-struct
                           :model (when (fboundp 'ai-model-management-get-current)
                                    (ai-model-management-get-current))))
                        (error nil))))
        (if context
            (ai-debug-show-context-debug context)
          (message "Could not retrieve AI context"))))
    (error
     (message "Error in ai-debug-show-context: %s" (error-message-string err)))))

(defun ai-debug-show-raw-structures-interactive ()
  "Show raw typed structures for the current AI operation.
Interactive command that displays typed structures in their original sequence.
Useful for understanding the exact order and structure of AI context elements."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((command-struct (condition-case-unless-debug nil
                               (when (fboundp 'ai-command-management--get-unrestricted-command)
                                 (ai-command-management--get-unrestricted-command))
                               (error nil)))
             (context (condition-case-unless-debug nil
                        (when (fboundp 'ai-context-management--get-executions-context-for-command)
                          (ai-context-management--get-executions-context-for-command
                           command-struct
                           :model (when (fboundp 'ai-model-management-get-current)
                                    (ai-model-management-get-current))))
                        (error nil))))
        (if context
            (ai-debug-show-raw-structures context)
          (message "Could not retrieve AI context"))))
    (error
     (message "Error in ai-debug-show-raw-structures: %s" (error-message-string err)))))

(defun ai-debug-completion-limited-context ()
  "Debug context for completion with limited context.
Shows completion context with restricted preceding and following context sizes.
Useful for debugging completion performance and context truncation issues."
  (interactive)
  (condition-case-unless-debug err
    (when (bound-and-true-p ai-mode)
      (let* ((command-struct (condition-case-unless-debug nil
                               (when (fboundp 'ai-command-management--get-command-from-registry)
                                 (ai-command-management--get-command-from-registry "complete"))
                               (error nil)))
             (context (condition-case-unless-debug nil
                        (when (and (fboundp 'ai-context-management--get-executions-context-for-command)
                                   (fboundp 'ai-model-management-get-current))
                          (ai-context-management--get-executions-context-for-command
                           command-struct
                           :model (ai-model-management-get-current)))
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
      (let* ((command-struct (condition-case-unless-debug nil
                               (when (fboundp 'ai-command-management--get-command-from-registry)
                                 ;; Create a command struct with full context settings
                                 (let ((base-command (ai-command-management--get-command-from-registry "complete")))
                                   (when base-command
                                     ;; Modify the behavior to use full context
                                     (let ((modified-behavior (copy-tree (ai-structs--get-command-behavior base-command))))
                                       (setf (ai-command-behavior-preceding-context-size modified-behavior) nil)
                                       (setf (ai-command-behavior-following-context-size modified-behavior) nil)
                                       (setf (ai-command-behavior base-command) modified-behavior)
                                       base-command))))
                               (error nil)))
             (context (condition-case-unless-debug nil
                        (when (and (fboundp 'ai-context-management--get-executions-context-for-command)
                                   (fboundp 'ai-model-management-get-current))
                          (ai-context-management--get-executions-context-for-command
                           command-struct
                           :model (ai-model-management-get-current)))
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
  (define-key ai-command-map (kbd "R") 'ai-debug-show-raw-structures-interactive)
  (define-key ai-command-map (kbd "C-c") 'ai-debug-show-completions)
  (define-key ai-command-map (kbd "C-d l") 'ai-debug-completion-limited-context)
  (define-key ai-command-map (kbd "C-d f") 'ai-debug-completion-full-context))

(provide 'ai-debug)

;;; ai-debug.el ends here
