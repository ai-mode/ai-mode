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
;; - File-based command inspection and debugging
;; - System prompts inspection and debugging
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
;; - File-based commands with source location tracking
;; - System prompts with source location tracking
;; - Raw typed structures in their original sequence
;; - Selection and cursor positioning information
;;
;; Usage:
;; Enable AI mode and use the following commands:
;; - `ai-debug-visual': Main debug interface showing current context
;; - `ai-debug-show-sources': Display all available context sources
;; - `ai-debug-show-file-commands': Display all loaded file-based commands
;; - `ai-debug-show-system-prompts': Display all loaded system prompts
;; - `ai-debug-show-raw-structures': Display typed structures in original sequence
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

(defvar ai-debug-file-commands-buffer-name "*AI File Commands*"
  "Buffer name for displaying AI file-based commands.")

(defvar ai-debug-system-prompts-buffer-name "*AI System Prompts*"
  "Buffer name for displaying AI system prompts.")

(defvar ai-debug-raw-structures-buffer-name "*AI Raw Structures*"
  "Buffer name for displaying AI typed structures in raw format.")

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

(defun ai-debug--create-file-commands-buffer ()
  "Create and return the AI file commands buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-file-commands-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI File Commands - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
                                       (if ai-debug-truncate-content "ON" "OFF")))
      ;; Add local keymap for debug buffer operations
      (local-set-key (kbd "C-c t") 'ai-debug-toggle-truncation)
      (local-set-key (kbd "C-r") 'ai-debug-refresh-buffer))
    buffer))

(defun ai-debug--create-system-prompts-buffer ()
  "Create and return the AI system prompts buffer.
Initializes the buffer with magit-section-mode and appropriate keybindings."
  (let ((buffer (get-buffer-create ai-debug-system-prompts-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (magit-section-mode)
      (setq header-line-format (format "AI System Prompts - TAB to expand/collapse, truncation: %s (C-c t to toggle, C-r to refresh)"
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

   ;; File commands debug buffer
   ((string= (buffer-name) ai-debug-file-commands-buffer-name)
    (message "Refreshing AI file commands...")
    (ai-debug-show-file-commands)
    (message "AI file commands refreshed"))

   ;; System prompts debug buffer
   ((string= (buffer-name) ai-debug-system-prompts-buffer-name)
    (message "Refreshing AI system prompts...")
    (ai-debug-show-system-prompts)
    (message "AI system prompts refreshed"))

   ;; Raw structures debug buffer
   ((string= (buffer-name) ai-debug-raw-structures-buffer-name)
    (message "Refreshing AI raw structures...")
    (if ai-debug--refresh-function
        (apply ai-debug--refresh-function ai-debug--refresh-args)
      (ai-debug-show-raw-structures))
    (message "AI raw structures refreshed"))

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

(defun ai-debug--insert-source-section (source-name source-data description)
  "Insert a section for SOURCE-NAME with SOURCE-DATA and DESCRIPTION.
Creates a collapsible section showing context source information
with comprehensive statistics and safe content handling."
  (condition-case-unless-debug err
    (let* ((is-empty (ai-debug--is-empty-source source-data))
           (total-count (condition-case-unless-debug nil
                          (cond
                           ((null source-data) 0)
                           ((and (listp source-data) (not (null source-data)) (keywordp (car source-data))) 1)
                           ((listp source-data) (length source-data))
                           (t 1))
                          (error 0)))
           (non-empty-count (ai-debug--count-non-empty-items source-data))
           (count-info (if (> total-count 0)
                           (format " [%d/%d items]" non-empty-count total-count)
                         " [empty]")))

      (magit-insert-section (ai-source-section source-name is-empty t) ; Pass 't' as hidden flag
        (magit-insert-heading
          (propertize (format "%s%s" source-name count-info) 'face 'ai-debug-context-header)
          (when description
            (concat " " (propertize (format "- %s" description) 'face 'ai-debug-context-metadata))))

        (cond
         ;; Empty or nil data
         (is-empty
          (insert (propertize "  (empty)\n\n" 'face 'ai-debug-empty-source)))

         ;; Special handling for Project Context (which contains nested file lists/structs)
         ((string-match-p "Project Context" source-name)
          (ai-debug--insert-project-context-category (list source-data))) ; Pass source-data as a list to match category fn signature

         ;; If source-data is a list of typed structs (e.g., summaries, memory items, buffer-bound prompts)
         ((and (listp source-data) (not (null source-data))
               (listp (car source-data)) (not (null (car source-data))) (keywordp (car (car source-data))))
          (let ((item-count (min (length source-data) (if ai-debug-truncate-content 10 100))))
            (when ai-debug-truncate-content
              (insert (propertize (format "  [showing %d of %d items]\n" item-count (length source-data)) 'face 'ai-debug-context-metadata)))
            (dotimes (i item-count)
              (let ((item (nth i source-data)))
                (ai-debug--insert-typed-struct item 1 source-name))) ; Pass source-name for parent context
            (when (and ai-debug-truncate-content (> (length source-data) item-count))
              (insert (propertize (format "  [%d more items not shown%s]\n\n"
                                         (- (length source-data) item-count)
                                         (if ai-debug-truncate-content " - toggle truncation with C-c t" ""))
                                 'face 'ai-debug-context-metadata)))))

         ;; If source-data is a single typed struct (not a list of structs)
         ((and (listp source-data) (not (null source-data)) (keywordp (car source-data)))
          (ai-debug--insert-typed-struct source-data 1 source-name)) ; Pass source-name for parent context

         ;; Other data types (e.g. simple strings)
         (t
          (insert (propertize (format "  %s\n" content) 'face 'default))))))
    (error
     (insert (propertize (format "[Error displaying source %s: %s]\n\n" source-name (error-message-string err)) 'face 'ai-debug-empty-source)))))

(defun ai-debug--insert-ignore-patterns-section ()
  "Insert file ignore patterns section with subsections for each source.
Shows patterns from global hardcoded, global files, project .gitignore, and project .ai-ignore,
followed by an aggregated result section."
  (condition-case-unless-debug err
      (let ((project-root (condition-case-unless-debug nil
                            (ai-common--get-project-root)
                            (error nil))))
        (magit-insert-section (ai-ignore-patterns nil t) ; Pass 't' as hidden flag
          (magit-insert-heading
            (propertize "File Ignore Patterns" 'face 'ai-debug-category-header))

          ;; Global hardcoded patterns subsection
          (let ((hardcoded-patterns (condition-case-unless-debug nil
                                      (ai-common--get-global-hardcoded-patterns)
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
                                        (ai-common--get-global-ignore-file-patterns)
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
                                        (ai-common--get-project-gitignore-patterns project-root)
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
                                        (ai-common--get-project-ai-ignore-patterns project-root)
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
                                  (ai-common--get-all-ignore-patterns project-root)
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

(defun ai-debug--collect-file-commands ()
  "Collect all loaded file-based commands from all instruction sources.
Returns a plist with command data grouped by location."
  (let ((default-commands '())
        (global-commands '())
        (local-commands '()))

    ;; Collect from default instructions (if available)
    (when (and (boundp 'ai--default-instructions-cache)
               (hash-table-p ai--default-instructions-cache))
      (maphash (lambda (command-name content)
                 (push `(:name ,command-name
                         :content ,content
                         :location "Default (Package)"
                         :directory ,(ai--get-default-instructions-directory))
                       default-commands))
               ai--default-instructions-cache))

    ;; Collect from global instructions (if available)
    (when (and (boundp 'ai--global-instructions-cache)
               (hash-table-p ai--global-instructions-cache))
      (maphash (lambda (command-name content)
                 (push `(:name ,command-name
                         :content ,content
                         :location "Global (~/.ai)"
                         :directory ,(ai--get-global-instructions-directory))
                       global-commands))
               ai--global-instructions-cache))

    ;; Collect from local instructions (if available)
    (when (and (boundp 'ai--local-instructions-cache)
               (hash-table-p ai--local-instructions-cache))
      (maphash (lambda (command-name content)
                 (push `(:name ,command-name
                         :content ,content
                         :location "Local (Project)"
                         :directory ,(ai--get-local-instructions-directory))
                       local-commands))
               ai--local-instructions-cache))

    `(:default-commands ,(reverse default-commands)
      :global-commands ,(reverse global-commands)
      :local-commands ,(reverse local-commands))))

(defun ai-debug--collect-system-prompts ()
  "Collect all loaded system prompts from all sources.
Returns a plist with system prompt data grouped by location."
  (let ((default-prompts '())
        (global-prompts '())
        (local-prompts '()))

    ;; Collect from default system prompts (if available)
    (when (and (boundp 'ai--default-system-prompts-cache)
               (hash-table-p ai--default-system-prompts-cache))
      (maphash (lambda (prompt-name content)
                 (push `(:name ,prompt-name
                         :content ,content
                         :location "Default (Package)"
                         :directory ,(ai--get-default-system-prompts-directory))
                       default-prompts))
               ai--default-system-prompts-cache))

    ;; Collect from global system prompts (if available)
    (when (and (boundp 'ai--global-system-prompts-cache)
               (hash-table-p ai--global-system-prompts-cache))
      (maphash (lambda (prompt-name content)
                 (push `(:name ,prompt-name
                         :content ,content
                         :location "Global (~/.ai/system)"
                         :directory ,(ai--get-global-system-prompts-directory))
                       global-prompts))
               ai--global-system-prompts-cache))

    ;; Collect from local system prompts (if available)
    (when (and (boundp 'ai--local-system-prompts-cache)
               (hash-table-p ai--local-system-prompts-cache))
      (maphash (lambda (prompt-name content)
                 (push `(:name ,prompt-name
                         :content ,content
                         :location "Local (Project)"
                         :directory ,(ai--get-local-system-prompts-directory))
                       local-prompts))
               ai--local-system-prompts-cache))

    `(:default-prompts ,(reverse default-prompts)
      :global-prompts ,(reverse global-prompts)
      :local-prompts ,(reverse local-prompts))))

(defun ai-debug--extract-modifier-indicators (command-name)
  "Extract modifier indicators from COMMAND-NAME for display.
Returns a list of shortened modifier indicators."
  (condition-case-unless-debug nil
    (when (and (fboundp 'ai--parse-command-modifiers) (string-match-p "__" command-name))
      (let* ((parsed (ai--parse-command-modifiers command-name))
             (modifier-config (cdr parsed))
             (indicators '()))

        ;; Add modifier indicators
        (when (plist-get modifier-config :user-input)
          (push "U" indicators))  ; User input required
        (when (plist-get modifier-config :needs-buffer-context)
          (push "B" indicators))  ; Buffer context needed
        (when (plist-get modifier-config :needs-project-context)
          (push "P" indicators))  ; Project context needed

        ;; Add result action indicator
        (when-let ((result-action (plist-get modifier-config :result-action)))
          (let ((action-indicator (cond
                                  ((eq result-action 'show) "S")
                                  ((eq result-action 'eval) "E")
                                  ((eq result-action 'replace) "R")
                                  ((eq result-action 'insert-at-point) "I")
                                  ((eq result-action 'complete) "C")
                                  (t "?"))))
            (push action-indicator indicators)))

        ;; Add context size indicators
        (when (plist-member modifier-config :preceding-context-size)
          (let ((preceding (plist-get modifier-config :preceding-context-size))
                (following (plist-get modifier-config :following-context-size)))
            (cond
             ((and (null preceding) (null following))
              (push "F" indicators))  ; Full context
             ((and (numberp preceding) (< preceding 10))
              (push "s" indicators))  ; Small context
             ((and (numberp preceding) (> preceding 15))
              (push "L" indicators))  ; Large context
             (t nil))))

        indicators))
    (error nil)))

(defun ai-debug--insert-file-command-item (command-info)
  "Insert a single file command COMMAND-INFO as an expandable section."
  (let* ((name (plist-get command-info :name))
         (content (plist-get command-info :content))
         (location (plist-get command-info :location))
         (directory (plist-get command-info :directory))
         (file-path (when directory
                      (ai--get-instruction-file-path name directory)))
         (modifier-indicators (ai-debug--extract-modifier-indicators name))
         (metadata-parts '()))

    ;; Build metadata parts
    (when file-path
      (push (format "file: %s" file-path) metadata-parts))
    (when modifier-indicators
      (push (format "mods: %s" (string-join modifier-indicators "")) metadata-parts))

    (magit-insert-section (ai-file-command name t) ; Pass 't' as hidden flag
      (magit-insert-heading
        (propertize name 'face 'ai-debug-context-type)
        (when metadata-parts
          (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                 'face 'ai-debug-context-metadata))))

      ;; Full content (truncated if necessary)
      (when (and content (> (length content) 0))
        (ai-debug--safe-insert-content content ai-debug-max-recursion-depth 0))

      (insert "\n"))))

(defun ai-debug--insert-system-prompt-item (prompt-info)
  "Insert a single system prompt PROMPT-INFO as an expandable section."
  (let* ((name (plist-get prompt-info :name))
         (content (plist-get prompt-info :content))
         (location (plist-get prompt-info :location))
         (directory (plist-get prompt-info :directory))
         (file-path (when directory
                      (ai--get-instruction-file-path name directory)))
         (metadata-parts '()))

    ;; Build metadata parts
    (when file-path
      (push (format "file: %s" file-path) metadata-parts))

    (magit-insert-section (ai-system-prompt name t) ; Pass 't' as hidden flag
      (magit-insert-heading
        (propertize name 'face 'ai-debug-context-type)
        (when metadata-parts
          (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                 'face 'ai-debug-context-metadata))))

      ;; Full content (truncated if necessary)
      (when (and content (> (length content) 0))
        (ai-debug--safe-insert-content content ai-debug-max-recursion-depth 0))

      (insert "\n"))))

(defun ai-debug--insert-file-commands-by-location (location-name commands)
  "Insert file commands from LOCATION-NAME with COMMANDS list."
  (when commands
    (magit-insert-section (ai-file-commands-location location-name t) ; Pass 't' as hidden flag
      (magit-insert-heading
        (propertize (format "%s (%d commands)" location-name (length commands))
                   'face 'ai-debug-category-header))

      (dolist (command-info commands)
        (ai-debug--insert-file-command-item command-info)))))

(defun ai-debug--insert-system-prompts-by-location (location-name prompts)
  "Insert system prompts from LOCATION-NAME with PROMPTS list."
  (when prompts
    (magit-insert-section (ai-system-prompts-location location-name t) ; Pass 't' as hidden flag
      (magit-insert-heading
        (propertize (format "%s (%d prompts)" location-name (length prompts))
                   'face 'ai-debug-category-header))

      (dolist (prompt-info prompts)
        (ai-debug--insert-system-prompt-item prompt-info)))))

(defun ai-debug-show-file-commands ()
  "Display all loaded file-based commands in a visual magit-like interface.
Shows commands from default, global, and local instruction sources with
expandable sections for detailed inspection."
  (interactive)
  (condition-case-unless-debug err
    (let ((buffer (ai-debug--create-file-commands-buffer)))
      (with-current-buffer buffer
        (setq buffer-read-only nil)

        ;; Set up refresh capability for file commands buffer
        (setq-local ai-debug--refresh-function 'ai-debug-show-file-commands)
        (setq-local ai-debug--refresh-args nil)

        ;; Ensure caches are updated
        (when (fboundp 'ai--ensure-instructions-cache-updated)
          (condition-case-unless-debug nil
            (progn
              (ai--ensure-instructions-cache-updated
               (ai--get-default-instructions-directory)
               ai--default-instructions-cache "default")
              (ai--ensure-instructions-cache-updated
               (ai--get-global-instructions-directory)
               ai--global-instructions-cache "global")
              (when-let ((local-dir (ai--get-local-instructions-directory)))
                (ai--ensure-instructions-cache-updated
                 local-dir ai--local-instructions-cache "local")))
            (error nil)))

        ;; Collect file commands
        (let* ((commands-data (ai-debug--collect-file-commands))
               (default-commands (plist-get commands-data :default-commands))
               (global-commands (plist-get commands-data :global-commands))
               (local-commands (plist-get commands-data :local-commands))
               (total-commands (+ (length default-commands)
                                 (length global-commands)
                                 (length local-commands))))

          ;; Insert main header
          (magit-insert-section (ai-file-commands-root)
            (magit-insert-heading
              (propertize (format "AI File-Based Commands (%d total)" total-commands)
                         'face 'ai-debug-category-header))

            ;; Insert commands by location with priority order
            (ai-debug--insert-file-commands-by-location "Local (Project)" local-commands)
            (ai-debug--insert-file-commands-by-location "Global (~/.ai)" global-commands)
            (ai-debug--insert-file-commands-by-location "Default (Package)" default-commands)

            ;; Show message if no commands found
            (when (= total-commands 0)
              (insert (propertize "No file-based commands found.\n" 'face 'ai-debug-empty-source))
              (insert (propertize "File commands are loaded from:\n" 'face 'ai-debug-context-metadata))
              (insert (propertize (format "- Default: %s\n" (ai--get-default-instructions-directory)) 'face 'ai-debug-context-metadata))
              (insert (propertize (format "- Global: %s\n" (ai--get-global-instructions-directory)) 'face 'ai-debug-context-metadata))
              (when-let ((local-dir (ai--get-local-instructions-directory)))
                (insert (propertize (format "- Local: %s\n" local-dir) 'face 'ai-debug-context-metadata))))))

        ;; Set up buffer display
        (setq buffer-read-only t)
        (goto-char (point-min))
        ;; Collapse all sections by default
        (condition-case-unless-debug nil
          (ai-debug--collapse-all-sections-recursive)
          (error nil)))

      (pop-to-buffer buffer))
    (error
     (message "Error in ai-debug-show-file-commands: %s" (error-message-string err)))))

(defun ai-debug-show-system-prompts ()
  "Display all loaded system prompts in a visual magit-like interface.
Shows system prompts from default, global, and local sources with
expandable sections for detailed inspection."
  (interactive)
  (condition-case-unless-debug err
    (let ((buffer (ai-debug--create-system-prompts-buffer)))
      (with-current-buffer buffer
        (setq buffer-read-only nil)

        ;; Set up refresh capability for system prompts buffer
        (setq-local ai-debug--refresh-function 'ai-debug-show-system-prompts)
        (setq-local ai-debug--refresh-args nil)

        ;; Ensure caches are updated
        (when (fboundp 'ai--ensure-system-prompts-cache-updated)
          (condition-case-unless-debug nil
            (progn
              (ai--ensure-system-prompts-cache-updated
               (ai--get-default-system-prompts-directory)
               ai--default-system-prompts-cache "default-system")
              (ai--ensure-system-prompts-cache-updated
               (ai--get-global-system-prompts-directory)
               ai--global-system-prompts-cache "global-system")
              (when-let ((local-dir (ai--get-local-system-prompts-directory)))
                (ai--ensure-system-prompts-cache-updated
                 local-dir ai--local-system-prompts-cache "local-system")))
            (error nil)))

        ;; Collect system prompts
        (let* ((prompts-data (ai-debug--collect-system-prompts))
               (default-prompts (plist-get prompts-data :default-prompts))
               (global-prompts (plist-get prompts-data :global-prompts))
               (local-prompts (plist-get prompts-data :local-prompts))
               (total-prompts (+ (length default-prompts)
                                (length global-prompts)
                                (length local-prompts))))

          ;; Insert main header
          (magit-insert-section (ai-system-prompts-root)
            (magit-insert-heading
              (propertize (format "AI System Prompts (%d total)" total-prompts)
                         'face 'ai-debug-category-header))

            ;; Insert prompts by location with priority order
            (ai-debug--insert-system-prompts-by-location "Local (Project)" local-prompts)
            (ai-debug--insert-system-prompts-by-location "Global (~/.ai/system)" global-prompts)
            (ai-debug--insert-system-prompts-by-location "Default (Package)" default-prompts)

            ;; Show message if no prompts found
            (when (= total-prompts 0)
              (insert (propertize "No system prompts found.\n" 'face 'ai-debug-empty-source))
              (insert (propertize "System prompts are loaded from:\n" 'face 'ai-debug-context-metadata))
              (insert (propertize (format "- Default: %s\n" (ai--get-default-system-prompts-directory)) 'face 'ai-debug-context-metadata))
              (insert (propertize (format "- Global: %s\n" (ai--get-global-system-prompts-directory)) 'face 'ai-debug-context-metadata))
              (when-let ((local-dir (ai--get-local-system-prompts-directory)))
                (insert (propertize (format "- Local: %s\n" local-dir) 'face 'ai-debug-context-metadata))))))

        ;; Set up buffer display
        (setq buffer-read-only t)
        (goto-char (point-min))
        ;; Collapse all sections by default
        (condition-case-unless-debug nil
          (ai-debug--collapse-all-sections-recursive)
          (error nil)))

      (pop-to-buffer buffer))
    (error
     (message "Error in ai-debug-show-system-prompts: %s" (error-message-string err)))))

(defun ai-debug-show-raw-structures (context)
  "Display typed structures from CONTEXT in raw sequential format.
Shows all typed structures in the order they appear in the context
with detailed type information and nested structure support."
  (interactive)
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

              ;; Buffer Context Category
              (magit-insert-section (ai-buffer-info nil t) ; Pass 't' as hidden flag
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
              (magit-insert-section (ai-completion-info nil t) ; Pass 't' as hidden flag
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

;; Helper function to get descriptive name for project context mode
(defun ai-debug--get-project-context-mode-display-name (mode)
  "Return a human-readable string for the project context MODE."
  (cdr (assoc mode '((disabled . "Disabled")
                      (full-project . "Full Files")
                      (project-ai-summary . "Summary Index")))))

;; Helper functions for collecting context sources
(defun ai-debug--collect-context-sources ()
  "Collect all context sources safely with comprehensive error handling.
Returns a plist with all context source data and their counts."
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
                              (when (and (fboundp 'ai--get-full-project-context)
                                         (fboundp 'ai-common--get-project-root)
                                         (ai-common--get-project-root))
                                (ai--get-full-project-context))
                            (error nil)))
         (project-root (condition-case-unless-debug nil
                           (ai-common--get-project-root)
                         (error nil)))
         (project-summary-index (condition-case-unless-debug nil
                                    (when (and (boundp 'ai--project-files-summary-index) project-root)
                                      (gethash project-root ai--project-files-summary-index))
                                  (error nil)))
         (current-buffer-context (condition-case-unless-debug nil
                                     (unless (use-region-p)
                                       (when (fboundp 'ai--get-current-buffer-context)
                                         (ai--get-current-buffer-context)))
                                   (error nil)))
         (current-selection (condition-case-unless-debug nil
                                (when (use-region-p)
                                  (ai-common--make-snippet-from-region 'selection))
                              (error nil))))

    `(:global-system-prompts ,global-system-prompts
      :global-memory ,global-memory
      :buffer-bound-prompts ,buffer-bound-prompts
      :context-pool ,context-pool
      :project-context ,project-context
      :project-summary-index ,project-summary-index
      :current-buffer-context ,current-buffer-context
      :current-selection ,current-selection)))

(defun ai-debug--calculate-source-counts (sources)
  "Calculate various counts for context SOURCES.
Returns a plist with total, non-empty, and file counts."
  (let* ((all-sources (list (plist-get sources :global-system-prompts)
                           (plist-get sources :global-memory)
                           (plist-get sources :buffer-bound-prompts)
                           (plist-get sources :context-pool)
                           (plist-get sources :project-context)
                           (plist-get sources :project-summary-index)
                           (plist-get sources :current-buffer-context)
                           (plist-get sources :current-selection)))
         (total-sources (length (cl-remove-if #'null all-sources)))
         (non-empty-sources (length (cl-remove-if #'ai-debug--is-empty-source all-sources)))
         (project-context (plist-get sources :project-context))
         (project-files-count (if (and project-context (plist-get project-context :content))
                                  (condition-case-unless-debug nil
                                    (let ((content (plist-get project-context :content)))
                                      (when (and content (listp content))
                                        (let ((files-list-item (nth 0 content)))
                                          (when (and files-list-item (plist-get files-list-item :count))
                                            (plist-get files-list-item :count)))))
                                    (error 0))
                                0))
         (project-summary-index (plist-get sources :project-summary-index))
         (project-summary-count (if (and project-summary-index (listp project-summary-index))
                                    (length project-summary-index)
                                  0)))

    `(:total-sources ,total-sources
      :non-empty-sources ,non-empty-sources
      :project-files-count ,project-files-count
      :project-summary-count ,project-summary-count)))

(defun ai-debug--insert-all-source-sections (sources counts)
  "Insert all source sections with SOURCES data and COUNTS information."
  (let* ((project-files-count (plist-get counts :project-files-count))
         (project-summary-count (plist-get counts :project-summary-count))
         (project-context-mode-display (ai-debug--get-project-context-mode-display-name ai--project-context-mode)))

    ;; Insert each source section with error protection
    (ai-debug--insert-source-section
     "Global System Prompts"
     (plist-get sources :global-system-prompts)
     "System-wide instructions that define AI behavior")

    (ai-debug--insert-source-section
     "Global Memory"
     (plist-get sources :global-memory)
     "Persistent context that spans across all buffers")

    (ai-debug--insert-source-section
     "Buffer-bound Prompts"
     (plist-get sources :buffer-bound-prompts)
     "Instructions specific to current buffer")

    (ai-debug--insert-source-section
     "Context Pool"
     (plist-get sources :context-pool)
     "Temporary context for current interaction session")

    (ai-debug--insert-source-section
     (format "Project Context (%s, %d files)" project-context-mode-display project-files-count)
     (plist-get sources :project-context)
     "Filtered project files and structure")

    (ai-debug--insert-source-section
     (format "Project AI Summary Index (%d files)" project-summary-count)
     (plist-get sources :project-summary-index)
     "AI-generated summaries of project files")

    (when (plist-get sources :current-buffer-context)
      (ai-debug--insert-source-section
       "Current Buffer Content"
       (plist-get sources :current-buffer-context)
       "Full content of current buffer"))

    (when (plist-get sources :current-selection)
      (ai-debug--insert-source-section
       "Current Selection"
       (plist-get sources :current-selection)
       "Currently selected text region"))))

(defun ai-debug--setup-sources-buffer-display (buffer)
  "Set up the sources BUFFER for display with proper section handling."
  (with-current-buffer buffer
    (setq buffer-read-only t)
    (goto-char (point-min))
    ;; Use the new recursive collapse function
    (condition-case-unless-debug nil
        (ai-debug--collapse-all-sections-recursive)
      (error nil))
    ;; Hide empty sections with comprehensive error protection
    (condition-case-unless-debug nil
        (ai-debug--hide-empty-sections)
      (error nil))))

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

        ;; Collect context sources and calculate counts
        (let* ((sources (ai-debug--collect-context-sources))
               (counts (ai-debug--calculate-source-counts sources)))

          ;; Insert main header with counts
          (magit-insert-section (ai-sources-root)
            (magit-insert-heading
              (propertize (format "AI Context Sources [%d/%d active]"
                                 (plist-get counts :non-empty-sources)
                                 (plist-get counts :total-sources))
                         'face 'ai-debug-category-header))

            ;; Insert all source sections
            (ai-debug--insert-all-source-sections sources counts)))

        ;; Set up buffer display
        (ai-debug--setup-sources-buffer-display buffer))

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

(defun ai-debug-show-raw-structures-interactive ()
  "Show raw typed structures for the current AI operation.
Interactive command that displays typed structures in their original sequence.
Useful for understanding the exact order and structure of AI context elements."
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
  (define-key ai-command-map (kbd "f") 'ai-debug-show-file-commands)
  (define-key ai-command-map (kbd "s") 'ai-debug-show-system-prompts)
  (define-key ai-command-map (kbd "R") 'ai-debug-show-raw-structures-interactive)
  (define-key ai-command-map (kbd "C-d l") 'ai-debug-completion-limited-context)
  (define-key ai-command-map (kbd "C-d f") 'ai-debug-completion-full-context))

(provide 'ai-debug)

;;; ai-debug.el ends here
