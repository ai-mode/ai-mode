;;; ai-response-processors-debug.el --- Debug interface for AI response processors -*- lexical-binding: t -*-

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
;; This module provides a debugging interface for AI response processors
;; using magit-section to visualize response buffer contexts, helping
;; developers understand and debug the context preservation system.

;;; Code:

(require 'magit-section)
(require 'ai-structs)
(require 'ai-response-processors)
(require 'ai-logging)
(require 'ai-common)

;; Face definitions for consistent styling
(defface ai-response-processors-debug-main-header
  '((t :inherit magit-section-heading :foreground "brown" :weight bold))
  "Face for main section headers."
  :group 'ai-debug)

(defface ai-response-processors-debug-category-header
  '((t :inherit magit-section-heading :foreground "brown"))
  "Face for category headers."
  :group 'ai-debug)

(defface ai-response-processors-debug-file-header
  '((t :inherit magit-section-secondary-heading))
  "Face for buffer headers."
  :group 'ai-debug)

(defface ai-response-processors-debug-metadata
  '((t :inherit magit-dimmed))
  "Face for metadata information."
  :group 'ai-debug)

(defface ai-response-processors-debug-empty
  '((t :inherit magit-dimmed :slant italic))
  "Face for empty or unavailable information."
  :group 'ai-debug)

(defface ai-response-processors-debug-context-type
  '((t :inherit magit-section-secondary-heading))
  "Face for context type labels."
  :group 'ai-debug)

(defvar ai-response-processors-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "r") 'ai-response-processors-debug-refresh)
    (define-key map (kbd "g") 'ai-response-processors-debug-refresh)
    map)
  "Keymap for AI response processors debug mode.")

(define-derived-mode ai-response-processors-debug-mode magit-section-mode "AI-Response-Debug"
  "Major mode for debugging AI response processors.

Key bindings:
\\{ai-response-processors-debug-mode-map}"
  :group 'ai
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun ai-response-processors-debug-refresh ()
  "Refresh the AI response processors debug buffer."
  (interactive)
  (when (eq major-mode 'ai-response-processors-debug-mode)
    (ai-response-processors-debug-show-buffers-status)))

(defun ai-response-processors-debug--get-all-response-buffers ()
  "Get list of all buffers with response context."
  (let ((response-buffers '()))
    (dolist (buffer (buffer-list))
      (when (ai-structs--response-buffer-context-p buffer)
        (push buffer response-buffers)))
    response-buffers))

(defun ai-response-processors-debug--format-buffer-info (buffer)
  "Format buffer information for BUFFER."
  (let* ((context (ai-structs--get-response-buffer-context buffer))
         (exec-context (when context (ai-structs--get-response-execution-context context)))
         (request-id (when exec-context (ai-structs--get-execution-context-request-id exec-context)))
         (model-info (when exec-context (ai-structs--get-execution-context-model exec-context)))
         (model-name (when model-info (plist-get model-info :name)))
         (timestamp (when context (ai-structs--get-response-timestamp context)))
         (buffer-size (with-current-buffer buffer (buffer-size)))
         (metadata-parts '()))

    ;; Build metadata for header
    (when model-name
      (push (format "model: %s" model-name) metadata-parts))
    (when request-id
      (push (format "id: %s" (substring request-id 0 8)) metadata-parts))
    (when timestamp
      (push (format "created: %s" (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)) metadata-parts))
    (push (format "size: %d chars" buffer-size) metadata-parts)

    (list
     :buffer buffer
     :context context
     :exec-context exec-context
     :metadata-parts metadata-parts)))

(defun ai-response-processors-debug--insert-original-context-section (exec-context)
  "Insert original context messages from EXEC-CONTEXT like in raw-structures-buffer."
  (let ((messages (when exec-context (ai-structs--get-execution-context-messages exec-context))))
    (magit-insert-section (ai-original-context nil t)
      (magit-insert-heading
        (propertize (format "ORIGINAL CONTEXT (%d messages)"
                           (if messages (length messages) 0))
                   'face 'ai-response-processors-debug-category-header))

      (if messages
          (let ((index 0))
            (dolist (struct messages)
              (when (and (listp struct) (keywordp (car struct)))
                (ai-response-processors-debug--insert-raw-typed-struct struct index))
              (setq index (1+ index))))
        (insert (propertize "  No messages in original context.\n"
                           'face 'ai-response-processors-debug-empty)))

      (insert "\n"))))

(defun ai-response-processors-debug--insert-raw-typed-struct (struct index)
  "Insert a single typed structure STRUCT at INDEX in raw format like ai-debug."
  (condition-case-unless-debug err
    (let* ((type (condition-case-unless-debug nil (plist-get struct :type) (error "unknown")))
           (source (condition-case-unless-debug nil (plist-get struct :source) (error nil)))
           (id (condition-case-unless-debug nil (plist-get struct :id) (error (format "id-%d" (random 10000)))))
           (content (condition-case-unless-debug nil (plist-get struct :content) (error nil)))
           (file (condition-case-unless-debug nil (plist-get struct :file) (error nil)))
           (relative-path (condition-case-unless-debug nil (plist-get struct :relative-path) (error nil)))
           (timestamp (condition-case-unless-debug nil (plist-get struct :timestamp) (error nil)))
           (metadata-parts '()))

      ;; Build metadata
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

      ;; Create section
      (magit-insert-section (ai-raw-struct (format "%d-%s" index id) t)
        (magit-insert-heading
          (propertize (format "[%d] %s" (1+ index) type)
                     'face 'ai-response-processors-debug-context-type)
          (when metadata-parts
            (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                   'face 'ai-response-processors-debug-metadata))))

        ;; Insert content without truncation
        (when content
          (let ((rendered-content (condition-case-unless-debug nil
                                    (ai-common--render-struct-to-string struct)
                                    (error content))))
            (ai-response-processors-debug--insert-content-without-truncation rendered-content)))

        (insert "\n")))
    (error
     (insert (propertize (format "[Error displaying structure %d: %s]\n\n" index (error-message-string err))
                        'face 'ai-response-processors-debug-empty)))))

(defun ai-response-processors-debug--insert-content-without-truncation (content)
  "Insert CONTENT without any truncation."
  (cond
   ;; String content
   ((stringp content)
    (insert (propertize content 'face 'default))
    (insert "\n"))

   ;; Plist content
   ((and (listp content) (keywordp (car content)))
    (let ((formatted-lines (ai-response-processors-debug--format-plist-as-table content)))
      (dolist (line formatted-lines)
        (insert (propertize line 'face 'default))
        (insert "\n"))))

   ;; List content
   ((listp content)
    (let ((item-index 0))
      (dolist (item content)
        (insert (propertize (format "  Item %d: " (1+ item-index)) 'face 'ai-response-processors-debug-metadata))
        (ai-response-processors-debug--insert-content-without-truncation item)
        (setq item-index (1+ item-index)))))

   ;; Other types
   (t
    (insert (propertize (format "  %s\n" content) 'face 'default)))))

(defun ai-response-processors-debug--format-plist-as-table (plist)
  "Format PLIST as a readable table without truncation."
  (condition-case-unless-debug nil
    (let ((result '()))
      (while plist
        (let ((key (car plist))
              (value (cadr plist)))
          (when (keywordp key)
            (let ((key-str (substring (symbol-name key) 1))
                  (value-str (ai-response-processors-debug--format-display-value value)))
              (push (format "%-20s: %s" key-str value-str) result)))
          (setq plist (cddr plist))))
      (reverse result))
    (error (list "Error formatting plist"))))

(defun ai-response-processors-debug--format-display-value (value)
  "Format VALUE for display without truncation."
  (cond
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   ((booleanp value) (if value "t" "nil"))
   ((and (listp value) (eq (car value) 'quote) (symbolp (cadr value)))
    (symbol-name (cadr value)))
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((listp value) (format "(%d items)" (length value)))
   ((vectorp value) (format "[%d items]" (length value)))
   (t (prin1-to-string value))))


(defun ai-response-processors-debug--insert-buffer-section (buffer-info)
  "Insert debug section for BUFFER-INFO."
  (let* ((buffer (plist-get buffer-info :buffer))
         (context (plist-get buffer-info :context))
         (exec-context (plist-get buffer-info :exec-context))
         (metadata-parts (plist-get buffer-info :metadata-parts))
         (buffer-name (buffer-name buffer))
         (response-content (when context (ai-response-buffer-context-response-content context)))
         (usage-stats (when context (ai-response-buffer-context-usage-stats context))))

    (magit-insert-section (ai-response-buffer buffer-name t)
      (magit-insert-heading
        (propertize buffer-name 'face 'ai-response-processors-debug-file-header)
        (when metadata-parts
          (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                 'face 'ai-response-processors-debug-metadata))))

      ;; Insert original context section
      (ai-response-processors-debug--insert-original-context-section exec-context)

      ;; Insert usage statistics section
      (ai-response-processors-debug--insert-usage-stats-section usage-stats)

      (insert "\n"))))

(defun ai-response-processors-debug-show-buffers-status ()
  "Show AI response processors buffer status in a dedicated buffer."
  (interactive)
  (let* ((buffer-name "*AI Response Processors Debug*")
         (buffer (get-buffer-create buffer-name))
         (response-buffers (ai-response-processors-debug--get-all-response-buffers)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ai-response-processors-debug-mode)

        ;; Set header-line-format with help information
        (setq header-line-format "AI Response Processors Debug - TAB to expand/collapse, r/g to refresh, q to quit")

        ;; Insert root section
        (magit-insert-section (ai-response-debug-root)
          (magit-insert-heading
            (propertize "AI RESPONSE PROCESSORS DEBUG" 'face 'ai-response-processors-debug-main-header))

          ;; Insert response buffer sections
          (if response-buffers
              (dolist (buffer response-buffers)
                (let ((buffer-info (ai-response-processors-debug--format-buffer-info buffer)))
                  (ai-response-processors-debug--insert-buffer-section buffer-info)))
            ;; No response buffers found
            (magit-insert-section (ai-no-buffers)
              (magit-insert-heading
                (propertize "NO RESPONSE BUFFERS FOUND" 'face 'ai-response-processors-debug-empty))
              (insert "    No AI response buffers with context are currently active.\n\n"))))

        ;; Collapse all buffer sections by default
        (goto-char (point-min))
        (condition-case-unless-debug nil
            (let ((root-section (magit-current-section)))
              (when root-section
                (dolist (child (oref root-section children))
                  (magit-section-hide child))))
          (error nil))

        (goto-char (point-min))))

    ;; Show buffer
    (pop-to-buffer buffer)))

(provide 'ai-response-processors-debug)

;;; ai-response-processors-debug.el ends here
