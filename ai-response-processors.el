;;; ai-response-processors.el --- AI response processing for AI mode -*- lexical-binding: t -*-

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
;; This module contains functions for processing and handling responses from AI models,
;; including text extraction, formatting, display, and callback creation for various
;; response insertion and replacement strategies.

;;; Code:

(require 'cl-lib)
(require 'ai-utils) ; For dependencies that remain in ai-utils, e.g., `ai-utils--replace-or-insert`
(require 'ai-common) ; For `ai-common--make-file-summary-struct`, `ai-common--get-text-content-from-struct`
(require 'ai-structs) ; For ai-buffer-state functions and ai-response-buffer-context
(require 'ai-logging) ; For logging functionality

;; Configuration variables
(defcustom ai-response-processors-create-unique-buffers t
  "Whether to create unique response buffers for each request.
When t, each AI response gets its own buffer with preserved context.
When nil, reuse the same response buffer (legacy behavior)."
  :type 'boolean
  :group 'ai-response-processors)

(defvar ai-response-processors--explanation-buffer-name "*AI explanation*"
  "Name of the buffer used for explanations.")

(defvar ai-response-processors--response-buffer-name "*AI response*"
  "Name of the buffer used to display AI responses.")

(defvar ai-response-processors--buffer-counter 0
  "Counter for generating unique buffer names.")

;; Keymap and minor mode for response buffers
(defvar ai-response-processors-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'ai-response-processors-regenerate-response)
    map)
  "Keymap for AI response processor buffers.")

(define-minor-mode ai-response-processors-mode
  "Minor mode for AI response buffers with context."
  :lighter " AI-Resp"
  :keymap ai-response-processors-mode-map
  (when ai-response-processors-mode
    (setq buffer-read-only t)))

;; Core helper functions
(defun ai-response-processors--get-explaination-help-buffer ()
  "Return the buffer used for explanations."
  (get-buffer-create ai-response-processors--explanation-buffer-name))

(defun ai-response-processors--get-response-buffer ()
  "Return the buffer used for displaying AI responses."
  (get-buffer-create ai-response-processors--response-buffer-name))

(defun ai-response-processors--generate-unique-buffer-name (base-name)
  "Generate a unique buffer name based on BASE-NAME with timestamp or counter."
  (if ai-response-processors-create-unique-buffers
      (let ((timestamp (format-time-string "%H:%M:%S"))
            (counter (setq ai-response-processors--buffer-counter
                          (1+ ai-response-processors--buffer-counter))))
        (format "*AI response %s #%d*" timestamp counter))
    base-name))

(defun ai-response-processors--create-unique-response-buffer (execution-context messages usage-stats)
  "Create a unique response buffer with preserved context information."
  (let* ((content (ai-response-processors--extract-content-from-messages messages))
         (buffer-name (ai-response-processors--generate-unique-buffer-name
                      ai-response-processors--response-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (response-context (ai-structs--create-response-buffer-context
                            execution-context content usage-stats)))

    (ai-logging--message 'debug "response-processors" "Created response buffer: %s, content length: %d"
                        buffer-name (if content (length content) 0))

    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)

        ;; Enable markdown mode if available
        (when (fboundp 'markdown-mode)
          (markdown-mode))

        ;; Store context using ai-response-buffer-context structure
        (ai-structs--set-response-buffer-context response-context buffer)

        ;; Enable response processor mode
        (ai-response-processors-mode 1)))

    buffer))

(defun ai-response-processors--setup-legacy-response-buffer (messages)
  "Setup legacy response buffer with content from MESSAGES."
  (let ((buffer (ai-response-processors--get-response-buffer))
        (content (ai-response-processors--extract-content-from-messages messages)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (when (fboundp 'markdown-mode)
          (markdown-mode))))
    buffer))

(defun ai-response-processors--display-response-buffer (buffer unique-mode-p)
  "Display response BUFFER with appropriate method based on UNIQUE-MODE-P.
For unique buffers, use pop-to-buffer. For legacy mode, use with-help-window."
  (if unique-mode-p
      (progn
        (pop-to-buffer buffer)
        (with-current-buffer buffer
          (goto-char (point-min))))
    (with-help-window buffer
      (with-current-buffer buffer
        (when (fboundp 'markdown-mode)
          (markdown-mode))))))

(cl-defun ai-response-processors--get-message (messages &optional (message-id 0))
  "Retrieve a message from MESSAGES by its MESSAGE-ID, defaulting to the first message."
  (when (and (listp messages) (> (length messages) 0))
    (if (>= message-id 0)
        (elt messages message-id)
      (car messages))))

(defun ai-response-processors--extract-content-from-messages (messages)
  "Extract the content from MESSAGES."
  (let ((message-element (ai-response-processors--get-message messages)))
    (let ((content (ai-common--get-text-content-from-struct message-element)))
      ;; Debug logging
      (ai-logging--message 'debug "response-processors" "Extracted content length: %d"
                          (if content (length content) 0))
      content)))

(cl-defun ai-response-processors--with-current-buffer-callback (callback &optional (trim t))
  "Create a CALLBACK function that will operate in the context of the current buffer.

If TRIM is non-nil, trims the content passed to CALLBACK."
  (let ((buffer (current-buffer)))
    (lambda (execution-context messages &optional usage-stats)
      (let ((content (ai-response-processors--extract-content-from-messages messages)))
        (with-current-buffer buffer
          (funcall callback execution-context (if trim (string-trim-left content) content)))))))

(cl-defun ai-response-processors--replace-region-or-insert-in-current-buffer (&optional (trim t) insert-mode)
  "Create a function to replace the active region or insert into the current buffer.

If a region is active, replaces only the selected part.
If no region is active and INSERT-MODE is nil, replaces the entire buffer.
If no region is active and INSERT-MODE is non-nil, inserts at cursor position.
If TRIM is non-nil, trims the content passed to the function."
  (let* ((buffer (current-buffer))
         (region-active (region-active-p))
         (cursor-pos (point))
         (beginning (cond (region-active (region-beginning))
                         (insert-mode nil)
                         (t (point-min))))
         (end (cond (region-active (region-end))
                   (insert-mode nil)
                   (t (point-max)))))
    (lambda (execution-context messages &optional usage-stats)
      (let* ((raw-content (ai-response-processors--extract-content-from-messages messages))
            (content (if trim (string-trim-left raw-content) raw-content)))
        (when (string-empty-p content)
          (user-error "No content to insert or replace"))
        (with-current-buffer buffer
          (when region-active
            (deactivate-mark))
          (cond
           ;; Insert mode: insert at cursor position
           ((and (not region-active) insert-mode)
            (goto-char cursor-pos)
            (insert content))
           ;; Replace mode: replace region or entire buffer
           (t
            (ai-utils--replace-or-insert content beginning end))))))))

(defun ai-response-processors--create-insert-at-point-callback (target-buffer cursor-position)
  "Create a callback that inserts AI response at CURSOR-POSITION in TARGET-BUFFER."
  (lambda (execution-context messages &optional usage-stats)
    (when (buffer-live-p target-buffer)
      (let ((content (ai-response-processors--extract-content-from-messages messages)))
        (when (and content (not (string-empty-p content)))
          (with-current-buffer target-buffer
            (save-excursion
              (goto-char cursor-position)
              (insert (string-trim-left content)))))))))

(cl-defun ai-response-processors--with-current-buffer-tagged-callback (callback tag &optional (trim t))
  "Create a CALLBACK function for inserting tagged content in the current buffer.

TAG is a marker for placing content passed to the CALLBACK function.
If TRIM is non-nil, trims content passed to the CALLBACK."
  (let ((buffer (current-buffer)))
    (lambda (execution-context messages &optional usage-stats)
      (let ((content (ai-response-processors--extract-content-from-messages messages)))
        (with-current-buffer buffer
          (funcall callback execution-context tag (if trim (string-trim-left content) content)))))))

(defun ai-response-processors--show-explain-help-buffer (text)
  "Display TEXT in the explanation buffer."
  (save-excursion
    (with-help-window (ai-response-processors--get-explaination-help-buffer)
      (princ "AI Explanation below: \n")
      (princ text)
      (switch-to-buffer (ai-response-processors--get-explaination-help-buffer)))))

(defun ai-response-processors--show-response-buffer (execution-context messages &optional usage-stats)
  "Display MESSAGES content in response buffer with context preservation.

If `ai-response-processors-create-unique-buffers` is t, creates a new buffer
for each response with preserved context. Otherwise uses existing behavior."
  (ai-logging--message 'debug "response-processors"
                      "Starting show-response-buffer: unique-buffers=%s, context-type=%s, messages-count=%d"
                      ai-response-processors-create-unique-buffers
                      (type-of execution-context)
                      (if (listp messages) (length messages) 0))

  (condition-case-unless-debug err
      (let ((buffer (if ai-response-processors-create-unique-buffers
                        (ai-response-processors--create-unique-response-buffer
                         execution-context messages usage-stats)
                      (ai-response-processors--setup-legacy-response-buffer messages))))

        (ai-response-processors--display-response-buffer
         buffer ai-response-processors-create-unique-buffers)

        (ai-logging--message 'debug "response-processors"
                            "Show-response-buffer completed: buffer=%s, content-preview=%s"
                            (buffer-name buffer)
                            (with-current-buffer buffer
                              (substring (buffer-string) 0 (min 50 (buffer-size))))))
    (error
     (ai-logging--message 'error "response-processors" "Error in show-response-buffer: %s" err)
     (signal (car err) (cdr err)))))

(defun ai-response-processors--show-and-eval-response (execution-context messages &optional usage-stats)
  "Show MESSAGES in a buffer and ask user for permission to evaluate the Emacs Lisp code."
  (let* ((content (ai-response-processors--extract-content-from-messages messages))
         (buffer-name "*AI Generated Code*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (emacs-lisp-mode)
      (insert content)
      (goto-char (point-min))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure)))

    (pop-to-buffer buffer)

    (when (yes-or-no-p "The AI has generated Emacs Lisp code. Do you want to evaluate it? ")
      (condition-case-unless-debug err
          (progn
            (eval-buffer buffer)
            (message "Code evaluated successfully."))
        (error
         (message "Error evaluating code: %s" (error-message-string err)))))))

(defun ai-response-processors--apply-patch-to-buffer (patch-content)
  "Apply PATCH-CONTENT to the current buffer using unified patch format."
  (let ((patch-file (make-temp-file "ai-patch" nil ".patch"))
        (original-buffer (current-buffer))
        (original-file (buffer-file-name))
        (result-code nil))
    (unwind-protect
        (progn
          ;; Write patch content to temporary file
          (with-temp-file patch-file
            (insert patch-content))

          ;; Save buffer before applying patch if it has a file
          (when (and original-file (buffer-modified-p))
            (save-buffer))

          ;; Apply patch to the buffer
          (if original-file
              ;; For file-backed buffers, apply patch to file
              (progn
                (setq result-code (call-process "patch" nil "*AI Patch Output*" t
                                                "-p1" "--forward" "--force"
                                                original-file patch-file))
                (if (= result-code 0)
                    (progn
                      (revert-buffer t t t)
                      (message "Patch applied successfully"))
                  (error "Failed to apply patch. Exit code: %d. Check *AI Patch Output* buffer" result-code)))
            ;; For non-file buffers, create temporary file and apply patch
            (let ((temp-file (make-temp-file "ai-buffer" nil ".tmp")))
              (unwind-protect
                  (progn
                    ;; Write buffer content to temp file
                    (write-region (point-min) (point-max) temp-file)
                    ;; Apply patch to temp file
                    (setq result-code (call-process "patch" nil "*AI Patch Output*" t
                                                    "-p1" "--forward" "--force"
                                                    temp-file patch-file))
                    (if (= result-code 0)
                        (progn
                          ;; Replace buffer content with patched content
                          (erase-buffer)
                          (insert-file-contents temp-file)
                          (message "Patch applied successfully to buffer"))
                      (error "Failed to apply patch. Exit code: %d. Check *AI Patch Output* buffer" result-code)))
                (when (file-exists-p temp-file)
                  (delete-file temp-file))))))
      ;; Cleanup
      (when (file-exists-p patch-file)
        (delete-file patch-file)))))

(defun ai-response-processors--create-patch-apply-callback (original-buffer)
  "Create a callback that applies patch content to ORIGINAL-BUFFER."
  (lambda (execution-context messages &optional usage-stats)
    (let ((response-content (ai-response-processors--extract-content-from-messages messages)))
      (if response-content
          (with-current-buffer original-buffer
            (condition-case err
                (ai-response-processors--apply-patch-to-buffer response-content)
              (error
               (message "Failed to apply patch: %s" (error-message-string err))
               (ai-response-processors--show-response-buffer execution-context messages usage-stats))))
        (message "No patch content received from AI")))))

(defun ai-response-processors--create-smart-replace-callback (target-buffer)
  "Create a callback for smart replacement based on `ai-execution--replace-action-use-patch`.
Returns a callback that takes EXECUTION-CONTEXT, MESSAGES and USAGE-STATS and applies the content
to TARGET-BUFFER, either by patching or by direct replacement/insertion."
  (if (bound-and-true-p ai-execution--replace-action-use-patch)
      (ai-response-processors--create-patch-apply-callback target-buffer)
    (lambda (execution-context messages &optional usage-stats)
      (with-current-buffer target-buffer
        (funcall (ai-response-processors--replace-region-or-insert-in-current-buffer)
                 execution-context
                 messages
                 usage-stats)))))

;; Public API functions

;; Interactive functions for response buffers
(defun ai-response-processors-regenerate-response ()
  "Regenerate response using the same context."
  (interactive)
  (let ((response-context (ai-structs--get-response-buffer-context)))
    (if response-context
        (message "Response regeneration not yet implemented")
      (message "No context available for regeneration"))))

(defun ai-response-processors--create-error-message-callback (&optional error-prefix)
  "Create a callback that prints error messages to *Messages* buffer.
ERROR-PREFIX is an optional prefix to add before the error message."
  (lambda (execution-context messages &optional usage-stats)
    (let* ((content (ai-response-processors--extract-content-from-messages messages))
           (prefix (or error-prefix "AI Error"))
           (error-message (format "%s: %s" prefix content)))
      (ai-logging--message 'error "response-processors" "%s" error-message)
      (message "%s" error-message))))

(provide 'ai-response-processors)

;;; ai-response-processors.el ends here
