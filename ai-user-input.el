;;; ai-user-input.el --- User input utilities for AI Mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, ai

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
;; This file provides user input utilities for AI mode, including various
;; methods for collecting multiline input from users.

;;; Code:

(require 'cl-lib)

(defun ai-user-input-multiline-buffer ()
  "Prompt the user for multiline input in a temporary buffer and return it as a string."
  (interactive)
  (let ((original-buffer (current-buffer))
        (original-window (selected-window))
        (original-point (point))
        (buffer (get-buffer-create "*Multiline Input*")))
    ;; Setup the temporary buffer
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (local-set-key (kbd "C-c C-c") #'exit-recursive-edit))
    ;; Display to user
    (pop-to-buffer buffer)
    (message "Type your text and press C-c C-c to finish. Press C-g to cancel.")
    ;; Handle input and cancel
    (condition-case-unless-debug nil
        (progn
          (recursive-edit) ;; Wait for completion
          (let ((input-text (with-current-buffer buffer
                              (buffer-substring-no-properties (point-min) (point-max)))))
            (kill-buffer buffer)
            (select-window original-window)
            (with-current-buffer original-buffer
              (goto-char original-point))
            input-text))
      (quit  ;; Handle C-g
       (kill-buffer buffer)
       (select-window original-window)
       (with-current-buffer original-buffer
         (goto-char original-point))
       (message "Input cancelled.")
       nil))))

(defun ai-user-input-minibuffer-with-preview ()
  "Get user input from minibuffer with dynamic resizing and multiline support.
Returns the input string or nil if cancelled.
Features:
- Dynamic minibuffer resizing up to 50% of screen height
- Visual line wrapping for long lines
- Empty line finishes input
- C-g cancels input"
  (let ((input "")
        (continue t)
        (max-mini-window-height 0.5))  ; Allow minibuffer to use up to 50% of screen
    (while continue
      (let* ((prompt (if (string-empty-p input)
                        "Enter your text (empty line to finish): "
                      (format "Current input:\n%s\n\nContinue (empty line to finish): " input)))
             (line (condition-case-unless-debug nil
                       (read-string prompt nil nil nil t)  ; Enable history
                     (quit
                      (setq continue nil)
                      nil))))
        (cond
         ;; User cancelled
         ((not continue)
          (setq input nil))
         ;; Empty line finishes input
         ((string-empty-p line)
          (setq continue nil))
         ;; Add line to input
         (t
          (setq input (if (string-empty-p input)
                          line
                        (concat input "\n" line)))))))
    input))

(defun ai-user-input-ctrl-enter ()
  "Get user input using minibuffer with C-RET to finish.
Features:
- C-RET (C-<return>) sends message and finishes input
- C-g cancels input
- RET creates new line
Returns the input string or nil if cancelled."
  (let ((input "")
        (continue t)
        (keymap (copy-keymap minibuffer-local-map)))
    ;; Set up custom keymap for minibuffer
    (define-key keymap (kbd "C-<return>")
                (lambda ()
                  (interactive)
                  (setq continue nil)
                  (exit-minibuffer)))
    (define-key keymap (kbd "RET")
                (lambda ()
                  (interactive)
                  (insert "\n")))

    ;; Get input using minibuffer with custom keymap
    (condition-case-unless-debug nil
        (progn
          (setq input (read-from-minibuffer
                       "Type your message (C-RET to send, C-g to cancel): "
                       nil keymap nil nil nil t))
          input)
      (quit
       (message "Input cancelled.")
       nil))))

(provide 'ai-user-input)

;;; ai-user-input.el ends here
