;;; ai-completions.el --- Code autocompletion via AI -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: help, tools


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
;; The package allows for text completion using various AI engines.
;;

;;; Code:
(require 'ai-openai-completions)

(defgroup ai-completions nil
  "AI code completion tool for Emacs."
  :group 'ai-mode)


(defcustom ai-completions--context-size 1
  "Number of lines for context."
  :type 'integer
  :group 'ai-chat)


(defcustom ai-completions--current-backend 'ai-openai-completions--completion-backend
  "Active completion backend."
  :group 'ai-completions)


(defcustom ai-completions--backends
  '(("OpenAI completions" . ai-openai-completions--completion-backend))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-completions)


(defcustom ai-completions--continue-commands '(not save-buffer save-some-buffers
                                                  save-buffers-kill-terminal
                                                  save-buffers-kill-emacs)
  "A list of commands that are allowed during completion."

  :type '(choice (const :tag "Any command" t)
                 (cons  :tag "Any except"
                        (const not)
                        (repeat :tag "Commands" function))
                 (repeat :tag "Commands" function))
  :group 'ai-completions)


(defcustom ai-completions--abort-commands '(not ai-completions--select-next-or-abort
                                               ai-completions--select-prev-or-abort
                                               ai-completions--select-current
                                               ignore
                                               switch-to-buffer
                                               other-window
                                               other-buffer)
  "A list of commands that abort completion."
  :type '(choice (const :tag "Any command" t)
                 (cons  :tag "Any except"
                        (const not)
                        (repeat :tag "Commands" function))
                 (repeat :tag "Commands" function))
  :group 'ai-completions)


(defvar-local ai-completions--candidates '())
(defvar-local ai-completions--current-candidate 0)

(defvar-local ai-completions--active nil)

(defvar ai-completions--emulation-alist '((t . nil)))


(defvar-local ai-completions--preview-overlay nil)
(defvar-local ai-completions--pseudo-tooltip-overlay nil)
(defvar-local ai-completions--last-point nil)
(defvar-local ai-completions--activated-keymap nil)


(defvar ai-completions--mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\e\e\e" 'ai-completions--abort)
    (define-key keymap "\C-g" 'ai-completions--abort)
    (define-key keymap (kbd "C-n") 'ai-completions--select-next-or-abort)
    (define-key keymap (kbd "C-p") 'ai-completions--select-prev-or-abort)
    (define-key keymap (kbd "<down>") 'ai-completions--select-next-or-abort)
    (define-key keymap (kbd "<up>") 'ai-completions--select-prev-or-abort)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [return] 'ai-completions--select-current)
    (define-key keymap (kbd "RET") 'ai-completions--select-current)
    (define-key keymap (kbd "C-<tab>") 'ai-completions--select-next-or-abort)
    keymap)

  "Keymap used for manage complete candidates.")


(defun ai-completions--coordinator ()
  "Decides whether to continue the previous process of supplementation or to start a new one."
  (condition-case-unless-debug err
      (progn
        (if (or (ai-completions--is-new-completion-required)
                (not (ai-completions--should-continue this-command)))
            (ai-completions--begin)
          (ai-completions--continue)))
    (error (message (format "AI completion: An error occured in ai-completions--coordinator => %s" (error-message-string err)))
           (ai-completions--cancel))))


(defun ai-completions--begin ()
  "Start new completion."
  (ai-completions--cancel)
  (setq ai-completions--last-point
        (if (region-active-p)
            (region-end)
          (point))
        ai-completions--active t)

  (condition-case-unless-debug err
      (progn (ai-completions-mode 1)
             (ai-completions--update-candidates (current-buffer))
             ;; (ai-completions--show-candidate)
             )
    (error (message (format "AI completion: An error occured in ai-completions--begin => %s" (error-message-string err)))
           (ai-completions--cancel))))

(defun ai-completions--continue ()
  "Continue completion."
  (condition-case-unless-debug err
      (progn )
    (error (message (format "AI completion : An error occured in ai-completions--continue => %s" (error-message-string err)))
           (ai-completions--cancel))))

(defun ai-completions--show-candidate ()
  "Show completion candidate."
  (ai-completions--update-preview)
  (ai-completions--prepare-keymap)
  (ai-completions--activate-keymap))

(defun ai-completions--should-continue (command)
  "Check if it is possible to proceed with completion for COMMAND."
  (or (eq t ai-completions--continue-commands)
      (if (eq 'not (car ai-completions--abort-commands))
          (consp (memq command (cdr ai-completions--abort-commands)))
        (or (memq command ai-completions--abort-commands)
            (and (symbolp command)
                 (string-match-p "\\`ai-" (symbol-name command)))))))


(defun ai-completions--is-new-completion-required ()
  "Check if it is necessary to start a new completion process."
  (or (not (equal (point) ai-completions--last-point))
      (not ai-completions--active)))


(defun ai-completions--get-text-to-complete ()
  "Get text to complete."
  (if (region-active-p)
      (ai-utils--get-region-content)

    (let* ((current (point))
           (min-point (- current ai-completions--context-size))
           (context-beginning-point (line-beginning-position (* -1 ai-completions--context-size))))
      (buffer-substring-no-properties context-beginning-point current))))


(defun ai-completions--update-candidates (buffer)
  "Update the list of candidates for BUFFER."
  (funcall ai-completions--current-backend
           (ai-completions--get-text-to-complete)
           (ai-completions--get-buffer-type-or-language buffer)
           (lambda (candidates)
             (progn
               (with-current-buffer buffer
                 (ai-completions--add-candidates candidates)
                 (ai-completions--show-candidate))))))

(defun ai-completions--update-preview ()
  "Show a preview with the current candidate."
  (ai-completions--preview-show-at-point ai-completions--last-point (nth ai-completions--current-candidate ai-completions--candidates)))

(defun ai-completions--add-candidates (candidates)
  "Add a CANDIDATES to the list of `ai-completions--candidates`."
  (setq ai-completions--candidates (append ai-completions--candidates candidates)
        ai-completions--current-candidate (- (length ai-completions--candidates) (length candidates))))

(defun ai-completions--preview-hide ()
  "Hide preview with current candidate."
  (when ai-completions--preview-overlay
    (delete-overlay ai-completions--preview-overlay)
    (setq ai-completions--preview-overlay nil)))


(defun ai-completions--abort ()
  "Aborf the supplementing process."
  (interactive)
  (ai-completions--cancel))


(defun ai-completions-finish (candidate)
  "Use CANDIDATE and complete the completion process."
  (ai-completions--preview-hide)
  (ai-completions--insert-candidate candidate)
  (ai-completions--cancel))

(defun ai-completions--cancel ()
  "Cancel the completion process."
  (ai-completions--preview-hide)
  (setq ai-completions--current-candidate 0
        ai-completions--candidates '()
        ai-completions--last-point nil
        ai-completions--active nil)
  (ai-completions--destroy-keymap)
  (ai-completions-mode 0)
  ;; TODO: run hooks
  nil)


(defun ai-completions--insert-candidate (candidate)
  "Use CANDIDATE."
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (ai-utils--insert-completion candidate)))


(defun ai-completions--select-current ()
  "Insert the selected candidate."
  (interactive)
  (let ((result (nth ai-completions--current-candidate ai-completions--candidates)))
    (ai-completions-finish result)))


(defun ai-completions--select-next-or-abort ()
  "Show the next candidate."
  (interactive)
  (let ((next-candidate-index (+ ai-completions--current-candidate 1)))
    (cond
     ((>= next-candidate-index (length ai-completions--candidates))
      (ai-completions--update-candidates (current-buffer)))
     (t (progn
          (setq ai-completions--current-candidate next-candidate-index)
          (ai-completions--show-candidate))))))

(defun ai-completions--select-prev-or-abort ()
  "Show the previous candidate."
  (interactive)
  (let ((prev-candidate-index (- ai-completions--current-candidate 1)))
    (cond
     ((>= prev-candidate-index 0)
      (let ((result (nth prev-candidate-index ai-completions--candidates)))
        (setq ai-completions--current-candidate prev-candidate-index)
        (ai-completions--show-candidate)))
     (t (message "completion candidate already selected")))))


(define-minor-mode ai-completions-mode
  "AI completion mode."
  :lighter "ai complete mode lighter"
  (if ai-completions-mode
      (progn
        (message "AI completion mode enabled")
        (add-hook 'pre-command-hook 'ai-completions-mode-pre-command)
        (add-hook 'post-command-hook 'ai-completions-mode-post-command))
    (remove-hook 'pre-command-hook 'ai-completions-mode-pre-command)
    (remove-hook 'post-command-hook 'ai-completions-mode-post-command)
    (message "AI completion mode disabled")))


(defun ai-completions-mode-pre-command ()
  "Function called before executing a command."
  (if (not (ai-completions--should-continue this-command))
      (progn
        (ai-completions--abort))
    (ai-completions--deactivate-keymap)))


(defun ai-completions-mode-post-command ()
  "Function called after executing a command."
  (when ai-completions--active
    (ai-completions--activate-keymap)))


(defun ai-completions--prepare-keymap ()
  "Prepare keymap."
  (ai-completions-ensure-emulation-alist)
  (ai-completions--enable-overriding-keymap ai-completions--mode-keymap))


(defun ai-completions--destroy-keymap ()
  "Destroy keymap."
  (ai-completions--deactivate-keymap)
  (ai-completions--enable-overriding-keymap nil))


(defun ai-completions--enable-overriding-keymap (keymap)
  "Activate KEYMAP."
  (ai-completions--deactivate-keymap)

  (setf (cdar ai-completions--emulation-alist) nil)
  (setq ai-completions--activated-keymap keymap))


(defun ai-completions--deactivate-keymap ()
  "Deactivate keymap."
  (setf (cdar ai-completions--emulation-alist) nil))


(defun ai-completions--activate-keymap ()
  "Activate keymap."
  (unless (or (cdar ai-completions--emulation-alist)
              (null ai-completions--activated-keymap))
    (setf (cdar ai-completions--emulation-alist) ai-completions--activated-keymap)))


(defun ai-completions-ensure-emulation-alist ()
  "Ensure emulation alist."
  (unless (eq 'ai-completions--emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'ai-completions--emulation-alist
                (delq 'ai-completions--emulation-alist emulation-mode-map-alists)))))


(defface ai-completions--preview-face
  '((((class color) (min-colors 88) (background light))
     (:background "light blue"))
    (((class color) (min-colors 88) (background dark))
     (:background "gray31"))
    (t (:background "green"))
    (((class color) (min-colors 88) (background light))
     (:foreground "black" :background "cornsilk"))
    (((class color) (min-colors 88) (background dark))
     (:background "gray26"))
    (t (:foreground "black" :background "yellow")))
  "Face used for the selection in the tooltip.")


(defun ai-completions--preview-show-at-point (pos completion)
  "Show COMPLETION preview at POS."
  (ai-completions--preview-hide)

  (let* ((completion completion))

    (add-face-text-property 0 (length completion) 'ai-completions--preview-face nil completion)

    (and (equal pos (point))
         (not (equal completion ""))
         (add-text-properties 0 1 '(cursor 1) completion))

    (let* ((beg pos)
           (pto ai-completions--pseudo-tooltip-overlay)
           (ptf-workaround (and
                            pto
                            (char-before pos)
                            (eq pos (overlay-start pto)))))
      ;; Try to accommodate for the pseudo-tooltip overlay,
      ;; which may start at the same position if it's at eol.
      (when ptf-workaround
        (cl-decf beg)
        (setq completion (concat (buffer-substring beg pos) completion)))

      (setq ai-completions--preview-overlay (make-overlay beg pos))

      (let ((ov ai-completions--preview-overlay))
        (overlay-put ov (if ptf-workaround 'display 'after-string)
                     completion)
        (overlay-put ov 'window (selected-window))))))


(defun ai-completions-change-backend ()
  "Change completion backend."
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ai-completions--backends))))
    (setq ai-completions--current-backend (cdr (assoc value ai-completions--backends)))
    (message (format "AI query backend changed to \"%s\"" value))))


(defun ai-completions--get-buffer-type-or-language (buffer)
  "Get programming language for BUFFER."
  (with-current-buffer buffer
    (file-name-extension (buffer-file-name buffer))))


(provide 'ai-completions)

;;; ai-completions.el ends here
