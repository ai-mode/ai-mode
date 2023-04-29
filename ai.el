;;; ai.el --- AI interaction mode for Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai, chatgpt, gpt


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
;; This package defines a set of functions and variables for interation
;; with AI engines.
;; Currently available:
;; - Code completion
;; - Code optimization
;; - Code fixing
;; - Code documentation
;; - Code improvement
;; - Code elaboration
;; - Code explanation

;;; Code:

(require 'url)
(require 'json)
(require 'cl-macs)


(require 'ai-utils)
(require 'ai-openai)
(require 'ai-openai-completions)
(require 'ai-openai-chatgpt)
(require 'ai-completions)
(require 'ai-chat)

;; (require 'uuid)

(defgroup ai nil
  "Support for AI interactions."
  :prefix "ai-"
  :group 'emacs
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))


(defgroup ai-mode nil
  "Use AI or AGI API."
  :prefix "ai-mode"
  :group 'ai-mode
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defcustom ai-keymap-prefix "C-c i"
  "AI mode keymap prefix."
  :group 'ai
  :type 'string
  )

(defvar url-http-end-of-headers)

(defvar ai--explain-buffer-name "*AI explanation*")
(defvar ai--response-buffer-name "*AI response*")
(defvar ai--explain-text-orig-buffer nil
  "")

(defcustom ai--query-type-prompt "Type of Query: "
  ""
  :type 'string
  :group 'ai-mode)


(defcustom ai--doc-tag "{{__ai_doc__}}"
  ""
  :type 'string
  :group 'ai-mode)

(defcustom ai-explanation-backend 'ai--openai--chat-explain-text
  ""
  :group 'ai-mode
  )

(defcustom ai-async-query-backend 'ai--openai--chat-explain-text
  ""
  :group 'ai-mode
  )


(defvar ai-command-map
  (let ((keymap (make-sparse-keymap)))

    (define-key keymap (kbd "<tab>") 'ai-insert-doc-marker)
    (define-key keymap (kbd "o") 'ai-optimize-code-region)
    (define-key keymap (kbd "i") 'ai-improve-code-region)
    (define-key keymap (kbd "f") 'ai-fix-code-region)
    (define-key keymap (kbd "d") 'ai-document-code-region)
    (define-key keymap (kbd "e b") 'ai-elaborate-code-region)
    (define-key keymap (kbd "s") 'ai-spellcheck-code-region)
    (define-key keymap (kbd "e") 'ai-explain-code-region)
    keymap)
  "Keymap for ai commands."
  )

(defvar ai-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when ai-keymap-prefix
      (define-key keymap (kbd ai-keymap-prefix) ai-command-map)
      )
    keymap)
  "Keymap used by `ai-mode`."
  )


(define-minor-mode ai-mode
  "Mode for AI interaction."
  :keymap ai-mode-map
  :lighter " AI mode"
  :group 'ai
  (if ai-mode
      (progn
        (add-hook 'pre-command-hook 'ai-mode-pre-command)
        (add-hook 'post-command-hook 'ai-mode-post-command)
        )
    (remove-hook 'pre-command-hook 'ai-mode-pre-command)
    (remove-hook 'post-command-hook 'ai-mode-post-command)
    )
  )


(define-globalized-minor-mode global-ai-mode ai-mode ai-mode-on
  :group 'ai
  )

(defun ai-mode-on ()
  ""
  (ai-mode 1)
  )

(defun ai-mode-pre-command ()
  ""
  )

(defun ai-mode-post-command ()
  ""
  )


(defun ai-complete-code-at-point ()
  ""
  (interactive)
  (ai--completion-coordinator)
  )

(defun ai-insert-doc-marker ()
  ""
  (interactive)

  (with-current-buffer (current-buffer)
    (insert ai--doc-tag)
  ) )

(cl-defun ai--with-current-buffer-callback (callback &optional (trim t))
  ""
  (let ((buffer (current-buffer)))
    (lambda (success-response)
      (with-current-buffer buffer
        (if trim
            (funcall callback (string-trim-left (ai--openai--completions-get-choice success-response)))
          (funcall callback (ai--openai--completions-get-choice success-response))
          )
        ))))

(cl-defun ai--with-current-buffer-tagged-callback (callback tag &optional (trim t))
  ""
  (let ((buffer (current-buffer)))
    (lambda (success-response)
      (with-current-buffer buffer
        (if trim
            (funcall callback tag (string-trim-left (ai--openai--completions-get-choice success-response)))
          (funcall callback tag (ai--openai--completions-get-choice success-response)))))))

(defun ai-optimize-code-region ()
  ""
  (interactive)
  (ai--openai--completions-async-query-by-type "optimize" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  )

(defun ai-improve-code-region ()
  ""
  (interactive)
  (ai--openai--completions-async-query-by-type "improve" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  )

(defun ai-fix-code-region ()
  ""
  (interactive)
  (ai--openai--completions-async-query-by-type "fix" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  )

(defun ai-document-code-region ()
  ""
  (interactive)
  (ai--openai--completions-async-query-by-type "document" (ai--get-region-content) ( ai--with-current-buffer-tagged-callback ai--doc-tag 'ai--replace-tag-in-region))
  )

(defun ai-elaborate-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "elaborate" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  ;; (ai--openai--completions-async-query-by-type "elaborate" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  )

(defun ai-spellcheck-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "spellcheck" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  ;; (ai--openai--completions-async-query-by-type "spellcheck" (ai--get-region-content) (ai--with-current-buffer-callback 'ai--replace-region))
  )


(defun ai-explain-code-region ()
  ""
  (interactive)
  (ai--explain-text (ai--get-region-content))
  )

(defun ai--get-explain-help-buffer ()
  ""
  (get-buffer-create ai--explain-buffer-name)
)

(defun ai--get-response-buffer ()
  ""
  (get-buffer-create ai--response-buffer-name)
  )


(defun ai--show-explain-help-buffer (text)
  ""
  (save-excursion
    (with-help-window (ai--get-explain-help-buffer)
      (princ "AI Explanation below: \n")
      (princ text)
      (switch-to-buffer (ai--get-explain-help-buffer))
      )
    )
  )

(defun ai--show-response-buffer (text)
  ""
  (save-excursion
    (with-help-window (ai--get-response-buffer)
      (princ text)
      (switch-to-buffer (ai--get-response-buffer))
      )
    )
  )

(defun ai--explain-text (text)
  ""
  (funcall ai-explanation-backend text 'ai--show-explain-help-buffer))


(defun ai--async-query-by-type (query-type input callback)
  ""
  (let ((query  (if-let (format-string (cdr (assoc query-type ai--openai--query-type-map)))
                    (format format-string query)
                  (format "%s\n\n%s" query-type query))))
    (funcall ai-async-query-backend query callback)
    )
  )


(defun ai-show ()
  ""
  (interactive)
  (ai--region-or-buffer-query-by-type
   (lambda (success-response)
     (ai--show-response-buffer (ai--openai--completions-get-choice success-response)))))


(provide 'ai)

;;; ai.el ends here
