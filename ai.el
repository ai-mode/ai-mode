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
;; - Chatting with AI

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

(defcustom ai--change-backend-prompt "Select query backend: "
  ""
  :type 'string
  :group 'ai-mode)


(defcustom ai--doc-tag "{{__ai_doc__}}"
  ""
  :type 'string
  :group 'ai-mode)

(defcustom ai-async-query-backend 'ai--openai--chat-async-send-query
  ""
  :group 'ai-mode
  )

(defcustom ai-async-query-backends
  '(("OpenAI ChatGPT" . ai--openai--chat-async-send-query)
    ("OpenAI completions" . ai--openai--completions-async-send-query))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-mode)



(defcustom ai--human-lang "english"
  ""
  :type 'string
  :group 'ai-mode
  )

(defcustom ai--query-type-map
  '(("spellcheck" . "Spellcheck this text: %s")
    ("elaborate" . "Elaborate on this text: %s")
    ("explain" . "Explain the following: %s")
    ("document" . "Please add the documentation for the following code: %s")
    ("fix" . "Here is a bug in the following function, please help me fix it: %s")
    ("improve" . "Improve and extend the following code: %s")
    ("optimize" . "Optimize the following code: %s")
    ("refactor" . "Refactor the following code: %s"))
  "An association list that maps query types to their corresponding format strings."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Format String"))
  :group 'ai-mode)


(defvar ai-command-map
  (let ((keymap (make-sparse-keymap)))

    (define-key keymap (kbd "m") 'ai-insert-doc-marker)
    (define-key keymap (kbd "o") 'ai-optimize-code-region)
    (define-key keymap (kbd "i") 'ai-improve-code-region)
    (define-key keymap (kbd "f") 'ai-fix-code-region)
    (define-key keymap (kbd "d") 'ai-document-code-region)
    (define-key keymap (kbd "e b") 'ai-elaborate-code-region)
    (define-key keymap (kbd "s c") 'ai-spellcheck-code-region)
    (define-key keymap (kbd "e") 'ai-explain-code-region)
    (define-key keymap (kbd "c c") 'ai-chat)
    (define-key keymap (kbd "b q") 'ai-change-query-backend)
    (define-key keymap (kbd "b c") 'ai-change-completion-backend)
    (define-key keymap (kbd "p") 'ai-perform)
    (define-key keymap (kbd "s") 'ai-show)
    keymap)
  "Keymap for ai commands.")


(defvar ai-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when ai-keymap-prefix
      (define-key keymap (kbd ai-keymap-prefix) ai-command-map)
      )
    keymap)
  "Keymap used by `ai-mode`.")


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
    (remove-hook 'post-command-hook 'ai-mode-post-command)))


(define-globalized-minor-mode global-ai-mode ai-mode ai-mode-on
  :group 'ai
  )

(defun ai-mode-on ()
  ""
  (interactive)
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
    (insert ai--doc-tag)))


(defun ai-optimize-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "optimize" (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
  )

(defun ai-improve-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "improve" (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
  )

(defun ai-fix-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "fix" (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
  )

(defun ai-document-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type
   "document"
   (ai--get-region-content)
   (ai--with-current-buffer-callback (lambda (text) (ai--replace-tag-in-region ai--doc-tag text))))
  )

(defun ai-elaborate-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "elaborate" (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
  )

(defun ai-spellcheck-code-region ()
  ""
  (interactive)
  (ai--async-query-by-type "spellcheck" (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
  )


(defun ai-explain-code-region ()
  ""
  (interactive)
  (ai--explain-text (ai--get-region-content))
  )

(defun ai--get-explaination-help-buffer ()
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
    (with-help-window (ai--get-explaination-help-buffer)
      (princ "AI Explanation below: \n")
      (princ text)
      (switch-to-buffer (ai--get-explaination-help-buffer))
      )
    )
  )

(defun ai--show-response-buffer (text)
  ""
  (save-excursion
    (with-help-window (ai--get-response-buffer)
      (princ text)
      (switch-to-buffer (ai--get-response-buffer)))))

(defun ai--explain-text (text)
  ""
  (ai--async-query-by-type "explain" text 'ai--show-explain-help-buffer))


(defun ai--async-query-by-type (query-type input callback)
  ""
  (let ((query (ai--format-query query-type input)))
    (funcall ai-async-query-backend query callback)))


(defun ai--format-query (query-type query)
  ""
  (if-let (format-string (cdr (assoc query-type ai--query-type-map)))
      (format format-string query)
    (format "%s\n\n%s" query-type query)))


(defun ai--get-query-type ()
  ""
  (interactive)
  (completing-read ai--query-type-prompt (mapcar #'car ai--query-type-map)))


(defun ai-show ()
  ""
  (interactive)
  (ai--async-query-by-type (ai--get-query-type) (ai--get-region-content) 'ai--show-response-buffe))


(defun ai-perform ()
  ""
  (interactive)
  (if (region-active-p)
      (ai--async-query-by-type (ai--get-query-type) (ai--get-region-content) (ai--replace-region-or-insert-in-current-buffer))
    (message "You must select region for this command")))


(defun ai-change-query-backend ()
  ""
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ai-async-query-backends))))
    (setq ai-async-query-backend (cdr (assoc value ai-async-query-backends)))
    (message (format "AI query backend is changed to \"%s\"" value))))

(provide 'ai)

;;; ai.el ends here
