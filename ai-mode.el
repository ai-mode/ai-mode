;;; ai-mode.el --- AI interaction mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
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
;; This library and package defines a set of functions and variables for interation
;; with AI engines.
;; Currently available:
;; - Code completion
;; - Code optimization
;; - Code fixing
;; - Code documentation
;; - Code improvement
;; - Code elaboration
;; - Code explanation
;; - Chatting with AI.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(require 'ai-utils)
(require 'ai-openai)
(require 'ai-openai-completions)
(require 'ai-openai-chat)
(require 'ai-completions)
(require 'ai-chat)


(defgroup ai nil
  "Support for AI interactions."
  :prefix "ai-"
  :group 'emacs
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))


(defvar url-http-end-of-headers)


(defgroup ai-mode nil
  "Use AI or AGI API."
  :prefix "ai-mode"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))


(defcustom ai-keymap-prefix "C-c i"
  "AI mode keymap prefix."
  :group 'ai-mode
  :type 'string)


(defcustom ai--query-type-prompt "Type of Query: "
  "Prompt for selecting the type of request."
  :type 'string
  :group 'ai-mode)

(defcustom ai--change-backend-prompt "Select query backend: "
  "Prompt for selecting backend."
  :type 'string
  :group 'ai-mode)


(defcustom ai--doc-tag "{{__ai_doc__}}"
  "Marker for documentation placement."
  :type 'string
  :group 'ai-mode)

(defcustom ai-async-query-backend 'ai-openai-chat--async-send-query
  "The current backend used to execute requests asynchronously."
  :group 'ai-mode)

(defcustom ai-async-query-backends
  '(("OpenAI ChatGPT" . ai-openai-chat--async-send-query)
    ("OpenAI completions" . ai-openai-completions--async-send-query))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-mode)


(defcustom ai-sync-query-backend 'ai-openai-chat--sync-send-query
  "The current backend used to execute requests asynchronously."
  :group 'ai-mode)

(defcustom ai-sync-query-backends
  '(("OpenAI ChatGPT" . ai-openai-chat--sync-send-query)
    ("OpenAI completions" . ai-openai-completions--sync-send-query))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-mode)



(defcustom ai--human-lang "english"
  "The language in which AI provides answers."
  :type 'string
  :group 'ai-mode)

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
    (define-key keymap (kbd "b c") 'ai-completions-change-backend)
    (define-key keymap (kbd "p") 'ai-perform)
    (define-key keymap (kbd "s") 'ai-show)
    keymap)
  "Keymap for ai commands.")


(defvar ai-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when ai-keymap-prefix
      (define-key keymap (kbd ai-keymap-prefix) ai-command-map))
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
        (add-hook 'post-command-hook 'ai-mode-post-command))
    (remove-hook 'pre-command-hook 'ai-mode-pre-command)
    (remove-hook 'post-command-hook 'ai-mode-post-command)))

;;;###autoload
(define-globalized-minor-mode global-ai-mode ai-mode ai-mode-on
  :group 'ai)

(defun ai-mode-on ()
  "Turn on AI mode."
  (interactive)
  (ai-mode 1))

(defun ai-mode-pre-command ()
  "AI mode pre command hook.")

(defun ai-mode-post-command ()
  "AI mode post command hook.")


(defun ai-complete-code-at-point ()
  "Start a completion at point."
  (interactive)
  (ai-completions--coordinator))

(defun ai-insert-doc-marker ()
  "Insert a marker for documentation placement."
  (interactive)
  (with-current-buffer (current-buffer)
    (insert ai--doc-tag)))


(defun ai-optimize-code-region ()
  "Optimize code region and replace it."
  (interactive)
  (ai--async-query-by-type "optimize" (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer)))

(defun ai-improve-code-region ()
  "Improve code region and replace it."
  (interactive)
  (ai--async-query-by-type "improve" (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer)))

(defun ai-fix-code-region ()
  "Fix code region and replace it."
  (interactive)
  (ai--async-query-by-type "fix" (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer)))

(defun ai-document-code-region ()
  "Document code region and replace doc market."
  (interactive)
  (ai--async-query-by-type
   "document"
   (ai-utils--get-region-content)
   (ai--with-current-buffer-callback (lambda (text) (ai-utils--replace-tag-in-region ai--doc-tag text)))))

(defun ai-elaborate-code-region ()
  "Elaborate code region and replace it."
  (interactive)
  (ai--async-query-by-type "elaborate" (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer)))

(defun ai-spellcheck-code-region ()
  "Spellcheck code region and replace it."
  (interactive)
  (ai--async-query-by-type "spellcheck" (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer)))


(defun ai-explain-code-region ()
  "Explain code region and show explaination in help buffer."
  (interactive)
  (ai--explain-text (ai-utils--get-region-content)))


(defun ai--explain-text (text)
  "Explain TEXT and show explaination in help buffer."
  (ai--async-query-by-type "explain" text 'ai-utils--show-explain-help-buffer))


(defun ai--async-query-by-type (query-type input callback)
  "Perform INPUT using the QUERY-TYPE type and passes the result to the CALLBACK function."
  (let ((query (ai--format-query query-type input)))
    (ai-perform-async-backend-query query callback :fail-callback nil)))


(defun ai--format-query (query-type query)
  "Format a request to AI based on QUERY and QUERY-TYPE."
  (if-let (format-string (cdr (assoc query-type ai--query-type-map)))
      (format format-string query)
    (format "%s\n\n%s" query-type query)))


(defun ai--get-query-type ()
  "Get the request type using `completing-read`."
  (interactive)
  (completing-read ai--query-type-prompt (mapcar #'car ai--query-type-map)))


(defun ai-show ()
  "Execute query and show response in special buffer."
  (interactive)
  (ai--async-query-by-type (ai--get-query-type) (ai-utils--get-region-content) 'ai--show-response-buffe))


(defun ai-perform ()
  "Execute request and replace selected region."
  (interactive)
  (if (region-active-p)
      (ai--async-query-by-type (ai--get-query-type) (ai-utils--get-region-content) (ai-utils--replace-region-or-insert-in-current-buffer))
    (message "You must select region for this command")))


(defun ai-change-async-query-backend ()
  "Change query backend."
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ai-async-query-backends))))
    (setq ai-async-query-backend (cdr (assoc value ai-async-query-backends)))
    (message (format "AI query async backend is changed to \"%s\"" value))))


(defun ai-change-sync-query-backend ()
  "Change query backend."
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ai-sync-query-backends))))
    (setq ai-sync-query-backend (cdr (assoc value ai-sync-query-backends)))
    (message (format "AI query sync backend is changed to \"%s\"" value))))


(cl-defun ai-perform-async-backend-query (query callback &key
                                             (fail-callback nil)
                                             (extra-params nil))
  "Execute QUERY by current backend asynchronously.

After successful execution CALLBACK will called.
If execution failed then FAIL-CALLBACK called if provided.
EXTRA-PARAMS is a alist of extra parameters for backend."
  (funcall ai-async-query-backend query callback :fail-callback fail-callback))


(cl-defun ai-perform-sync-backend-query (query &key (extra-params nil))
  "Execute QUERY by current backend synchronously.

EXTRA-PARAMS is a alist of extra parameters for backend."
  (funcall ai-sync-query-backend query :extra-params extra-params))



(provide 'ai-mode)

;;; ai-mode.el ends here
