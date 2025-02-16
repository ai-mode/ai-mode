;;; ai-chat.el --- AI interactive chat -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, chat

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
;; This package provides an interactive chat interface with AI models
;; using the ai-mode framework. It supports multiple AI backends and
;; allows users to interact with different models asynchronously within
;; the Emacs environment. The chat interface is built using `comint-mode`,
;; making it extensible and customizable for various use cases.
;;
;; Features:
;; - Asynchronous interaction with AI models
;; - Dynamic model selection and parameter adjustment
;; - Syntax highlighting and language support in chat
;; - Extensive customization options for prompts and context handling

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'comint)
(require 'ai-utils)

(defgroup ai-chat nil
  "Use AI or AGI API."
  :prefix "ai-chat-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defvar ai-chat--buffer-name "*ai-chat*"
  "Buffer name for AI chat.")

(defcustom ai-chat--prompt "AI> "
  "Prompt text for AI chat."
  :type 'string
  :group 'ai-chat)

(defvar ai-chat--busy nil
  "Indicates whether AI chat is busy processing input.")

(defvar ai-chat--prompt-internal "AI> "
  "Internal prompt text used by AI chat.")

(defvar ai-chat--buffer-history nil
  "History of inputs and responses in the AI chat buffer.")

(defcustom ai-chat--buffer-context-size 5
  "Number of historical messages to display for context."
  :type 'integer
  :group 'ai-chat)

(defvar ai-chat--models-providers nil
  "List of functions providing available models for AI chat.")

(defcustom ai-chat--execution-model nil
  "The current backend used to execute AI requests asynchronously."
  :group 'ai-mode)

(defvar ai-chat--header
  "*** Welcome to AI BUDDY chat ***  \n"
  "Message displayed when AI BUDDY chat starts.")

(defcustom ai-chat--change-backend-prompt "Select query backend: "
  "Prompt for selecting the query backend in AI chat."
  :type 'string
  :group 'ai-chat)

(defvar ai-chat--show-invisible-markers nil
  "If non-nil, marks invisible text that should not be shown.")

(defvar ai-chat-change-model-hook nil
  "Hooks for when the execution model of AI chat is changed.")

(defvar ai-chat-change-params-hook nil
  "Hooks for when parameters of AI chat are changed.")

(defcustom ai-chat--language-mapping '(("elisp" . "emacs-lisp")
                                       ("objective-c" . "objc")
                                       ("cpp" . "c++"))
  "Maps external language names to Emacs mode names."
  :type '(alist)
  :group 'ai-chat)

(defvaralias 'ai-chat--inferior-mode-map 'ai-chat--map)

(defvar ai-chat--map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") 'ai-chat--return)
    (define-key map (kbd "C-c C-c") 'ai-chat--interrupt)
    (define-key map (kbd "RET") 'ai-chat--insert-new-line)
    (define-key map (kbd "C-c C-e") 'ai-chat--clear-buffer)
    (define-key map (kbd "C-c +") 'ai-chat--increase-context-size)
    (define-key map (kbd "C-c -") 'ai-chat--decrease-context-size)
    (define-key map (kbd "C-c C-b") 'ai-chat-change-backend)
    map)
  "Keymap for AI Chat mode.")

(defconst ai-chat--font-lock-keywords
  `(;; Markdown triple backticks source blocks
    ("\\(^\\(```\\)\\([^`\n]*\\)\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     ;; (2) ``` (3) language (4) body (6) ```
     (0 (progn
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 2)
                                     (match-end 2)) 'invisible t)
          ;; Language box.
          (overlay-put (make-overlay (match-beginning 3)
                                     (match-end 3)) 'face '(:box t))
          ;; Additional newline after language box.
          (overlay-put (make-overlay (match-end 3)
                                     (1+ (match-end 3))) 'display "\n\n")
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 6)
                                     (match-end 6)) 'invisible t)
          ;; Show body
          (ai-chat--fontify-source-block
           (buffer-substring (match-beginning 3)
                             (match-end 3))
           ;; body
           (match-beginning 4) (match-end 4))
          nil)))
    ;; Markdown single backticks
    ("`\\([^`\n]+\\)`"
     (1 'markdown-inline-code-face)))
  "Font-lock keywords for syntax highlighting in AI chat.")

(defun ai-chat--fontify-source-block (lang start end)
  "Fontify a block of code from START to END using LANG mode."
  (let ((lang-mode (intern (concat (or
                                    (map-elt ai-chat--language-mapping
                                             (downcase (string-trim lang)))
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties start end))
        (pos (point-min))
        (props))
    (remove-text-properties start end '(face nil))
    (if (fboundp lang-mode)
        (with-current-buffer
            (get-buffer-create
             (format " *AI-chat-fontification:%s*" lang-mode))
          (let ((inhibit-modification-hooks nil))
            (erase-buffer)
            (insert string " ")
            (funcall lang-mode)
            (font-lock-ensure))
          (while (< pos (1- (point-max)))
            (setq props (text-properties-at pos))
            (with-current-buffer (ai-chat--buffer)
              (set-text-properties (+ start (1- pos))
                                   (+ start (1+ (1- pos)))
                                   props))
            (setq pos (1+ pos))))
      (set-text-properties start end
                           '(face 'markdown-pre-face)))))

;;;###autoload
(defun ai-chat ()
  "Start a chat with AI."
  (interactive)
  (message "Starting new chat with AI BUDDY")
  (let ((old-point)
        (buf-name ai-chat--buffer-name))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create ai-chat--buffer-name)
        (setq-local ai-chat--busy nil)
        (set-buffer-multibyte t)
        (setq-local ai-chat--buffer-history '())
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (ai-chat--inferior-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

(defvar ai-chat--input
  "Stores the last input entered by the user.")

(defun ai-chat--clear-buffer ()
  "Clear the AI chat buffer."
  (interactive)
  (comint-clear-buffer)
  (ai-chat--clear-buffer-history))

(defun ai-chat--clear-buffer-history ()
  "Clear the history in the AI chat buffer."
  (with-current-buffer (current-buffer)
    (setq-local ai-chat--buffer-history '())))

(defun ai-chat--increase-context-size ()
  "Increase the context size for AI chat."
  (interactive)
  (setq ai-chat--buffer-context-size (+ ai-chat--buffer-context-size 1))
  (message (format "Current context size: %d" ai-chat--buffer-context-size))
  (run-hooks 'ai-chat-change-params-hook))

(defun ai-chat--decrease-context-size ()
  "Decrease the context size for AI chat."
  (interactive)
  (if (> ai-chat--buffer-context-size 1)
      (setq ai-chat--buffer-context-size (- ai-chat--buffer-context-size 1)))
  (message (format "Current context size: %d" ai-chat--buffer-context-size))
  (run-hooks 'ai-chat-change-params-hook))

(defun ai-chat--set-context-size (size)
  "Set context to SIZE."
  (interactive (list (read-number "Enter context size: ")))
  (setq ai-chat--buffer-context-size size)
  (message (format "Current context size: %s" ai-chat--buffer-context-size))
  (run-hooks 'ai-chat-change-params-hook))

(defun ai-chat--input-sender (_proc input)
  "Capture INPUT for the AI chat mode."
  (setq ai-chat--input input))

(defun ai-chat--get-old-input nil
  "Return the previous input surrounding the point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun ai-chat--process nil
  "Get the process associated with the *ai-chat* buffer."
  (get-buffer-process (ai-chat--buffer)))

(defun ai-chat--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))) pos))

(defun ai-chat--pm ()
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))))

(defun ai-chat--insert-new-line ()
  "Insert a new line in AI chat."
  (interactive)
  (newline))

(define-derived-mode ai-chat--inferior-mode comint-mode "AI CHAT MODE"
  "Major mode for interactively evaluating AI prompts.

This mode allows users to interact with AI models through `comint-mode`."
  (visual-line-mode +1)
  (setq comint-prompt-regexp (concat "^" (regexp-quote ai-chat--prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'ai-chat--input-sender)
  (setq comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'ai-chat--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (setq-local ai-chat--prompt-internal ai-chat--prompt)

  (local-unset-key (kbd "RET"))
  (local-set-key (kbd "RET") 'newline-and-indent)

  (add-to-list 'mode-line-format '(:eval (ai-chat-mode-line-info)) t)

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "ai-chat" (current-buffer) "hexl")
      (file-error (start-process "ai-chat" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (ai-chat--process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (setq-local comint-inhibit-carriage-motion t)

    ;; Add a welcome header
    (insert ai-chat--header)
    (ai-chat--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (ai-chat--process) ai-chat--prompt-internal)
    (set-marker comint-last-input-start (ai-chat--pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))

  (font-lock-add-keywords nil ai-chat--font-lock-keywords))

(defun ai-chat--return ()
  "Handle the <C-return> key binding to send input."
  (interactive)
  (ai-chat--send-input))

(defun ai-chat--send-input ()
  "Send the text in the AI chat after the prompt."
  (interactive)
  (let (ai-chat--input)  ; set by ai-chat--input-sender
    (comint-send-input)
    (ai-chat--eval-input ai-chat--input)))

(defun ai-chat--clear-text-properties (str)
  "Remove all text properties from the string STR."
  (set-text-properties 0 (length str) nil str)
  str)

(defun ai-chat--eval-input (input-string)
  "Evaluate INPUT-STRING in AI chat mode."
  (if (not ai-chat--busy)
      (progn
        (setq ai-chat--busy t)
        (cond
         ((string-empty-p (string-trim input-string))
          (comint-output-filter (ai-chat--process)
                                (concat "\n" ai-chat--prompt-internal))
          (setq ai-chat--busy nil))
         (t
          (comint-output-filter (ai-chat--process)
                                (propertize "<ai--chat-end-of-prompt>"
                                            'invisible (not ai-chat--show-invisible-markers)))
          (ai-chat--add-entry-to-history input-string 'user-input)

          (condition-case processing-error
              (let* ((execution-model (ai-chat--get-current-model))
                     (execution-backend (map-elt execution-model :execution-backend))
                     (context (ai-chat--get-execution-context)))
                (funcall execution-backend
                         context
                         execution-model
                         :success-callback 'ai-chat--request-success-callback
                         :fail-callback 'ai-chat--request-fail-callback))

            (error (error  "Evaluation input error: %s" (error-message-string processing-error) ))))))
    (error "AI busy")))

(defun ai-chat--add-entry-to-history (input &optional message-type)
  "Append INPUT to the AI chat buffer history.

MESSAGE-TYPE specifies the type of message and is optional."
  (let ((entry (if (and (listp input)
                        (plist-get input :type))
                   input
                 (ai-common--make-typed-struct (ai-chat--clear-text-properties input) message-type))))
    (setq-local ai-chat--buffer-history (append ai-chat--buffer-history `(,entry)))))

(defun ai-chat--request-fail-callback (requst-data error-message)
  "Handle request failures and display ERROR-MESSAGE.

REQUEST-DATA is the data sent with the failed request."
  (let ((content (ai-common--get-text-content-from-struct error-message)))
    (ai-chat--write-reply (or content "Request failed. Please, check your connection or try again later.")))
  (setq ai-chat--busy nil))

(defun ai-chat--request-success-callback (messages)
  "Handle successful requests by processing MESSAGES.

MESSAGES is a list of messages returned from the backend."
  (condition-case-unless-debug processing-error
      (progn
        (setq ai-chat--busy nil)
        (with-current-buffer (ai-chat--buffer)
          (dolist (entry messages)
            (ai-chat--add-entry-to-history entry)
            (ai-chat--write-reply (ai-common--get-text-content-from-struct entry)))))
    (error  (progn
              (setq ai-chat--busy nil)
              (ai-chat--write-reply "EMACS: Invalid request. Please, try again.")
              (error "Process chat request error: %s" (error-message-string processing-error))))))

(defun ai-chat--buffer ()
  "Return the AI chat buffer."
  (get-buffer-create ai-chat--buffer-name))

(cl-defun ai-chat--get-buffer-history-context (&optional (size ai-chat--buffer-context-size))
  "Retrieve a slice of the chat history based on SIZE.

SIZE is the number of historical messages to include. If SIZE is less than or equal to 0, return the full history."
  (condition-case _
      (with-current-buffer (ai-chat--buffer)
        (if (or (<= size 0) (> size (length ai-chat--buffer-history)))
            ai-chat--buffer-history
          (seq-subseq ai-chat--buffer-history (* -1 size))))
    (error (list))))

(cl-defun ai-chat--get-execution-context ()
  "Construct the execution context for the current chat interaction."
  (let* ((messages-history (ai-chat--get-buffer-history-context))
         (full-context '())
         (basic-file-prompt (ai-common--make-typed-struct (ai--get-rendered-action-prompt "chat-basic" full-context) 'agent-instructions))
         (global-system-prompts (ai-common--get-global-system-prompts))
         (global-memory (ai-common--get-global-memory))
         (buffer-bound-prompts (ai-common--get-buffer-bound-prompts))

         (additional-context
          (let ((context-pool-content (ai-common--render-struct-to-string (ai-common--get-context-pool))))
            (when context-pool-content
              (ai-common--make-typed-struct context-pool-content 'additional-context))))

         (messages
          (cl-remove-if #'null
                        (append
                         (when ai--extended-instructions-enabled
                           (list basic-file-prompt
                                 global-system-prompts
                                 global-memory
                                 buffer-bound-prompts
                                 additional-context))
                         messages-history)))

         (filtered-messages (cl-remove-if #'ai-utils--is-empty-message messages))
         (full-context `((:messages . ,filtered-messages))))
    full-context))

(defun ai-chat--interrupt ()
  "Interrupt the current request being processed by AI."
  (interactive)
  (with-current-buffer (ai-chat--buffer)
    (comint-send-input)
    (goto-char (point-max))
    (comint-output-filter (ai-chat--process)
                          (concat (propertize "<ai--chat-end-of-prompt>\n<ai--chat-ignored-response>"
                                              'invisible (not ai-chat--show-invisible-markers))
                                  "\n"
                                  ai-chat--prompt-internal))
    (setq ai-chat--busy nil)))

(defun ai-chat--write-reply (reply &optional failed)
  "Write REPLY to the chat buffer.

If FAILED is non-nil, marks reply as invisible to indicate failure."
  (comint-output-filter (ai-chat--process)
                        (concat "\n"
                                (string-trim reply)
                                (if failed
                                    (propertize "\n<ai--chat-ignored-response>"
                                                'invisible (not ai-chat--show-invisible-markers))
                                  "")
                                "\n\n"
                                ai-chat--prompt-internal)))

(defun ai-chat--set-execution-model (model)
  "Set the execution model to MODEL and run any setup function."
  (let ((setup-function (map-elt model :setup-function)))
    (when setup-function
      (funcall setup-function))
    (setq ai-chat--execution-model model)
    (run-hooks 'ai-chat-change-model-hook)))

(defun ai-chat-change-backend (&optional model-name)
  "Change the chat query backend to MODEL-NAME."
  (interactive)
  (let* ((names (mapcar (lambda (item) `(,(map-elt item :name) ,item)) (ai-chat--get-models)))
         (value (or model-name (completing-read ai--change-backend-prompt (mapcar #'car names))))
         (model (ai-utils--find-model-config-by-name value (ai-chat--get-models))))
    (ai-chat--set-execution-model model)
    (message (format "AI chat query async backend is changed to \"%s\"" value))))

(defun ai-chat--get-current-model ()
  "Retrieve the current execution model for AI chat."
  (or ai-chat--execution-model
      (let ((default-model (car (ai-chat--get-models))))
        (ai-chat--set-execution-model default-model)
        default-model)))

(defun ai-chat--get-models ()
  "Return a flat list of available AI models retrieved from multiple sources."
  (if (not ai-chat--models-providers)
      (error "You need to setup ai-chat--models-providers"))
  (let ((model-funcs ai-chat--models-providers))
    (apply #'append (mapcar #'funcall model-funcs))))

(defun ai-chat-mode-line-info ()
  "Construct and return formatted mode line information for AI chat."
  (let* ((model (ai-chat--get-current-model)))
    (format "AI-CHAT[%s|%d]" (map-elt model :name) (ai-chat-get-context-size))))

(defun ai-chat-get-context-size ()
  "Return the current context size for AI chat."
  ai-chat--buffer-context-size)

(defun ai-chat-update-mode-name ()
  "Set dynamic mode name for `ai-chat--inferior-mode`."
  (setq-local mode-name (ai-chat-mode-line-info)))

(defun ai-chat-mode-update-mode-line-info ()
  "Update the mode line information in AI chat mode."
  (ai-chat-update-mode-name)
  (force-mode-line-update))

(add-hook 'ai-chat--inferior-mode-hook 'ai-chat-mode-update-mode-line-info)
(add-hook 'ai-chat-change-model-hook 'ai-chat-mode-update-mode-line-info)
(add-hook 'ai-chat-change-params-hook  'ai-chat-mode-update-mode-line-info)

(provide 'ai-chat)

;;; ai-chat.el ends here
