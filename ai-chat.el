;;; ai-chat.el --- AI interactive chat -*- lexical-binding: t -*-

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
;; Run `ai-chat` to get chat with AI.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'comint)


(defgroup ai-chat nil
  "Use AI or AGI API."
  :prefix "ai-chat-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))


(defvar ai-chat--buffer-name "*ai-chat*")

(defcustom ai-chat--prompt "AI> "
  "Prompt text."
  :type 'string
  :group 'ai-chat)

(defvar ai-chat--busy nil)

(defvar ai-chat--prompt-internal "AI> ")

(defvar ai-chat--buffer-history '())

(defcustom ai-chat--buffer-context-size 1
  "History context size."
  :type 'integer
  :group 'ai-chat)

(defcustom ai-chat--query-backend 'ai-openai-chat--async-send-context
  "Active chat backend."
  :type 'symbol
  :group 'ai-chat)

(defcustom ai-chat--query-backends
  '(("OpenAI ChatGPT" . ai-openai-chat--async-send-context))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-chat)

(defvar ai-chat--header
  "*** Welcome to AI BUDDY chat ***  \n"
  "Message to display when AI BUDDY chat is started.")

(defcustom ai-chat--change-backend-prompt "Select query backend: "
  "Prompt for selecting backend."
  :type 'string
  :group 'ai-chat)

(defvar ai-chat--show-invisible-markers nil)


(defcustom ai-chat--language-mapping '(("elisp" . "emacs-lisp")
                                       ("objective-c" . "objc")
                                       ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
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
     (1 'markdown-inline-code-face))))


(defun ai-chat--fontify-source-block (lang start end)
  "Fontify using LANG from START to END."
  (let ((lang-mode (intern (concat (or
                                    (map-elt ai-chat--language-mapping
                                             (downcase (string-trim lang)))
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties start end))
        ;; (buf (current-buffer))
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
  (let ((old-point)
        (buf-name ai-chat--buffer-name))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create ai-chat--buffer-name)
        (setq ai-chat--busy nil)
        (set-buffer-multibyte t)
        (setq-local ai-chat--buffer-history '())
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (ai-chat--inferior-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))


(defvar ai-chat--input)

(defun ai-chat--clear-buffer ()
  "Clear buffer."
  (interactive)
  (comint-clear-buffer)
  (ai-chat--clear-buffer-history))

(defun ai-chat--clear-buffer-history ()
  "Clear history."
  (with-current-buffer (current-buffer)
    (setq-local ai-chat--buffer-history '())))

(defun ai-chat--increase-context-size ()
  "Increase context size."
  (interactive)
  (setq ai-chat--buffer-context-size (+ ai-chat--buffer-context-size 1))
  (message (format "Current context size: %d" ai-chat--buffer-context-size)))


(defun ai-chat--decrease-context-size ()
  "Decrease context size."
  (interactive)
  (if (> ai-chat--buffer-context-size 1)
      (setq ai-chat--buffer-context-size (- ai-chat--buffer-context-size 1)))
  (message (format "Current context size: %d" ai-chat--buffer-context-size))
  nil)


(defun ai-chat--input-sender (_proc input)
  "Input sender func.

INPUT"
  (setq ai-chat--input input))

(defun ai-chat--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))


(defun ai-chat--process nil
  "Get *ai-chat* process."
  (get-buffer-process (ai-chat--buffer)))


(defun ai-chat--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))) pos))


(defun ai-chat--pm ()
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))))


(defun ai-chat--insert-new-line ()
  "Invert new line."
  (interactive)
  (newline))

(define-derived-mode ai-chat--inferior-mode comint-mode "AI MODE"
  "Major mode for interactively evaluating AI prompts.
Uses the interface provided by `comint-mode'"
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

    ;; Add a silly header
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
  "RET binding."
  (interactive)
  (ai-chat--send-input))

(defun ai-chat--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (ai-chat--input)  ; set by ai-chat--input-sender
    (comint-send-input)
    (ai-chat--eval-input ai-chat--input)))


(defun ai-chat--eval-input (input-string)
  "Evaluate INPUT-STRING."
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

          (setq-local ai-chat--buffer-history (append ai-chat--buffer-history `((("role" . "user") ("content" . ,input-string)))))

          (condition-case processing-error
              (funcall ai-chat--query-backend (ai-chat--get-buffer-history-context) 'ai-chat--request-callback)
            (error (error  "Evaluation input error: %s" (error-message-string processing-error) ))))))
    (error "AI busy")))

(defun ai-chat--request-callback (messages)
  "Request callback.

MESSAGES - list of messages returned from backend."
  (condition-case-unless-debug processing-error
      (let* ((message (elt messages 0))
             (content (cdr (assoc "content" message))))
        (setq ai-chat--busy nil)
        (with-current-buffer (ai-chat--buffer)
          (setq-local ai-chat--buffer-history (append ai-chat--buffer-history `(,message)))
          (ai-chat--write-reply content)))
    (error  (progn
              (setq ai-chat--busy nil)
              (ai-chat--write-reply "EMACS: Invalid request. Please, try again.")
              (error "Process chat request error: %s" (error-message-string processing-error))))))

(defun ai-chat--buffer ()
  "Return chat buffer."
  (get-buffer-create ai-chat--buffer-name))

(cl-defun ai-chat--get-buffer-history-context (&optional (size ai-chat--buffer-context-size))
  "Get history context.

SIZE - number of messages."
  (condition-case _
      (with-current-buffer (ai-chat--buffer)
        (if (> size (length ai-chat--buffer-history))
            ai-chat--buffer-history
          (seq-subseq ai-chat--buffer-history (* -1 size))))
    (error (list))))

(defun ai-chat--interrupt ()
  "Interrupt current request."
  (interactive)
  (with-current-buffer (ai-chat--buffer)
    (comint-send-input)
    (goto-char (point-max))
    (comint-output-filter (ai-chat--process)
                          (concat (propertize "<ai--chat-end-of-prompt>\n<ai--chat-ignored-response>"
                                              'invisible (not ai-chat--show-invisible-markers))
                                  "\n"
                                  ai-chat--prompt-internal))
    (setq ai-chat--busy nil)
    (message "interrupted!")))

(defun ai-chat--write-reply (reply &optional failed)
  "Write reply go chat.

REPLY - Recorded message.
FAILED - mark as invisible."
  (comint-output-filter (ai-chat--process)
                        (concat "\n"
                                (string-trim reply)
                                (if failed
                                    (propertize "\n<ai--chat-ignored-response>"
                                                'invisible (not ai-chat--show-invisible-markers))
                                  "")
                                "\n\n"
                                ai-chat--prompt-internal)))

(defun ai-chat-change-backend ()
  "Change chat backend."
  (interactive)
  (let* ((value (completing-read ai-chat--change-backend-prompt (mapcar #'car ai-chat--query-backends))))
    (setq ai-chat--query-backend (cdr (assoc value ai-chat--query-backends)))
    (message (format "AI chat query backend changed to \"%s\"" value))))


(provide 'ai-chat)

;;; ai-chat.el ends here
