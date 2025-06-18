;;; ai-chat.el --- AI interactive chat -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
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

;;; Group definition

(defgroup ai-chat nil
  "Use AI or AGI API."
  :prefix "ai-chat-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

;;; Customizable variables

(defcustom ai-chat--prompt "AI> "
  "Prompt text for AI chat."
  :type 'string
  :group 'ai-chat)

(defcustom ai-chat--buffer-context-size 5
  "Number of historical messages to display for context."
  :type 'integer
  :group 'ai-chat)

(defcustom ai-chat--execution-model nil
  "The current backend used to execute AI requests asynchronously."
  :group 'ai-chat)

(defcustom ai-chat--change-backend-prompt "Select query backend: "
  "Prompt for selecting the query backend in AI chat."
  :type 'string
  :group 'ai-chat)

(defcustom ai-chat--language-mapping '(("elisp" . "emacs-lisp")
                                       ("objective-c" . "objc")
                                       ("cpp" . "c++")
                                       ("c++" . "c++")
                                       ("csharp" . "c#")
                                       ("cs" . "c#")
                                       ("javascript" . "js")
                                       ("typescript" . "ts")
                                       ("python" . "python")
                                       ("py" . "python")
                                       ("ruby" . "ruby")
                                       ("rb" . "ruby")
                                       ("rust" . "rust")
                                       ("rs" . "rust")
                                       ("golang" . "go")
                                       ("go" . "go")
                                       ("java" . "java")
                                       ("kotlin" . "kotlin")
                                       ("kt" . "kotlin")
                                       ("swift" . "swift")
                                       ("php" . "php")
                                       ("perl" . "perl")
                                       ("pl" . "perl")
                                       ("r" . "r")
                                       ("scala" . "scala")
                                       ("clojure" . "clojure")
                                       ("clj" . "clojure")
                                       ("cljs" . "clojurescript")
                                       ("haskell" . "haskell")
                                       ("hs" . "haskell")
                                       ("ocaml" . "ocaml")
                                       ("ml" . "ocaml")
                                       ("fsharp" . "f#")
                                       ("fs" . "f#")
                                       ("elixir" . "elixir")
                                       ("ex" . "elixir")
                                       ("exs" . "elixir")
                                       ("erlang" . "erlang")
                                       ("erl" . "erlang")
                                       ("dart" . "dart")
                                       ("julia" . "julia")
                                       ("jl" . "julia")
                                       ("lua" . "lua")
                                       ("groovy" . "groovy")
                                       ("pascal" . "pascal")
                                       ("pas" . "pascal")
                                       ("lisp" . "lisp")
                                       ("lsp" . "lisp")
                                       ("scheme" . "scheme")
                                       ("scm" . "scheme")
                                       ("shell" . "sh")
                                       ("bash" . "sh")
                                       ("zsh" . "sh")
                                       ("fish" . "sh")
                                       ("powershell" . "powershell")
                                       ("ps1" . "powershell")
                                       ("sql" . "sql")
                                       ("mysql" . "sql")
                                       ("postgresql" . "sql")
                                       ("sqlite" . "sql")
                                       ("html" . "html")
                                       ("htm" . "html")
                                       ("xml" . "xml")
                                       ("css" . "css")
                                       ("scss" . "scss")
                                       ("sass" . "sass")
                                       ("less" . "less")
                                       ("json" . "json")
                                       ("yaml" . "yaml")
                                       ("yml" . "yaml")
                                       ("toml" . "toml")
                                       ("ini" . "ini")
                                       ("conf" . "conf")
                                       ("config" . "conf")
                                       ("markdown" . "markdown")
                                       ("md" . "markdown")
                                       ("tex" . "latex")
                                       ("latex" . "latex")
                                       ("dockerfile" . "dockerfile")
                                       ("docker" . "dockerfile")
                                       ("makefile" . "makefile")
                                       ("make" . "makefile")
                                       ("cmake" . "cmake")
                                       ("terraform" . "terraform")
                                       ("tf" . "terraform")
                                       ("hcl" . "terraform")
                                       ("nginx" . "nginx")
                                       ("apache" . "apache")
                                       ("vim" . "vim")
                                       ("vimscript" . "vim")
                                       ("asm" . "asm")
                                       ("assembly" . "asm")
                                       ("diff" . "diff")
                                       ("patch" . "diff")
                                       ("gitignore" . "gitignore")
                                       ("gitconfig" . "gitconfig")
                                       ("properties" . "properties")
                                       ("log" . "log")
                                       ("plain" . "text")
                                       ("text" . "text")
                                       ("txt" . "text"))
  "Maps external language names to Emacs mode names."
  :type '(alist)
  :group 'ai-chat)

(defcustom ai-chat--history-directory (expand-file-name "~/.ai-chat-history")
  "Directory where AI chat history files are stored."
  :type 'directory
  :group 'ai-chat)

(defcustom ai-chat--auto-save-enabled nil
  "Whether to automatically save chat sessions to files."
  :type 'boolean
  :group 'ai-chat)

(defcustom ai-chat--progress-indicator-enabled t
  "Enable progress indicator for AI chat requests."
  :type 'boolean
  :group 'ai-chat)

(defcustom ai-chat--progress-indicator-style 'spinner
  "Style of progress indicator to use for AI chat requests."
  :type '(choice (const :tag "Spinner animation" spinner)
                 (const :tag "Progress dots" dots)
                 (const :tag "Message only" message))
  :group 'ai-chat)

;;; Variables

(defvar ai-chat--buffer-name "*ai-chat*"
  "Buffer name for AI chat.")

(defvar ai-chat--busy nil
  "Indicates whether AI chat is busy processing input.")

(defvar ai-chat--prompt-internal "AI> "
  "Internal prompt text used by AI chat.")

(defvar ai-chat--buffer-history nil
  "History of inputs and responses in the AI chat buffer.")

(defvar ai-chat--models-providers nil
  "List of functions providing available models for AI chat.")

(defvar ai-chat--header
  "*** Welcome to AI BUDDY chat ***  \n"
  "Message displayed when AI BUDDY chat starts.")

(defvar ai-chat--show-invisible-markers nil
  "If non-nil, marks invisible text that should not be shown.")

(defvar ai-chat--input nil
  "Stores the last input entered by the user.")

(defvar ai-chat--current-session-start-time nil
  "Timestamp when the current chat session started.")

(defvar ai-chat--current-session-id nil
  "Unique identifier for the current chat session.")

;; Progress indicator variables (buffer-local)
(defvar-local ai-chat--progress-timer nil
  "Timer for progress indicator animation.")

(defvar-local ai-chat--progress-counter 0
  "Counter for progress indicator animation.")

(defvar-local ai-chat--progress-active nil
  "Flag indicating if progress indicator is currently active.")

(defvar-local ai-chat--progress-message "AI request in progress"
  "Message to display during AI request progress.")

(defvar-local ai-chat--progress-start-time nil
  "Start time of the current AI request.")

;;; Hook variables

(defvar ai-chat-change-model-hook nil
  "Hooks for when the execution model of AI chat is changed.")

(defvar ai-chat-change-params-hook nil
  "Hooks for when parameters of AI chat are changed.")

;;; Keymaps

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
    (define-key map (kbd "C-c C-s") 'ai-chat-save-session)
    (define-key map (kbd "C-c C-l") 'ai-chat-load-session)
    (define-key map (kbd "C-c C-a") 'ai-chat-toggle-auto-save)
    map)
  "Keymap for AI Chat mode.")

;;; Font lock

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

;;; Utility functions

(defun ai-chat--buffer ()
  "Return the AI chat buffer."
  (get-buffer-create ai-chat--buffer-name))

(defun ai-chat--process ()
  "Get the process associated with the *ai-chat* buffer."
  (get-buffer-process (ai-chat--buffer)))

(defun ai-chat--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))) pos))

(defun ai-chat--pm ()
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (get-buffer-create ai-chat--buffer-name))))

(defun ai-chat--get-old-input ()
  "Return the previous input surrounding the point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun ai-chat--input-sender (_proc input)
  "Capture INPUT for the AI chat mode."
  (setq ai-chat--input input))

(defun ai-chat--clear-text-properties (str)
  "Remove all text properties from the string STR."
  (set-text-properties 0 (length str) nil str)
  str)

(defun ai-chat--make-typed-struct (content type)
  "Create a typed struct with CONTENT and TYPE."
  (list :content content :type type))

(defun ai-chat--get-text-content-from-struct (struct)
  "Extract text content from STRUCT."
  (cond
   ((and (listp struct) (plist-get struct :content))
    (plist-get struct :content))
   ((stringp struct)
    struct)
   (t "")))

(defun ai-chat--get-struct-type (struct)
  "Extract type from STRUCT."
  (cond
   ((and (listp struct) (plist-get struct :type))
    (plist-get struct :type))
   (t 'unknown)))

(defun ai-chat--is-empty-message (message)
  "Check if MESSAGE is empty or nil."
  (let ((content (ai-chat--get-text-content-from-struct message)))
    (or (null content)
        (string-empty-p (string-trim content)))))

(defun ai-chat--reconstruct-single-typed-struct (plist-entry)
  "Reconstructs an ai-common typed struct from its plist representation.
   Assumes plist-entry is of the form (:content X :type Y)."
  (when (and (listp plist-entry) (plist-get plist-entry :content) (plist-get plist-entry :type))
    (ai-chat--make-typed-struct (plist-get plist-entry :content)
                                (plist-get plist-entry :type))))

(defun ai-chat--reconstruct-list-of-typed-structs (list-of-plists)
  "Reconstructs a list of ai-common typed structs from a list of plist representations."
  (mapcar 'ai-chat--reconstruct-single-typed-struct list-of-plists))

(defun ai-chat--generate-session-id ()
  "Generate a unique session identifier."
  (format "%s-%04x"
          (format-time-string "%Y%m%d%H%M%S")
          (random 65536)))

;;; Progress indicator functions

(defun ai-chat--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let* ((elapsed (- (float-time) start-time))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun ai-chat--progress-start (&optional message buffer)
  "Start progress indicator with optional MESSAGE in specified BUFFER or current buffer."
  (when ai-chat--progress-indicator-enabled
    (with-current-buffer (or buffer (current-buffer))
      (setq ai-chat--progress-active t
            ai-chat--progress-counter 0
            ai-chat--progress-start-time (float-time)
            ai-chat--progress-message (or message "AI request in progress"))

      (cond
       ((eq ai-chat--progress-indicator-style 'spinner)
        (ai-chat--progress-start-spinner))
       ((eq ai-chat--progress-indicator-style 'dots)
        (ai-chat--progress-start-dots))
       ((eq ai-chat--progress-indicator-style 'message)
        (ai-chat-mode-update-mode-line-info))))))

(defun ai-chat--progress-stop (&optional buffer)
  "Stop progress indicator in specified BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ai-chat--progress-active
      (setq ai-chat--progress-active nil
            ai-chat--progress-start-time nil)
      (when ai-chat--progress-timer
        (cancel-timer ai-chat--progress-timer)
        (setq ai-chat--progress-timer nil))
      (ai-chat-mode-update-mode-line-info))))

(defun ai-chat--progress-start-spinner ()
  "Start spinner-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai-chat--progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (when ai-chat--progress-active
                                  (setq ai-chat--progress-counter (1+ ai-chat--progress-counter))
                                  (ai-chat-mode-update-mode-line-info)))))))))

(defun ai-chat--progress-start-dots ()
  "Start dots-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai-chat--progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (when ai-chat--progress-active
                                  (setq ai-chat--progress-counter (1+ ai-chat--progress-counter))
                                  (ai-chat-mode-update-mode-line-info)))))))))

(defun ai-chat--progress-wrap-callback (original-callback &optional buffer)
  "Wrap ORIGINAL-CALLBACK to stop progress indicator when called in specified BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (lambda (&rest args)
      (ai-chat--progress-stop target-buffer)
      (when original-callback
        (apply original-callback args)))))

;;; Font lock functions

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

;;; Fallback functions for missing ai-common functionality

(defun ai-chat--get-global-system-prompts ()
  "Fallback function to get global system prompts."
  nil)

(defun ai-chat--set-global-system-prompts (_prompts)
  "Fallback function to set global system prompts."
  nil)

(defun ai-chat--get-global-memory ()
  "Fallback function to get global memory."
  nil)

(defun ai-chat--set-global-memory (_memory)
  "Fallback function to set global memory."
  nil)

(defun ai-chat--get-buffer-bound-prompts ()
  "Fallback function to get buffer bound prompts."
  nil)

(defun ai-chat--set-buffer-bound-prompts (_prompts)
  "Fallback function to set buffer bound prompts."
  nil)

(defun ai-chat--get-context-pool ()
  "Fallback function to get context pool."
  nil)

(defun ai-chat--set-context-pool (_context)
  "Fallback function to set context pool."
  nil)

(defun ai-chat--render-struct-to-string (struct)
  "Fallback function to render struct to string."
  (ai-chat--get-text-content-from-struct struct))

(defun ai-chat--get-rendered-action-prompt (_action-type _context)
  "Fallback function to get rendered action prompt."
  "You are an AI assistant. Please help the user with their request.")

;;; Model management

(defun ai-chat--get-models ()
  "Return a flat list of available AI models retrieved from multiple sources."
  (if (not ai-chat--models-providers)
      (error "You need to setup ai-chat--models-providers"))
  (let ((model-funcs ai-chat--models-providers))
    (apply #'append (mapcar #'funcall model-funcs))))

(defun ai-chat--get-current-model ()
  "Retrieve the current execution model for AI chat."
  (or ai-chat--execution-model
      (let ((default-model (car (ai-chat--get-models))))
        (ai-chat--set-execution-model default-model)
        default-model)))

(defun ai-chat--set-execution-model (model)
  "Set the execution model to MODEL and run any setup function."
  (let ((setup-function (map-elt model :setup-function)))
    (when setup-function
      (funcall setup-function))
    (setq ai-chat--execution-model model)
    (customize-save-variable 'ai-chat--execution-model model) ; Save the setting persistently
    (run-hooks 'ai-chat-change-model-hook)))

;;; Context management

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
         (basic-file-prompt (ai-chat--make-typed-struct (ai-chat--get-rendered-action-prompt "chat-basic" full-context) 'agent-instructions))
         (global-system-prompts (ai-chat--get-global-system-prompts))
         (global-memory (ai-chat--get-global-memory))
         (buffer-bound-prompts (ai-chat--get-buffer-bound-prompts))

         (additional-context
          (let ((context-pool-content (ai-chat--render-struct-to-string (ai-chat--get-context-pool))))
            (when context-pool-content
              (ai-chat--make-typed-struct context-pool-content 'additional-context))))

         (messages
          (cl-remove-if #'null
                        (append
                         (when (boundp 'ai--extended-instructions-enabled)
                           (list basic-file-prompt
                                 global-system-prompts
                                 global-memory
                                 buffer-bound-prompts
                                 additional-context))
                         messages-history)))

         (filtered-messages (cl-remove-if #'ai-chat--is-empty-message messages))
         (full-context `((:messages . ,filtered-messages))))
    full-context))

(defun ai-chat--add-entry-to-history (input &optional message-type)
  "Append INPUT to the AI chat buffer history.

MESSAGE-TYPE specifies the type of message and is optional."
  (let ((entry (if (and (listp input)
                        (plist-get input :type))
                   input
                 (ai-chat--make-typed-struct (ai-chat--clear-text-properties input) message-type))))
    (setq-local ai-chat--buffer-history (append ai-chat--buffer-history `(,entry)))))

;;; Session management

(defun ai-chat--ensure-history-directory ()
  "Ensure the AI chat history directory exists."
  (unless (file-exists-p ai-chat--history-directory)
    (make-directory ai-chat--history-directory t)))

(defun ai-chat--get-session-filename ()
  "Generate filename for current chat session based on start time and session ID."
  (when (and ai-chat--current-session-start-time ai-chat--current-session-id)
    (expand-file-name
     (format "%s_%s.el"
             (format-time-string "%Y-%m-%dT%H-%M-%S" ai-chat--current-session-start-time)
             ai-chat--current-session-id)
     ai-chat--history-directory)))

(defun ai-chat--start-new-session ()
  "Start a new chat session with timestamp and unique ID."
  (setq ai-chat--current-session-start-time (current-time))
  (setq ai-chat--current-session-id (ai-chat--generate-session-id))
  (ai-chat--clear-buffer-history))

(defun ai-chat--initialize-session ()
  "Initialize a new chat session if none exists."
  (unless (and ai-chat--current-session-start-time ai-chat--current-session-id)
    (ai-chat--start-new-session)))

(defun ai-chat--save-session-context ()
  "Save the current chat session context to a file."
  (interactive)
  (ai-chat--ensure-history-directory)
  (when-let ((filename (ai-chat--get-session-filename)))
    (let* ((buffer-history ai-chat--buffer-history)
           (buffer-context-size ai-chat--buffer-context-size)
           (session-start-time ai-chat--current-session-start-time)
           (session-id ai-chat--current-session-id)
           (global-system-prompts (ai-chat--get-global-system-prompts))
           (global-memory (ai-chat--get-global-memory))
           (buffer-bound-prompts (ai-chat--get-buffer-bound-prompts))
           (context-pool (ai-chat--get-context-pool))
           (execution-model ai-chat--execution-model))
      (with-temp-file filename
        (insert ";; AI Chat Session History\n")
        (insert (format ";; Saved on: %s\n" (current-time-string)))
        (insert (format ";; Session started: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S" session-start-time)))
        (insert (format ";; Session ID: %s\n\n" session-id))
        (insert "(setq ai-chat--session-data\n")
        (insert "  '(")
        (insert ":buffer-history ")
        (prin1 buffer-history (current-buffer))
        (insert "\n    :buffer-context-size ")
        (prin1 buffer-context-size (current-buffer))
        (insert "\n    :session-start-time ")
        (prin1 session-start-time (current-buffer))
        (insert "\n    :session-id ")
        (prin1 session-id (current-buffer))
        (insert "\n    :global-system-prompts ")
        (prin1 global-system-prompts (current-buffer))
        (insert "\n    :global-memory ")
        (prin1 global-memory (current-buffer))
        (insert "\n    :buffer-bound-prompts ")
        (prin1 buffer-bound-prompts (current-buffer))
        (insert "\n    :context-pool ")
        (prin1 context-pool (current-buffer))
        (insert "\n    :execution-model ")
        (prin1 execution-model (current-buffer))
        (insert "))\n")))
    (message "Chat session saved to: %s" filename)))

(defun ai-chat--auto-save-session ()
  "Automatically save session context after each interaction if auto-save is enabled."
  (when (and ai-chat--auto-save-enabled ai-chat--current-session-start-time ai-chat--current-session-id)
    (ai-chat--save-session-context)))

(defun ai-chat--get-history-files ()
  "Get list of available chat history files."
  (when (file-exists-p ai-chat--history-directory)
    (directory-files ai-chat--history-directory nil "\\.el$")))

(defun ai-chat--load-session-from-file (filename)
  "Load chat session context from FILENAME."
  (let ((filepath (expand-file-name filename ai-chat--history-directory)))
    (when (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (goto-char (point-min))
        (condition-case err
            (let* ((read-form (read (current-buffer)))
                   (temp-session-data-quoted (if (and (listp read-form) (eq (car read-form) 'setq)
                                                      (eq (cadr read-form) 'ai-chat--session-data))
                                                 (caddr read-form)
                                               (error "Invalid session file format")))
                   (session-data (if (and (consp temp-session-data-quoted)
                                          (eq (car temp-session-data-quoted) 'quote))
                                     (cadr temp-session-data-quoted)
                                   temp-session-data-quoted)))

              (when (plist-get session-data :buffer-history)
                (with-current-buffer (ai-chat--buffer)
                  (setq-local ai-chat--buffer-history
                              (ai-chat--reconstruct-list-of-typed-structs (plist-get session-data :buffer-history)))))

              (when (plist-get session-data :buffer-context-size)
                (setq ai-chat--buffer-context-size (plist-get session-data :buffer-context-size)))

              (when (plist-get session-data :session-start-time)
                (setq ai-chat--current-session-start-time (plist-get session-data :session-start-time)))

              (when (plist-get session-data :session-id)
                (setq ai-chat--current-session-id (plist-get session-data :session-id)))

              (when (plist-get session-data :global-system-prompts)
                (ai-chat--set-global-system-prompts
                 (ai-chat--reconstruct-list-of-typed-structs (plist-get session-data :global-system-prompts))))

              (when (plist-get session-data :global-memory)
                (ai-chat--set-global-memory
                 (ai-chat--reconstruct-list-of-typed-structs (plist-get session-data :global-memory))))

              (when (plist-get session-data :buffer-bound-prompts)
                (ai-chat--set-buffer-bound-prompts
                 (ai-chat--reconstruct-list-of-typed-structs (plist-get session-data :buffer-bound-prompts))))

              (when (plist-get session-data :context-pool)
                (ai-chat--set-context-pool
                 (ai-chat--reconstruct-single-typed-struct (plist-get session-data :context-pool))))

              (when (plist-get session-data :execution-model)
                (setq ai-chat--execution-model (plist-get session-data :execution-model)))

              (ai-chat--restore-chat-display)
              (message "Chat session loaded from: %s" filename))
          (error (message "Error loading session: %s" (error-message-string err))))))))

(defun ai-chat--restore-chat-display ()
  "Restore chat display from loaded history."
  (with-current-buffer (ai-chat--buffer)
    (let ((inhibit-read-only t)
          (process (ai-chat--process)))
      (erase-buffer)
      ;; Insert header at the top
      (insert ai-chat--header)
      ;; Set process mark after header
      (ai-chat--set-pm (point-max))

      (dolist (entry ai-chat--buffer-history)
        (let* ((content (ai-chat--get-text-content-from-struct entry))
               (type (ai-chat--get-struct-type entry)))
          (cond
           ((eq type 'user-input)
            ;; Insert prompt with proper properties and face
            (let ((prompt-start (point)))
              (insert ai-chat--prompt)
              (let ((prompt-end (point)))
                ;; Apply prompt face to the prompt text
                (add-text-properties prompt-start prompt-end
                                     (list 'font-lock-face 'comint-highlight-prompt
                                           'rear-nonsticky t
                                           'field 'output
                                           'inhibit-line-move-field-capture t))
                ;; Insert user content and newline
                (insert content "\n")
                ;; Add invisible marker
                (let ((marker-start (point)))
                  (insert "<ai--chat-end-of-prompt>")
                  (add-text-properties marker-start (point)
                                       (list 'invisible (not ai-chat--show-invisible-markers)))))))
           ((eq type 'assistant-response)
            ;; Insert assistant response with proper formatting
            (insert "\n" (string-trim content) "\n\n")))))
      ;; Insert the final prompt for new input
      (let ((final-prompt-start (point)))
        (insert ai-chat--prompt-internal)
        ;; Apply prompt face and properties to final prompt
        (add-text-properties final-prompt-start (point)
                             (list 'font-lock-face 'comint-highlight-prompt
                                   'rear-nonsticky t
                                   'field 'output
                                   'inhibit-line-move-field-capture t)))
      ;; Update process mark to end of buffer
      (ai-chat--set-pm (point-max))
      (set-marker comint-last-input-start (ai-chat--pm)))))

;;; Request handling

(defun ai-chat--request-fail-callback (request-data error-message)
  "Handle request failures and display ERROR-MESSAGE.

REQUEST-DATA is the data sent with the failed request."
  (let ((content (ai-chat--get-text-content-from-struct error-message)))
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
            (ai-chat--write-reply (ai-chat--get-text-content-from-struct entry)))))
    (error  (progn
              (setq ai-chat--busy nil)
              (ai-chat--write-reply "EMACS: Invalid request. Please, try again.")
              (error "Process chat request error: %s" (error-message-string processing-error))))))

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

;;; Mode line

(defun ai-chat-mode-line-info ()
  "Construct and return formatted mode line information for AI chat."
  (let* ((model (ai-chat--get-current-model))
         (progress-indicator (cond
                              ((and ai-chat--progress-active
                                    (eq ai-chat--progress-indicator-style 'spinner))
                               (let ((spinner-chars '("○" "◔" "◑" "◕" "●" "◕" "◑" "◔"))
                                     (elapsed-time (when ai-chat--progress-start-time
                                                     (ai-chat--format-elapsed-time ai-chat--progress-start-time))))
                                 (format "%s%s"
                                         (nth (% ai-chat--progress-counter (length spinner-chars)) spinner-chars)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              ((and ai-chat--progress-active
                                    (eq ai-chat--progress-indicator-style 'dots))
                               (let ((elapsed-time (when ai-chat--progress-start-time
                                                     (ai-chat--format-elapsed-time ai-chat--progress-start-time))))
                                 (format "%s%s"
                                         (make-string (% ai-chat--progress-counter 4) ?.)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              (ai-chat--progress-active "⚡")
                              (t "")))
         (context-info (if ai-chat--progress-active
                           (when ai-chat--progress-start-time
                             (format "%s" (if (string-empty-p progress-indicator) "" (format "%s" progress-indicator))))
                         (format "%d" (ai-chat-get-context-size))))
         (ai-chat-mode-line-section
          (format "AI-CHAT[%s|%s%s]"
                  (map-elt model :name)
                  context-info
                  (if ai-chat--auto-save-enabled "|AS" ""))))
    ai-chat-mode-line-section))

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

;;; Interactive commands

(defun ai-chat--insert-new-line ()
  "Insert a new line in AI chat."
  (interactive)
  (newline))

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
                     (context (ai-chat--get-execution-context))
                     (current-buffer (current-buffer))
                     (wrapped-success-callback (ai-chat--progress-wrap-callback 'ai-chat--request-success-callback current-buffer))
                     (wrapped-fail-callback (ai-chat--progress-wrap-callback 'ai-chat--request-fail-callback current-buffer)))

                ;; Start progress indicator
                (ai-chat--progress-start (format "Chatting with %s" (map-elt execution-model :name)) current-buffer)

                (funcall execution-backend
                         context
                         execution-model
                         :success-callback wrapped-success-callback
                         :fail-callback wrapped-fail-callback))

            (error (error  "Evaluation input error: %s" (error-message-string processing-error) ))))))
    (error "AI busy")))

(defun ai-chat--clear-buffer ()
  "Clear the AI chat buffer."
  (interactive)
  (with-current-buffer (ai-chat--buffer)
    (let ((inhibit-read-only t)
          (process (ai-chat--process)))
      (erase-buffer)
      ;; Insert header at the top after clearing
      (insert ai-chat--header)
      ;; Set process mark after header
      (ai-chat--set-pm (point-max))
      ;; Insert prompt for next input
      (comint-output-filter process ai-chat--prompt-internal)
      ;; Start new session
      (ai-chat--start-new-session))))

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
    (ai-chat--progress-stop)
    (setq ai-chat--busy nil)))

(defun ai-chat-change-backend (&optional model-name)
  "Change the chat query backend to MODEL-NAME."
  (interactive)
  (let* ((names (mapcar (lambda (item) `(,(map-elt item :name) ,item)) (ai-chat--get-models)))
         (value (or model-name (completing-read ai-chat--change-backend-prompt (mapcar #'car names))))
         (model (ai-utils--find-model-config-by-name value (ai-chat--get-models))))
    (ai-chat--set-execution-model model)
    (message (format "AI chat query async backend is changed to \"%s\"" value))))

(defun ai-chat-toggle-auto-save ()
  "Toggle automatic saving of chat sessions."
  (interactive)
  (setq ai-chat--auto-save-enabled (not ai-chat--auto-save-enabled))
  (if ai-chat--auto-save-enabled
      (progn
        (ai-chat--initialize-session)
        (message "Auto-save enabled. Sessions will be automatically saved."))
    (message "Auto-save disabled. Sessions will not be automatically saved."))
  (ai-chat-mode-update-mode-line-info))

(defun ai-chat-load-session ()
  "Load a chat session from history files."
  (interactive)
  (let ((history-files (ai-chat--get-history-files)))
    (if history-files
        (let ((selected-file (completing-read "Select chat session to load: " history-files)))
          (ai-chat--load-session-from-file selected-file))
      (message "No chat history files found in %s" ai-chat--history-directory))))

(defun ai-chat-save-session ()
  "Manually save the current chat session."
  (interactive)
  (if (and ai-chat--current-session-start-time ai-chat--current-session-id)
      (ai-chat--save-session-context)
    (progn
      (ai-chat--initialize-session)
      (ai-chat--save-session-context))))

;;; Major mode

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

  ;; Initialize new session when starting mode
  (ai-chat--initialize-session)

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

;;; Main entry point

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

;;; Hook setup

(add-hook 'ai-chat--inferior-mode-hook 'ai-chat-mode-update-mode-line-info)
(add-hook 'ai-chat-change-model-hook 'ai-chat-mode-update-mode-line-info)
(add-hook 'ai-chat-change-params-hook  'ai-chat-mode-update-mode-line-info)
(add-hook 'ai-chat-change-params-hook 'ai-chat--auto-save-session)

(provide 'ai-chat)

;;; ai-chat.el ends here
