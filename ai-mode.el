;;; ai-mode.el --- AI interaction mode for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI

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
;; AI Mode is a comprehensive Emacs package that provides seamless integration
;; with various AI engines and language models. This package transforms Emacs
;; into a powerful AI-assisted development environment.
;;
;; Features include:
;; - Intelligent code completion with context awareness
;; - Code optimization and refactoring suggestions
;; - Automated bug detection and fixing
;; - Comprehensive code documentation generation
;; - Code improvement and modernization suggestions
;; - Code elaboration and extension capabilities
;; - Interactive code explanation and analysis
;; - Conversational AI interactions within the editor
;; - Support for multiple AI backends and models
;; - Customizable prompts and instruction templates
;; - Buffer-local and global context management
;; - Real-time preview of AI suggestions
;;
;; The package supports multiple AI providers and models, allowing users to
;; choose the most suitable AI engine for their specific needs. With its
;; extensive customization options and intuitive interface, AI Mode enhances
;; productivity and code quality for developers working in any programming
;; language supported by Emacs.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(require 'ai-utils)
(require 'ai-common)
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

(defcustom ai--project-file-instructions-enabled t
  "Enable file instructions"
  :type 'boolean
  :group 'ai)

(defcustom ai--extended-instructions-enabled t
  "Enable extended instructions."
  :type 'boolean
  :group 'ai)

(defcustom ai--global-prompts-enabled t
  "Enable global prompts."
  :type 'boolean
  :group 'ai)

(defcustom ai--current-buffer-additional-context t
  "Enable additional context for the current buffer."
  :type 'boolean
  :group 'ai)

(defcustom ai--user-input-method 'ai-utils--user-input-minibuffer-with-preview
  "Function to use for collecting user input.
Should be a function symbol that returns a string or nil."
  :type '(choice (const :tag "Simple minibuffer" ai-utils--user-input)
                 (const :tag "Minibuffer with preview" ai-utils--user-input-minibuffer-with-preview)
                 (const :tag "Ctrl-Enter to send" ai-utils--user-input-ctrl-enter)
                 (function :tag "Custom function"))
  :group 'ai)

(defvar-local ai--buffer-file-instructions (make-hash-table :test 'equal))
(defvar ai-mode--actions-instructions (make-hash-table :test 'equal))

(defvar ai-mode--models-providers nil)

(defcustom ai-mode--execution-model nil
  "The current backend used to execute requests asynchronously."
  :group 'ai-mode)

(defcustom ai--current-context-size 10
  "Number of lines for context."
  :type 'integer
  :group 'ai-completions)

(defcustom ai--current-following-context-size 10
  "Following context size."
  :type 'integer
  :group 'ai-completions)

(defvar ai-mode-change-model-hook nil
  "Hook that is run when execution model changes.")

(defcustom ai-mode--base-additional-context-prompts-names
  '("basic"
    "complete"
    "_file_metadata"
    "modify_action_type_object"
    "complete_action-type_object"
    "explain_action_type_object"
    "chat-basic")
  "List of file names to load additional prompt instructions from.

These files should contain context or prompts intended to guide the AI chatbot."
  :type 'string
  :group 'ai-mode)

(defcustom ai--result-action-prompts-names
  '("result_action_replace"
    "result_action_show"
    "result_action_eval"
    "result_action_complete")
  "List of result action prompt file names to load instructions from.

These files should contain instructions for how to format and apply results."
  :type 'string
  :group 'ai-mode)

(defcustom ai--query-type-config-map
  '(("modify" . (:template "" :instructions nil :user-input t :result-action replace))
    ("generate code" . (:instructions nil :result-action replace))
    ("execute prompt inplace" . (:instructions nil :result-action replace))
    ("explain" . (:instructions nil :action-type "explain" :result-action show))
    ("explain with user input" . (:instructions nil :action-type "explain" :result-action show))
    ("doc" . (:instructions nil :result-action replace))
    ("fix" . (:instructions nil :result-action replace))
    ("simplify" . (:instructions nil :result-action replace))
    ("improve" . (:instructions nil :result-action replace))
    ("optimize" . (:instructions nil :result-action replace))
    ("spellcheck" . (:instructions nil :result-action replace)))

  "An association list that maps query types to their corresponding format strings.
   The `:result-action` key specifies the default handling of the AI's response."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (plist :tag "Format Specification"
                                   :options ((:template (string :tag "Template") :optional t)
                                             (:instructions (string :tag "Instructions") :optional t)
                                             (:user-input (boolean :tag "User input"))
                                             (:action-type (string :tag "Action Type") :optional t)
                                             (:result-action (symbol :tag "Result Action" :value-type (choice (const show) (const replace) (const eval)))))))
  :group 'ai-mode)


(defcustom ai--completion-config
  `(:action "complete" :instructions nil :action-type "complete" :result-action complete)
  "Configuration for code completion."
  :group 'ai-mode)

(defcustom ai--query-config
  `(:instructions nil)
  "Configuration for generic queries."
  :group 'ai-mode)

(defvar ai-command-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "o") 'ai-optimize-code-region)
    (define-key keymap (kbd "e") 'ai-explain-code-region)
    (define-key keymap (kbd "c c") 'ai-chat)
    (define-key keymap (kbd "b c") 'ai--change-execution-backend)
    (define-key keymap (kbd "f") 'ai--switch-file-instructions-enabled)
    (define-key keymap (kbd "p") 'ai-perform-coordinator)
    (define-key keymap (kbd "r") 'ai-perform)
    (define-key keymap (kbd "s") 'ai-show)
    (define-key keymap (kbd "x") 'ai-execute)
    (define-key keymap (kbd "b a") 'ai-common--add-buffer-bound-prompts)
    (define-key keymap (kbd "b c") 'ai-common--clear-buffer-bound-prompts)
    (define-key keymap (kbd "m a") 'ai-common--add-to-global-memory)
    (define-key keymap (kbd "m c") 'ai-common--clear-global-memory)
    (define-key keymap (kbd "a c") 'ai-common--add-to-context-pool)
    keymap)
  "Keymap for AI commands.")

(defvar ai-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when ai-keymap-prefix
      (define-key keymap (kbd ai-keymap-prefix) ai-command-map))
    keymap)
  "Keymap used by `ai-mode`.")

;;;###autoload
(define-minor-mode ai-mode
  "Minor mode for AI interaction."
  :keymap ai-mode-map
  :lighter (:eval (ai-mode-line-info))
  :group 'ai
  :after-hook (force-mode-line-update t)
  (progn
    (if ai-mode
        (progn
          (add-hook 'pre-command-hook 'ai-mode-pre-command)
          (add-hook 'post-command-hook 'ai-mode-post-command)
          (add-hook 'after-save-hook 'ai-mode--update-file-instructions))
      (remove-hook 'pre-command-hook 'ai-mode-pre-command)
      (remove-hook 'post-command-hook 'ai-mode-post-command)
      (remove-hook 'after-save-hook 'ai-mode--update-file-instructions))
    (ai-mode-update-mode-line-info)))

;;;###autoload
(define-globalized-minor-mode global-ai-mode ai-mode ai-mode-on
  :group 'ai)

(defun ai-mode-on ()
  "Turn on AI mode."
  (interactive)
  (ai-mode 1)
  (ai-mode-update-mode-line-info))

(defun ai-mode-pre-command ()
  "Function called before each command in `ai-mode`.")

(defun ai-mode-post-command ()
  "Function called after each command in `ai-mode`.")

(defun ai-mode-global-init-hook ()
  "Function that runs when `global-ai-mode` is initialized."
  (message "Global AI mode is now enabled or disabled!")
  (ai-mode-update-mode-line-info)
  (if global-ai-mode
      (progn
        (ai-mode--update-mode-instructions)
        (message "Global AI mode is now enabled!"))
    (message "Global AI mode is now disabled!")))

(add-hook 'global-ai-mode-hook 'ai-mode-global-init-hook)

(defun ai-mode--update-file-instructions ()
  "Update local file instructions for AI mode."
  (message "Updating AI mode local actions instructions...")

  (with-current-buffer (current-buffer)
    (let* ((actions (mapcar #'car ai--query-type-config-map))
           (actions (append actions ai-mode--base-additional-context-prompts-names))
           (actions (append actions ai--result-action-prompts-names))
           (root-path (ai-utils--get-buffer-root-path (current-buffer)))
           (library-root-path (file-name-directory (locate-library "ai-mode")))
           (instructions-table (make-hash-table :test 'equal)))

      (unless (equal root-path library-root-path)
        (message "Updating AI mode buffer actions instructions...")

        (dolist (action actions)
          (let* ((instruction (ai-utils--file-instructions-for-command action root-path))
                 (action-examples-file-name (format "%s_examples" action))
                 (instruction-examples (ai-utils--file-instructions-for-command action-examples-file-name root-path)))
            (when instruction
              (puthash action instruction instructions-table))
            (when instruction-examples
              (puthash (format "%s-examples" action) instruction-examples instructions-table))))

        (setq-local ai--buffer-file-instructions instructions-table)
        (message "AI mode buffer actions instructions updated!")))))

(defun ai-mode--update-mode-instructions ()
  "Update global AI mode library instructions."
  (interactive)
  (message "Updating AI mode global actions instructions...")

  (let* ((root-path (file-name-directory (locate-library "ai-mode")))
         (actions (mapcar #'car ai--query-type-config-map))
         (actions (append actions ai-mode--base-additional-context-prompts-names))
         (actions (append actions ai--result-action-prompts-names))
         (instructions-table (make-hash-table :test 'equal)))

    (message "ai-mode library path: %s" root-path)

    (dolist (action actions)
      (let* ((instruction (ai-utils--file-instructions-for-command action root-path))
             (action-examples-file-name (format "%s_examples" action))
             (instruction-examples (ai-utils--file-instructions-for-command action-examples-file-name root-path)))
        (when instruction
          (puthash action instruction instructions-table)
          (ai-utils--verbose-message "Instructions for action \"%s\" added to global actions instructions" action))
        (when instruction-examples
          (puthash (format "%s-examples" action) instruction-examples instructions-table)
          (ai-utils--verbose-message "Examples for action \"%s\" added to global actions instructions" action))))

    (setq ai-mode--actions-instructions instructions-table))
  (message "AI mode actions instructions updated!"))

(cl-defun ai--execute-context (context success-callback &key (fail-callback nil) (model nil))
  "Execute CONTEXT using SUCCESS-CALLBACK and optional FAIL-CALLBACK with an optional MODEL."
  (ai-perform-async-backend-query context success-callback :fail-callback fail-callback :model model))

(cl-defun ai--execute-command (command success-callback &key (fail-callback nil) (model nil))
  "Execute COMMAND by dispatching to the appropriate backend using SUCCESS-CALLBACK.
Optionally use FAIL-CALLBACK and specify a MODEL."
  (let* ((execution-model (if model model (ai--get-current-model)))
         (execution-backend (map-elt execution-model :execution-backend))
         (context (ai--get-executions-context-for-query-type command :model execution-model)))
    (funcall execution-backend
             context
             execution-model
             :success-callback success-callback
             :fail-callback fail-callback)))

(defun ai--get-selected-region (config full-context)
  "Return the currently selected region.
CONFIG contains configuration details and FULL-CONTEXT includes information for rendering."
  (when (region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ai--render-messages-templates (items context)
  "Render message templates from ITEMS using CONTEXT."
  (mapcar (lambda (content) (ai-utils--render-template content context)) items))

(defun ai--get-action-type-for-config (config)
  "Determine the action type from CONFIG based on result-action, defaulting to \"modify\"."
  (let* ((result-action (map-elt config :result-action))
         (action-type (ai--get-container-type-by-result-action result-action)))
    (or action-type "modify")))

(defun ai--get-container-type-by-result-action (result-action)
  "Determine the container type based on RESULT-ACTION.
Returns the container name or nil if no specific container is needed."
  (cond
   ((eq result-action 'complete)    "complete")
   ((eq result-action 'show)        "explain")
   ((eq result-action 'eval)        "eval")
   ((eq result-action 'replace)     "modify")
   (t                               nil)))

(cl-defun ai--get-contextual-action-object (config &key preceding-context-size following-context-size)
  "Generate contextual action object based on CONFIG and optional context sizes PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE."
  (let* ((action (map-elt config :action))
         (result-action (map-elt config :result-action))
         (container-type (ai--get-container-type-by-result-action result-action))
         (action-object-name (if container-type
                                 (format "%s-action-object" container-type)
                               "action-object")))

    (if (equal container-type "complete")
        (ai-common--render-container-from-elements
         action-object-name
         (ai-common--assemble-completion-context
          :preceding-context-size preceding-context-size
          :following-context-size following-context-size))
      (ai-common--render-container-from-elements
       action-object-name
       (ai-common--assemble-edit-context)))))

(defun ai--get-current-buffer-context ()
  "Get the additional context for the current buffer if enabled."
  (when ai--current-buffer-additional-context
    (ai-common--make-file-context)))

(defun ai--get-result-action-prompt (result-action context)
  "Get the prompt for RESULT-ACTION rendered with CONTEXT."
  (let ((prompt-name (format "result_action_%s" (symbol-name result-action))))
    (ai--get-rendered-action-prompt prompt-name context)))

(defun ai--get-user-input ()
  "Get user input using the configured function."
  (if (functionp ai--user-input-method)
      (funcall ai--user-input-method)
    (error "ai--user-input-method is not a valid function: %s" ai--user-input-method)))


(cl-defun ai--get-execution-context (buffer config query-type &key
                                            (preceding-context-size ai--current-context-size)
                                            (following-context-size ai--current-following-context-size)
                                            model)
  "Get full execution context for BUFFER.
CONFIG specifies configuration, QUERY-TYPE indicates the query, and options for context sizes are PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE."
  (with-current-buffer buffer
    (let* ((completion-context (ai-utils--get-completion-params
                                :preceding-context-size preceding-context-size
                                :following-context-size following-context-size))
           (buffer-context (ai-utils--get-buffer-context (current-buffer)))
           (model-context (ai-utils--get-model-context model))
           (full-context (append completion-context buffer-context model-context))

           (basic-file-prompt (ai-common--make-typed-struct
                               (ai--get-rendered-action-prompt "basic" full-context)
                               'agent-instructions
                               'basic-prompt))

           (file-metadata-context (ai-common--make-typed-struct
                                   (ai--get-rendered-action-prompt "_file_metadata" full-context)
                                   'additional-context
                                   'file-metadata))

           (action-file-prompt (ai-common--make-typed-struct
                                (ai--get-rendered-action-prompt query-type full-context)
                                'agent-instructions
                                'action-specific-prompt))

           (action-examples-prompt (ai-common--make-typed-struct
                                    (ai--get-rendered-action-prompt (format "%s-examples" query-type) full-context)
                                    'agent-instructions
                                    'action-examples))

           (action-type-object-prompt (ai-common--make-typed-struct
                                       (ai--get-action-type-object-prompt (ai--get-action-type-for-config config) full-context)
                                       'agent-instructions
                                       'action-object-rules))

           (result-action-prompt (let ((result-action (map-elt config :result-action)))
                                   (when result-action
                                     (ai-common--make-typed-struct
                                      (ai--get-result-action-prompt result-action full-context)
                                      'agent-instructions
                                      'result-action-format))))

           (action-config-prompt (when-let ((instructions (map-elt config :instructions)))
                                   (ai-common--make-typed-struct instructions 'agent-instructions 'config-instructions)))

           (additional-context
            (let ((context-pool-content (ai-common--render-struct-to-string (ai-common--get-context-pool))))
              (when context-pool-content
                (ai-common--make-typed-struct context-pool-content 'additional-context 'context-pool))))

           (current-buffer-content-context
            (when (use-region-p)
              (ai--get-current-buffer-context)))

           (user-input (when (map-elt config :user-input)
                         (let ((input-text (ai--get-user-input)))
                           (when input-text
                             (ai-common--make-typed-struct
                              (ai-utils--render-template input-text full-context)
                              'user-input
                              'user-input)))))

           (query-struct (when-let ((query-text (map-elt config :query)))
                           (ai-common--make-typed-struct query-text 'user-input 'config-query)))

           (rendered-action-context (ai-common--make-typed-struct
                                     (ai--get-contextual-action-object
                                      config
                                      :preceding-context-size preceding-context-size
                                      :following-context-size following-context-size)
                                     'action-context
                                     'contextual-action))

           (messages
            (append
             '()
             (when ai--extended-instructions-enabled
               (cl-remove-if
                #'null
                (list basic-file-prompt
                      action-type-object-prompt
                      (ai-common--get-global-system-prompts)
                      (ai-common--get-global-memory)
                      action-file-prompt
                      file-metadata-context
                      current-buffer-content-context
                      action-config-prompt
                      result-action-prompt
                      additional-context
                      (ai-common--get-buffer-bound-prompts)
                      action-examples-prompt
                      rendered-action-context
                      user-input
                      query-struct)))))

           (messages (ai-utils-filter-non-empty-content messages))
           (_ (ai-utils-write-context-to-prompt-buffer messages))
           (full-context (append full-context `(:messages ,messages))))

      full-context)))

(defun ai--get-local-query-prompt (query-type)
  "Return instruction for QUERY-TYPE from buffer-local hash table as a string."
  (gethash query-type ai--buffer-file-instructions))

(defun ai--get-default-action-prompt (query-type)
  "Return instruction for QUERY-TYPE from global actions hash table as a string."
  (gethash query-type ai-mode--actions-instructions))

(defun ai--get-action-prompt (query-type)
  "Return buffer-specific instructions for QUERY-TYPE if available.
If not available and `ai--global-prompts-enabled' is non-nil,
return the global instruction from `ai--get-default-action-prompt'."
  (or (ai--get-local-query-prompt query-type)
      (when ai--global-prompts-enabled
        (ai--get-default-action-prompt query-type))))

(defun ai--get-rendered-action-prompt (query-type context)
  "Get prompt for QUERY-TYPE, render it with CONTEXT, and return the result.
If no prompt is found for QUERY-TYPE, returns nil."
  (when-let* ((prompt-content (ai--get-action-prompt query-type)))
    (ai-utils--render-template prompt-content context)))

(defun ai--get-action-type-object-prompt (action-type context)
  "Get the prompt for ACTION-TYPE rendered with CONTEXT."
  (ai--get-rendered-action-prompt (format "%s_action_type_object" action-type) context))

(defun ai--get-query-config-by-type (query-type &optional default-result-action)
  "Get query config by QUERY-TYPE, applying DEFAULT-RESULT-ACTION for unknown types."
  (if-let (config (cdr (assoc query-type ai--query-type-config-map)))
      (append config `(:action ,query-type))
    (if (string= query-type "complete")
        ai--completion-config
      (let ((base-config (append ai--query-config `(:query ,query-type))))
        (if default-result-action
            (append base-config `(:result-action ,default-result-action))
          base-config)))))

(cl-defun ai--get-executions-context-for-query-type (query-type &key (model nil) (default-result-action nil))
  "Get execution context for QUERY-TYPE with optional MODEL and DEFAULT-RESULT-ACTION."
  (let* ((config (ai--get-query-config-by-type query-type default-result-action))
         (execution-context (ai--get-execution-context (current-buffer) config query-type :model model)))
    execution-context))

(defun ai-explain-code-region ()
  "Explain the selected code region and display the explanation in a help buffer."
  (interactive)
  (ai--execute-context (ai--get-executions-context-for-query-type "explain") 'ai-utils--show-explain-help-buffer))

(defun ai--get-query-type ()
  "Prompt the user to select the type of request using `completing-read`."
  (interactive)
  (completing-read ai--query-type-prompt (mapcar #'car ai--query-type-config-map)))

(defun ai--get-informational-query-type ()
  "Prompt the user to select an informational type of request, filtering by :result-action 'show'."
  (interactive)
  (let* ((show-query-types
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'show))
                   ai--query-type-config-map))))
    (completing-read ai--query-type-prompt show-query-types nil nil nil nil)))

(defun ai--get-executable-query-type ()
  "Prompt the user to select an executable type of request, filtering by :result-action 'eval'."
  (interactive)
  (let* ((eval-query-types
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'eval))
                   ai--query-type-config-map))))
    (completing-read ai--query-type-prompt eval-query-types nil nil nil nil)))

(defun ai--get-all-available-query-types ()
  "Get all available query types including those from config and additional actions."
  (let* ((config-types (mapcar #'car ai--query-type-config-map))
         (additional-types '("complete"))
         (all-types (append config-types additional-types)))
    (delete-dups all-types)))

(defun ai--get-query-type-unrestricted ()
  "Prompt the user to select any available query type without restrictions."
  (interactive)
  (completing-read ai--query-type-prompt (ai--get-all-available-query-types) nil nil nil nil))

(defun ai--set-execution-model (model)
  "Set the execution model and execute hooks.
MODEL is the model configuration to be set."
  (let ((setup-function (map-elt model :setup-function)))
    (when setup-function
      (funcall setup-function))
    (setq ai-mode--execution-model model)
    (run-hooks 'ai-mode-change-model-hook)
    (ai-mode-update-mode-line-info)))

(defun ai--change-execution-backend (&optional model-name)
  "Change query backend interactively, or use MODEL-NAME if given."
  (interactive)
  (let* ((models (mapcar (lambda (item) `(,(map-elt item :name) ,item)) (ai-mode--get-models)))
         (value (or model-name
                    (completing-read ai--change-backend-prompt (mapcar #'car models))))
         (model (ai-utils--find-model-config-by-name value (ai-mode--get-models))))

    (message "Setup model: %s" (pp-to-string model))
    (ai--set-execution-model model)
    (message "AI mode backend changed to '%s'." value)))

(defun ai--switch-file-instructions-enabled ()
  "Toggle file instructions for the current buffer."
  (setq ai--buffer-file-instructions (not ai--buffer-file-instructions)))

(cl-defun ai-perform-async-backend-query (context success-callback &key
                                                  (fail-callback nil)
                                                  (extra-params nil)
                                                  (model nil))
  "Execute CONTEXT by current backend asynchronously.
After successful execution, call SUCCESS-CALLBACK. If execution fails, call FAIL-CALLBACK if provided. EXTRA-PARAMS is a list of additional parameters for backend configuration."
  (let* ((execution-model (if model model (ai--get-current-model)))
         (execution-backend (map-elt execution-model :execution-backend)))
    (funcall execution-backend
             context
             execution-model
             :success-callback success-callback
             :fail-callback fail-callback)))

(defun ai-show ()
  "Execute query and show the response in a special buffer, filtering by show-compatible query types."
  (interactive)
  (let* ((query-type (ai--get-informational-query-type))
         (context (ai--get-executions-context-for-query-type query-type :default-result-action 'show)))
    (ai--execute-context context 'ai-utils--show-response-buffer)))

(defun ai-execute ()
  "Execute query and show the response for evaluation, filtering by eval-compatible query types."
  (interactive)
  (let* ((query-type (ai--get-executable-query-type))
         (context (ai--get-executions-context-for-query-type query-type :default-result-action 'eval)))
    (ai--execute-context context 'ai-utils--show-and-eval-response)))


(defun ai-perform ()
  "Execute request and apply the result based on query type's specified result action or default to replace.
   If result action is 'replace', it replaces the selected region or inserts in current buffer.
   If result action is 'show', it shows the response in a special buffer.
   If result action is 'eval', it shows the response and asks for permission to evaluate."
  (interactive)
  (let* ((query-type (ai--get-query-type-unrestricted))
         (context (ai--get-executions-context-for-query-type query-type :default-result-action 'replace))
         (config (ai--get-query-config-by-type query-type 'replace))
         (result-action (map-elt config :result-action)))
    (cond
     ((eq result-action 'show)
      ;; If the selected query type is meant to be shown, delegate
      (message "Query type '%s' is informational. Displaying in a new buffer." query-type)
      (ai--execute-context context 'ai-utils--show-response-buffer))
     ((eq result-action 'eval)
      ;; Show response and ask for permission to evaluate
      (message "Query type '%s' will generate code for evaluation." query-type)
      (ai--execute-context context 'ai-utils--show-and-eval-response))
     ((eq result-action 'replace)
      ;; Default replace behavior
      (ai--execute-context context (ai-utils--replace-region-or-insert-in-current-buffer)))
     (t
      ;; Fallback for unconfigured or new actions
      (message "Unknown or unspecified result action for query type '%s'. Defaulting to replace." query-type)
      (ai--execute-context context (ai-utils--replace-region-or-insert-in-current-buffer))))))

(defun ai-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one."
  (interactive)
  (ai-completions--coordinator :action-type (ai--get-query-type-unrestricted) :strategy 'replace))

(defun ai-debug ()
  "Debug AI mode by printing region status and execution context."
  (interactive)
  (ai-utils--show-context-debug (ai--get-executions-context-for-query-type (ai--get-query-type-unrestricted) :model (ai--get-current-model))))

(defun ai--get-current-model ()
  "Return the currently selected execution model or set a default if none is selected."
  (or ai-mode--execution-model
      (let ((default-model (car (ai-mode--get-models))))
        (ai--set-execution-model default-model)
        default-model)))

(defun ai-mode--get-models ()
  "Return a flat list of available AI models retrieved from multiple sources."
  (let ((model-funcs ai-mode--models-providers))
    (apply #'append (mapcar #'funcall model-funcs))))

(defun ai-mode-line-info ()
  "Return a formatted string describing the current AI mode state for the mode line."
  (let* ((model (ai--get-current-model))
         (ai-mode-line-section
          (format " AI[%s|%d/%d]"
                  (map-elt model :name)
                  ai--current-context-size
                  ai--current-following-context-size)))
    ai-mode-line-section))

(defun ai-mode-update-mode-line-info ()
  "Force update of the mode line to reflect current AI mode state."
  (force-mode-line-update))

(when (require 'doom-modeline nil 'noerror)

  (doom-modeline-def-segment ai-mode-line-info
    "Display AI mode line information."
    (ai-mode-line-info))

  (add-hook 'ai-mode-change-model-hook 'doom-modeline-refresh-bars)
  (add-to-list 'mode-line-misc-info  '(:eval (ai-mode-line-info)) t))

(add-hook 'ai-mode-change-model-hook 'ai-mode-update-mode-line-info)

(provide 'ai-mode)

;;; ai-mode.el ends here
