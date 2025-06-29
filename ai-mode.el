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
;; AI Mode is a comprehensive Emacs package that integrates various AI engines
;; and language models, transforming Emacs into a powerful AI-assisted
;; development environment. It offers intelligent code completion, refactoring,
;; bug detection, documentation generation, and interactive code explanations.
;; The package supports conversational AI interactions, multiple backends,
;; customizable prompts, and flexible context management, all with real-time
;; previews.
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
;; - Project-wide context analysis and indexing
;; - Progress indicators with elapsed time tracking
;; - File-based instruction system with hot-reloading
;; - Flexible memory and context pool management
;; - Configurable user input methods with preview support

;; File-based Command Modifiers:
;; Commands defined in instruction files can use modifiers in their names
;; to control behavior without code changes. Modifiers are separated by '__':
;;
;; Examples:
;; - user__explain_code__show.md     - requires user input, shows result
;; - buffer__refactor_function__replace.md - uses full buffer context, replaces selection
;; - large__smart_completion__complete.md  - uses large context for completion
;; - project__analyze_architecture__show.md - includes project context
;;
;; Available modifiers:
;;
;; Action Modifiers (control how results are handled):
;; - show      [S] - Display in a buffer
;; - eval      [E] - Display and offer to evaluate
;; - replace   [R] - Replace selection/buffer
;; - insert    [I] - Insert at cursor position
;; - complete  [C] - Code completion
;;
;; Behavior Modifiers (control input and context requirements):
;; - user      [U] - Requires user input
;; - buffer    [B] - Uses full buffer context
;; - project   [P] - Uses project context
;; - global    [G] - Uses global context
;;
;; Context Size Modifiers (control amount of surrounding context):
;; - small     [s] - Limited context (5/5 lines)
;; - large     [L] - Extended context (20/20 lines)
;; - full      [F] - Full context (unlimited)
;;
;; Display Indicators:
;; Commands in completion lists show indicators in brackets:
;; - [C]       - Configured command (from ai--commands-config-map)
;; - [D]       - Has default instructions (from package)
;; - [G]       - Has global instructions (from ~/.ai/)
;; - [L]       - Has local instructions (from project .ai/)
;; - [UBPSLEGRCIF] - Active modifiers (combinations possible)
;;
;; Example display: "[C][DG] explain" or "[UBS] user__buffer__explain_code__show"
;;
;; Installation and Setup:
;; 1. Add ai-mode to your Emacs configuration
;; 2. Configure at least one AI backend (OpenAI, Anthropic, etc.)
;; 3. Set API keys in your environment or configuration
;; 4. Enable `global-ai-mode` or `ai-mode` in specific buffers
;; 5. Use `C-c i r` to start executing AI commands
;;
;; Key Bindings (default prefix C-c i):
;; - C-c i r   - Execute AI command with replace action
;; - C-c i s   - Show AI response in separate buffer
;; - C-c i x   - Execute and evaluate AI-generated code
;; - C-c i c c - Start AI chat session
;; - C-c i b c - Change AI backend/model
;; - C-c i p c - Execute AI coordinator (smart completion)
;; - C-c i e i - Edit command instructions
;; - C-c i m c - Create command with modifiers
;; - C-c i p s - Switch project context mode
;; - C-c i p u - Update project files summary index
;;
;; Project Context Modes:
;; - disabled: No project context included
;; - full-project: Include all filtered project files
;; - project-ai-summary: Use cached AI-generated summaries
;;
;; Instruction File Hierarchy:
;; 1. Local project instructions (.ai/commands/ in project root)
;; 2. Global user instructions (~/.ai/commands/)
;; 3. Default package instructions (built-in templates)
;;
;; Memory and Context Management:
;; - Global memory: ~/.ai/memory.md (persistent across sessions)
;; - Local memory: .ai/memory.md (project-specific)
;; - Context pool: temporary additional context for current session
;; - Buffer-bound prompts: buffer-local instruction additions

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

(defcustom ai--prompt-caching-enabled nil
  "Enable prompt caching for AI requests.
When enabled, AI providers that support prompt caching will evaluate
content for caching based on the provider's specific rules."
  :type 'boolean
  :group 'ai-mode)

(defcustom ai--command-prompt "Command or query: "
  "Prompt for selecting the command."
  :type 'string
  :group 'ai-mode)

(defcustom ai--change-execution-backend-prompt "Select execution backend: "
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

(defcustom ai--project-context-mode 'disabled
  "Project context inclusion mode for AI execution context.
Controls how project-wide context is included in AI requests:
- `disabled': No project context is included
- `full-project': Include all filtered project files as context
- `project-ai-summary': Include project files summary from cached index"
  :type '(choice (const :tag "Disabled" disabled)
                 (const :tag "Full Project Files" full-project)
                 (const :tag "Project AI Summary from Index" project-ai-summary))
  :group 'ai)

(defcustom ai--user-input-method 'ai-utils--user-input-minibuffer-with-preview
  "Function to use for collecting user input.
Should be a function symbol that returns a string or nil."
  :type '(choice (const :tag "Simple minibuffer" ai-utils--user-input)
                 (const :tag "Minibuffer with preview" ai-utils--user-input-minibuffer-with-preview)
                 (const :tag "Ctrl-Enter to send" ai-utils--user-input-ctrl-enter)
                 (function :tag "Custom function"))
  :group 'ai)

(defcustom ai--progress-indicator-enabled t
  "Enable progress indicator for AI requests."
  :type 'boolean
  :group 'ai)

(defcustom ai--progress-indicator-style 'spinner
  "Style of progress indicator to use for AI requests."
  :type '(choice (const :tag "Spinner animation" spinner)
                 (const :tag "Progress dots" dots)
                 (const :tag "Message only" message))
  :group 'ai)

(defcustom ai--instruction-file-extension ".md"
  "File extension used for instruction files."
  :type 'string
  :group 'ai)

(defcustom ai--instruction-watching-enabled t
  "Enable file watching for instruction files to auto-reload cache."
  :type 'boolean
  :group 'ai)

(defcustom ai--indexing-include-existing-context t
  "Include context from already indexed files when indexing new files.
When enabled, the indexing process will include summaries of previously
indexed files to ensure consistent format and improve contextual understanding."
  :type 'boolean
  :group 'ai)

(defcustom ai--indexing-strategy 'parallel-independent
  "Strategy for file indexing process.
Controls how files are processed during project indexing:
- `parallel-independent': Parallel processing without context sharing between files
- `sequential': Sequential processing with accumulating context from current session only"
  :type '(choice (const :tag "Parallel processing without context sharing" parallel-independent)
                 (const :tag "Sequential with session context accumulation" sequential))
  :group 'ai)

(defcustom ai--index-retention-depth 5
  "Number of index versions to retain before cleanup.
When exceeded, older index versions will be automatically deleted."
  :type 'integer
  :group 'ai)

(defcustom ai--file-command-action-modifiers
  '(("show" . show)
    ("eval" . eval)
    ("replace" . replace)
    ("insert" . insert-at-point)
    ("complete" . complete))
  "Mapping of action modifier names to result-action symbols for file-based commands.
These modifiers can be used in file command names to specify the result action."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (symbol :tag "Result Action"))
  :group 'ai-mode)

(defcustom ai--file-command-behavior-modifiers
  '(("user" . (:user-input t))
    ("buffer" . (:needs-buffer-context t))
    ("project" . (:needs-project-context t))
    ("global" . (:needs-global-context t)))
  "Mapping of behavior modifier names to configuration plists for file-based commands.
These modifiers control command execution behavior."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (plist :tag "Configuration"))
  :group 'ai-mode)

(defcustom ai--file-command-context-modifiers
  '(("full" . (:preceding-context-size nil :following-context-size nil))
    ("small" . (:preceding-context-size 5 :following-context-size 5))
    ("large" . (:preceding-context-size 20 :following-context-size 20)))
  "Mapping of context modifier names to context size configuration for file-based commands.
These modifiers control the amount of context included with commands."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (plist :tag "Context Configuration"))
  :group 'ai-mode)

(defvar ai--progress-spinner-chars '("○" "◔" "◑" "◕" "●" "○" "◔" "◑" "◕" "●")
  "Characters used for spinner animation in progress indicator.")

;; Instruction cache variables
(defvar ai--default-instructions-cache (make-hash-table :test 'equal)
  "Cache for default instruction files from ai-mode package.")

(defvar ai--global-instructions-cache (make-hash-table :test 'equal)
  "Cache for global instruction files from ~/.ai/.")

(defvar-local ai--local-instructions-cache (make-hash-table :test 'equal)
  "Buffer-local cache for project instruction files.")

;; System prompt cache variables
(defvar ai--default-system-prompts-cache (make-hash-table :test 'equal)
  "Cache for default system prompt files from ai-mode package.")

(defvar ai--global-system-prompts-cache (make-hash-table :test 'equal)
  "Cache for global system prompt files from ~/.ai/.")

(defvar-local ai--local-system-prompts-cache (make-hash-table :test 'equal)
  "Buffer-local cache for project system prompt files.")

(defvar ai--instruction-file-watchers (make-hash-table :test 'equal)
  "Hash table storing file watchers for instruction directories.")

(defvar ai--instruction-directory-mtimes (make-hash-table :test 'equal)
  "Hash table storing modification times for instruction directories.")

(defvar ai-mode--models-providers nil)

(defcustom ai-mode--execution-model nil
  "The current backend used to execute requests asynchronously."
  :group 'ai-mode)

(defcustom ai--current-precending-context-size 10
  "Number of lines for context."
  :type 'integer
  :group 'ai-completions)

(defcustom ai--current-forwarding-context-size 10
  "Following context size."
  :type 'integer
  :group 'ai-completions)

(defcustom ai--indexing-call-timeout 0.1
  "Timeout in seconds between individual file indexing calls during project summary generation.
Set to 0 for no delay."
  :type 'number
  :group 'ai)

(defvar ai-mode-change-model-hook nil
  "Hook that is run when execution model changes.")

;; Project files summary index
(defvar ai--project-files-summary-index (make-hash-table :test 'equal)
  "Cached index of project files summary structures.
Maps project root paths to lists of file summary structs.")

;; Index persistence variables
(defvar ai--persistent-index-metadata (make-hash-table :test 'equal)
  "Metadata for persistent index versions.
Maps project root paths to plists containing version information.")

;; Progress indicator variables (buffer-local)
(defvar-local ai--progress-timer nil
  "Timer for progress indicator animation.")

(defvar-local ai--progress-counter 0
  "Counter for progress indicator animation.")

(defvar-local ai--progress-active nil
  "Flag indicating if progress indicator is currently active.")

(defvar-local ai--progress-message "AI request in progress"
  "Message to display during AI request progress.")

(defvar-local ai--progress-start-time nil
  "Start time of the current AI request.")

(defcustom ai--commands-config-map
  '(("modify" . (:template "" :instructions nil :user-input t :result-action replace))
    ("generate code from selection" . (:instructions nil :result-action replace))
    ("generate code from user input" . (:instructions nil :user-input t :result-action insert-at-point :needs-buffer-context t))
    ("create from user input" . (:instructions nil :user-input t :result-action insert-at-point :needs-buffer-context t))
    ("create from selection" . (:instructions nil :result-action replace))
    ("execute prompt inplace" . (:instructions nil :result-action replace))
    ("explain" . (:instructions nil :result-action show))
    ("explain with full context" . (:instructions nil :user-input t :result-action show :needs-buffer-context t))
    ("explain with user input" . (:instructions nil :user-input t :result-action show))
    ("doc" . (:instructions nil :result-action replace))
    ("fix" . (:instructions nil :result-action replace))
    ("simplify" . (:instructions nil :result-action replace))
    ("improve" . (:instructions nil :result-action replace))
    ("optimize" . (:instructions nil :result-action replace))
    ("spellcheck" . (:instructions nil :result-action replace))
    ("index file" . (:instructions nil :result-action show))

    ("answer" . (:instructions nil :user-input t :result-action show ))
    ("add to memory" . (:instructions nil :result-action replace))
    ("complete" . (:instructions nil :action-type "complete" :result-action complete)))

  "An association list mapping AI commands to their configurations.
Each entry is a pair: `(COMMAND . CONFIG-PLIST)`.

`COMMAND` is a string (e.g., \"modify\", \"explain\").
`CONFIG-PLIST` is a property list with the following keys:
- `:template` (string, optional): A template string for the command.
- `:instructions` (string, optional): Specific instructions for the AI for this command.
- `:user-input` (boolean): `t` if the command requires additional user input.
- `:action-type` (string, optional): The high-level action type (e.g., \"modify\", \"complete\") used for structuring the request.
- `:result-action` (symbol): Specifies how the AI's response should be handled.
  Possible values: `show` (display in a buffer), `replace` (replace selection/buffer),
  `eval` (display and offer to evaluate), `insert-at-point` (insert at cursor).
- `:needs-buffer-context` (boolean, optional): `t` if the full buffer content
  is required for context, even if a region is active."
  :type '(alist :key-type (string :tag "Command")
                :value-type (plist :tag "Format Specification"
                                   :options ((:template (string :tag "Template") :optional t)
                                             (:instructions (string :tag "Instructions") :optional t)
                                             (:user-input (boolean :tag "User input"))
                                             (:action-type (string :tag "Action Type") :optional t)
                                             (:result-action (symbol :tag "Result Action" :value-type (choice (const show) (const replace) (const eval) (const insert-at-point))))
                                             (:needs-buffer-context (boolean :tag "Needs Buffer Context") :optional t))))
  :group 'ai-mode)

(defcustom ai--completion-config
  `(:action "complete" :instructions nil :action-type "complete" :result-action complete)
  "Configuration for code completion."
  :group 'ai-mode)

(defcustom ai--command-config
  `(:instructions nil)
  "Configuration for generic commands."
  :group 'ai-mode)

(defvar ai-command-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "c c") 'ai-chat)
    (define-key keymap (kbd "b c") 'ai--change-execution-backend)
    (define-key keymap (kbd "f") 'ai--switch-file-instructions-enabled)
    (define-key keymap (kbd "p c") 'ai-perform-coordinator)
    (define-key keymap (kbd "r") 'ai-perform)
    (define-key keymap (kbd "s") 'ai-show)
    (define-key keymap (kbd "x") 'ai-execute)
    (define-key keymap (kbd "b a") 'ai-common--add-buffer-bound-prompts)
    (define-key keymap (kbd "b c") 'ai-common--clear-buffer-bound-prompts)
    (define-key keymap (kbd "m a") 'ai-common--add-to-global-memory)
    (define-key keymap (kbd "m c") 'ai-common--clear-global-memory)
    (define-key keymap (kbd "a c") 'ai-common--add-to-context-pool)
    (define-key keymap (kbd "p s") 'ai--switch-project-context-mode)
    (define-key keymap (kbd "p u") 'ai--update-project-files-summary-index)
    (define-key keymap (kbd "e i") 'ai-edit-command-instructions)
    (define-key keymap (kbd "m d") 'ai-describe-command-modifiers)
    (define-key keymap (kbd "m c") 'ai-create-modified-command)
    (define-key keymap (kbd "i t") 'ai--toggle-indexing-context)
    (define-key keymap (kbd "i r") 'ai--reindex-project-with-context)
    (define-key keymap (kbd "i s") 'ai--switch-indexing-strategy)
    (define-key keymap (kbd "i v") 'ai--select-index-version)
    (define-key keymap (kbd "i l") 'ai--list-index-versions)
    (define-key keymap (kbd "i d") 'ai--delete-old-index-versions)
    (define-key keymap (kbd "c t") 'ai--toggle-prompt-caching)
    keymap)
  "Keymap for AI commands.")

(defvar ai-mode-map
  (let ((keymap (make-sparse-keymap)))
    (when ai-keymap-prefix
      (define-key keymap (kbd ai-keymap-prefix) ai-command-map))
    keymap)
  "Keymap used by `ai-mode`.")

;; Instruction management system

(defun ai--get-default-instructions-directory ()
  "Get the default instructions directory from the ai-mode package."
  (file-name-as-directory
   (expand-file-name ".ai/commands"
                     (file-name-directory (locate-library "ai-mode")))))

(defun ai--get-global-instructions-directory ()
  "Get the global instructions directory from user's home."
  (file-name-as-directory
   (expand-file-name ".ai/commands" "~")))

(defun ai--get-local-instructions-directory ()
  "Get the local instructions directory from current project root."
  (when-let ((project-root (ai-common--get-project-root)))
    (file-name-as-directory
     (expand-file-name ".ai/commands" project-root))))

;; System prompt management system

(defun ai--get-default-system-prompts-directory ()
  "Get the default system prompts directory from the ai-mode package."
  (file-name-as-directory
   (expand-file-name ".ai/system"
                     (file-name-directory (locate-library "ai-mode")))))

(defun ai--get-global-system-prompts-directory ()
  "Get the global system prompts directory from user's home."
  (file-name-as-directory
   (expand-file-name ".ai/system" "~")))

(defun ai--get-local-system-prompts-directory ()
  "Get the local system prompts directory from current project root."
  (when-let ((project-root (ai-common--get-project-root)))
    (file-name-as-directory
     (expand-file-name ".ai/system" project-root))))

;; System prompt normalization functions

(defun ai--normalize-prompt-name (name)
  "Normalize prompt NAME by converting spaces to underscores for consistent lookup.
This ensures both 'modify_action_type_object' and 'modify action type object'
map to the same cache key."
  (when name
    (replace-regexp-in-string "[[:space:]]" "_" name)))

(defun ai--normalize-prompt-name-for-cache (name)
  "Normalize prompt NAME for cache storage, ensuring consistent keys."
  (ai--normalize-prompt-name name))

;; Index persistence management

(defun ai--get-project-index-directory (project-root)
  "Get the index directory for PROJECT-ROOT."
  (file-name-as-directory (expand-file-name ".ai/index" project-root)))

(defun ai--generate-index-version-name (start-time end-time)
  "Generate a version name from START-TIME and END-TIME.
Format: start_YYYYMMDD-HHMMSS_end_YYYYMMDD-HHMMSS"
  (let ((start-str (format-time-string "%Y%m%d-%H%M%S" start-time))
        (end-str (format-time-string "%Y%m%d-%H%M%S" end-time)))
    (format "start_%s_end_%s" start-str end-str)))

(defun ai--parse-index-version-name (version-name)
  "Parse VERSION-NAME and return (start-time . end-time) or nil if invalid."
  (when (string-match "start_\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)_end_\\([0-9]\\{8\\}-[0-9]\\{6\\}\\)" version-name)
    (let ((start-str (match-string 1 version-name))
          (end-str (match-string 2 version-name)))
      (condition-case nil
          (let ((start-iso (concat (substring start-str 0 4) "-"    ; YYYY-
                                  (substring start-str 4 6) "-"     ; MM-
                                  (substring start-str 6 8) "T"     ; DDT
                                  (substring start-str 9 11) ":"    ; HH:
                                  (substring start-str 11 13) ":"   ; MM:
                                  (substring start-str 13 15)))     ; SS
                (end-iso (concat (substring end-str 0 4) "-"        ; YYYY-
                                (substring end-str 4 6) "-"         ; MM-
                                (substring end-str 6 8) "T"         ; DDT
                                (substring end-str 9 11) ":"        ; HH:
                                (substring end-str 11 13) ":"       ; MM:
                                (substring end-str 13 15))))        ; SS
            (cons (date-to-time start-iso)
                  (date-to-time end-iso)))
        (error nil)))))

(defun ai--get-index-version-directory (project-root version-name)
  "Get the directory path for a specific index VERSION-NAME in PROJECT-ROOT."
  (file-name-as-directory
   (expand-file-name version-name (ai--get-project-index-directory project-root))))

(defun ai--get-index-metadata-file (project-root version-name)
  "Get the metadata file path for index VERSION-NAME in PROJECT-ROOT."
  (expand-file-name "metadata.json" (ai--get-index-version-directory project-root version-name)))

(defun ai--get-index-mapping-file (project-root version-name)
  "Get the mapping file path for index VERSION-NAME in PROJECT-ROOT."
  (expand-file-name "mapping.json" (ai--get-index-version-directory project-root version-name)))

(defun ai--generate-index-file-id (relative-path)
  "Generate a unique file ID for RELATIVE-PATH.
Returns a hash-based identifier."
  (let ((hash (secure-hash 'sha256 relative-path)))
    (substring hash 0 16)))

(defun ai--get-index-file-path (project-root version-name file-id)
  "Get the path for an index file with FILE-ID in VERSION-NAME for PROJECT-ROOT."
  (expand-file-name (format "%s.json" file-id)
                    (ai--get-index-version-directory project-root version-name)))

(defun ai--save-index-metadata (project-root version-name metadata)
  "Save METADATA for index VERSION-NAME in PROJECT-ROOT."
  (let ((metadata-file (ai--get-index-metadata-file project-root version-name))
        (version-dir (ai--get-index-version-directory project-root version-name)))
    (unless (file-directory-p version-dir)
      (make-directory version-dir t))
    (with-temp-file metadata-file
      (insert (json-encode metadata)))))

(defun ai--load-index-metadata (project-root version-name)
  "Load metadata for index VERSION-NAME in PROJECT-ROOT.
Returns nil if metadata file doesn't exist or is invalid."
  (let ((metadata-file (ai--get-index-metadata-file project-root version-name)))
    (when (file-readable-p metadata-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents metadata-file)
            (json-read))
        (error nil)))))

(defun ai--save-index-mapping (project-root version-name mapping)
  "Save file MAPPING for index VERSION-NAME in PROJECT-ROOT.
MAPPING is an alist of (relative-path . file-id) pairs."
  (let ((mapping-file (ai--get-index-mapping-file project-root version-name))
        (version-dir (ai--get-index-version-directory project-root version-name)))
    (unless (file-directory-p version-dir)
      (make-directory version-dir t))
    (with-temp-file mapping-file
      (insert (json-encode mapping)))))

(defun ai--load-index-mapping (project-root version-name)
  "Load file mapping for index VERSION-NAME in PROJECT-ROOT.
Returns nil if mapping file doesn't exist or is invalid."
  (let ((mapping-file (ai--get-index-mapping-file project-root version-name)))
    (when (file-readable-p mapping-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents mapping-file)
            (json-read))
        (error nil)))))

(defun ai--save-index-file (project-root version-name file-id summary-struct)
  "Save SUMMARY-STRUCT as FILE-ID in VERSION-NAME for PROJECT-ROOT."
  (let ((index-file (ai--get-index-file-path project-root version-name file-id)))
    (with-temp-file index-file
      (insert (json-encode summary-struct)))))

(defun ai--load-index-file (project-root version-name file-id)
  "Load summary struct for FILE-ID in VERSION-NAME for PROJECT-ROOT.
Returns nil if file doesn't exist or is invalid."
  (let ((index-file (ai--get-index-file-path project-root version-name file-id)))
    (when (file-readable-p index-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents index-file)
            (json-read))
        (error nil)))))

(defun ai--get-available-index-versions (project-root)
  "Get list of available index versions for PROJECT-ROOT.
Returns a list of version names sorted by creation time (newest first)."
  (let ((index-dir (ai--get-project-index-directory project-root)))
    (when (file-directory-p index-dir)
      (let ((versions nil))
        (dolist (entry (directory-files index-dir nil "^start_.*_end_.*$"))
          (when (file-directory-p (expand-file-name entry index-dir))
            (push entry versions)))
        ;; Sort by creation time (newest first)
        (sort versions (lambda (a b)
                         (let ((time-a (ai--parse-index-version-name a))
                               (time-b (ai--parse-index-version-name b)))
                           (when (and time-a time-b)
                             (time-less-p (cdr time-b) (cdr time-a))))))))))

(defun ai--format-index-version-display (version-name)
  "Format VERSION-NAME for display in completing-read."
  (if-let ((times (ai--parse-index-version-name version-name)))
      (let ((start-time (car times))
            (end-time (cdr times)))
        (format "%s → %s (%s)"
                (format-time-string "%Y-%m-%d %H:%M:%S" start-time)
                (format-time-string "%Y-%m-%d %H:%M:%S" end-time)
                version-name))
    version-name))

(defun ai--save-current-index-to-disk (project-root start-time)
  "Save current in-memory index to disk for PROJECT-ROOT.
START-TIME is when the indexing process began."
  (when-let ((summaries (gethash project-root ai--project-files-summary-index)))
    (let* ((end-time (current-time))
           (version-name (ai--generate-index-version-name start-time end-time))
           (mapping nil)
           (metadata `((version . ,version-name)
                      (project-root . ,project-root)
                      (start-time . ,(format-time-string "%Y-%m-%dT%H:%M:%S" start-time))
                      (end-time . ,(format-time-string "%Y-%m-%dT%H:%M:%S" end-time))
                      (file-count . ,(length summaries))
                      (strategy . ,(symbol-name ai--indexing-strategy)))))

      ;; Create version directory
      (let ((version-dir (ai--get-index-version-directory project-root version-name)))
        (make-directory version-dir t))

      ;; Save each summary file and build mapping
      (dolist (summary summaries)
        (let* ((relative-path (plist-get summary :relative-path))
               (file-id (ai--generate-index-file-id relative-path)))
          (ai--save-index-file project-root version-name file-id summary)
          (push (cons relative-path file-id) mapping)))

      ;; Save metadata and mapping
      (ai--save-index-metadata project-root version-name metadata)
      (ai--save-index-mapping project-root version-name mapping)

      (message "Index version '%s' saved to disk with %d files" version-name (length summaries))
      version-name)))

(defun ai--load-index-version (project-root version-name)
  "Load index VERSION-NAME from disk for PROJECT-ROOT into memory.
Returns the loaded summaries list or nil if loading fails."
  (when-let* ((mapping (ai--load-index-mapping project-root version-name))
              (metadata (ai--load-index-metadata project-root version-name)))
    (let ((summaries nil)
          (loaded-count 0))
      (dolist (mapping-entry mapping)
        (let* ((relative-path (car mapping-entry))
               (file-id (cdr mapping-entry))
               (summary (ai--load-index-file project-root version-name file-id)))
          (when summary
            (push summary summaries)
            (setq loaded-count (1+ loaded-count)))))

      (when summaries
        ;; Update in-memory index
        (puthash project-root summaries ai--project-files-summary-index)
        (message "Loaded index version '%s' with %d/%d files"
                 version-name loaded-count (length mapping))
        summaries))))

(defun ai--delete-index-version (project-root version-name)
  "Delete index VERSION-NAME for PROJECT-ROOT from disk."
  (let ((version-dir (ai--get-index-version-directory project-root version-name)))
    (when (file-directory-p version-dir)
      (delete-directory version-dir t)
      (message "Deleted index version: %s" version-name))))

(defun ai--cleanup-old-index-versions (project-root)
  "Delete old index versions beyond the retention depth for PROJECT-ROOT."
  (let ((versions (ai--get-available-index-versions project-root)))
    (when (> (length versions) ai--index-retention-depth)
      (let ((versions-to-delete (nthcdr ai--index-retention-depth versions)))
        (dolist (version versions-to-delete)
          (ai--delete-index-version project-root version))
        (message "Cleaned up %d old index versions" (length versions-to-delete))))))

(defun ai--select-index-version ()
  "Interactively select and load an index version for the current project."
  (interactive)
  (if-let ((project-root (ai-common--get-project-root)))
      (let ((versions (ai--get-available-index-versions project-root)))
        (if versions
            (let* ((version-displays (mapcar #'ai--format-index-version-display versions))
                   (version-alist (cl-mapcar #'cons version-displays versions))
                   (selected-display (completing-read "Select index version: " version-displays))
                   (selected-version (cdr (assoc selected-display version-alist))))
              (when selected-version
                (ai--load-index-version project-root selected-version)))
          (message "No index versions found for project")))
    (message "Not in a project. Cannot select index version.")))

(defun ai--list-index-versions ()
  "List all available index versions for the current project."
  (interactive)
  (if-let ((project-root (ai-common--get-project-root)))
      (let ((versions (ai--get-available-index-versions project-root)))
        (if versions
            (with-output-to-temp-buffer "*AI Index Versions*"
              (princ (format "Index versions for project: %s\n\n" project-root))
              (dolist (version versions)
                (princ (format "• %s\n" (ai--format-index-version-display version)))
                (when-let ((metadata (ai--load-index-metadata project-root version)))
                  (princ (format "  Strategy: %s, Files: %s\n"
                                (alist-get 'strategy metadata "unknown")
                                (alist-get 'file-count metadata "unknown"))))))
          (message "No index versions found for project")))
    (message "Not in a project. Cannot list index versions.")))

(defun ai--delete-old-index-versions ()
  "Interactively delete old index versions beyond retention depth."
  (interactive)
  (if-let ((project-root (ai-common--get-project-root)))
      (let ((versions (ai--get-available-index-versions project-root)))
        (if (> (length versions) ai--index-retention-depth)
            (let ((versions-to-delete (nthcdr ai--index-retention-depth versions)))
              (when (yes-or-no-p (format "Delete %d old index versions? " (length versions-to-delete)))
                (ai--cleanup-old-index-versions project-root)))
          (message "No old index versions to delete (current: %d, retention: %d)"
                   (length versions) ai--index-retention-depth)))
    (message "Not in a project. Cannot delete index versions.")))

(defun ai--toggle-prompt-caching ()
  "Toggle prompt caching for AI requests."
  (interactive)
  (setq ai--prompt-caching-enabled (not ai--prompt-caching-enabled))
  (customize-save-variable 'ai--prompt-caching-enabled ai--prompt-caching-enabled)
  (message "Prompt caching %s"
           (if ai--prompt-caching-enabled "enabled" "disabled")))

(defun ai--command-name-to-filename (command-name)
  "Convert COMMAND-NAME to a filesystem-safe filename with extension.
Returns nil if COMMAND-NAME is nil."
  (when command-name
    (concat (replace-regexp-in-string "[[:space:]]" "_" command-name) ai--instruction-file-extension)))

(defun ai--has-modifier-pattern-p (filename)
  "Check if FILENAME contains modifier patterns (double underscores).
Returns t if the filename appears to contain modifiers, nil otherwise."
  (let ((base-name (file-name-sans-extension filename)))
    (string-match-p "__" base-name)))

(defun ai--is-modifier-keyword-p (part)
  "Check if PART is a known modifier keyword.
Returns t if PART matches any known modifier from action, behavior, or context modifiers."
  (or (assoc part ai--file-command-action-modifiers)
      (assoc part ai--file-command-behavior-modifiers)
      (assoc part ai--file-command-context-modifiers)))

(defun ai--filename-to-command-name (filename)
  "Convert FILENAME back to command name intelligently.
For files with modifier patterns, preserve the original structure with __.
For simple files, convert underscores to spaces for backward compatibility."
  (let ((base-name (file-name-sans-extension filename)))
    (if (ai--has-modifier-pattern-p filename)
        ;; File has modifier patterns - preserve __ structure
        ;; but convert single _ to spaces only in non-modifier parts
        (let ((parts (split-string base-name "__")))
          (mapconcat (lambda (part)
                       (if (ai--is-modifier-keyword-p part)
                           part  ; Keep modifier keywords unchanged
                         (replace-regexp-in-string "_" " " part)))  ; Convert _ to space in command parts
                     parts "__"))
      ;; Simple file without modifiers - convert all underscores to spaces
      (replace-regexp-in-string "_" " " base-name))))

(defun ai--get-instruction-file-path (command-name directory)
  "Get the full path to instruction file for COMMAND-NAME in DIRECTORY.
Returns nil if COMMAND-NAME is nil or DIRECTORY is nil."
  (when (and command-name directory)
    (when-let ((filename (ai--command-name-to-filename command-name)))
      (expand-file-name filename directory))))

(defun ai--get-command-examples-file-path (command-name directory)
  "Get path to examples file for COMMAND-NAME in DIRECTORY.
Returns nil if COMMAND-NAME is nil or DIRECTORY is nil."
  (when (and command-name directory)
    (let* ((base-name (ai--get-base-command-name-for-examples command-name))
           (examples-filename (format "%s.examples%s" base-name ai--instruction-file-extension)))
      (expand-file-name examples-filename directory))))

(defun ai--get-base-command-name-for-examples (command-name)
  "Extract base command name for finding examples, handling modifiers.
Convert spaces to underscores for file lookup."
  (let* ((parsed (ai--parse-command-modifiers command-name))
         (base-name (car parsed)))
    ;; Convert spaces back to underscores for file lookup
    (replace-regexp-in-string "[[:space:]]" "_" base-name)))

(defun ai--read-instruction-file (file-path)
  "Read instruction content from FILE-PATH. Returns nil if file doesn't exist or isn't readable."
  (when (and file-path (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(defun ai--get-directory-modification-time (directory)
  "Get the modification time of DIRECTORY. Returns nil if directory doesn't exist."
  (when (file-directory-p directory)
    (file-attribute-modification-time (file-attributes directory))))

(defun ai--directory-needs-cache-update-p (directory cache-key)
  "Check if DIRECTORY needs cache update based on modification time."
  (when-let ((current-mtime (ai--get-directory-modification-time directory)))
    (let ((cached-mtime (gethash cache-key ai--instruction-directory-mtimes)))
      (not (and cached-mtime (time-equal-p current-mtime cached-mtime))))))

(defun ai--scan-instruction-files-in-directory (directory)
  "Scan DIRECTORY for instruction files and return list of command names.
Excludes .examples.md files from the command list."
  (when (file-directory-p directory)
    (let ((files (directory-files directory nil (concat "\\." (regexp-quote (substring ai--instruction-file-extension 1)) "$")))
          (commands nil))
      (dolist (file files)
        ;; Skip .examples.md files
        (unless (string-match-p "\\.examples\\." file)
          (push (ai--filename-to-command-name file) commands)))
      commands)))

(defun ai--update-instructions-cache (directory cache-table cache-key)
  "Update CACHE-TABLE with instructions from DIRECTORY. CACHE-KEY is used for modification time tracking."
  (when (file-directory-p directory)
    (clrhash cache-table)
    (let ((commands (ai--scan-instruction-files-in-directory directory)))
      (dolist (command commands)
        (when-let ((content (ai--read-instruction-file
                            (ai--get-instruction-file-path command directory))))
          (puthash command content cache-table))))
    ;; Update modification time
    (puthash cache-key (ai--get-directory-modification-time directory) ai--instruction-directory-mtimes)
    (ai-utils--verbose-message "Updated instruction cache for %s (%d commands)" cache-key (hash-table-count cache-table))))

(defun ai--update-system-prompts-cache (directory cache-table cache-key)
  "Update CACHE-TABLE with system prompts from DIRECTORY using normalized names as keys."
  (when (file-directory-p directory)
    (clrhash cache-table)
    (let ((prompts (ai--scan-instruction-files-in-directory directory)))
      (dolist (prompt prompts)
        (when-let ((content (ai--read-instruction-file
                            (ai--get-instruction-file-path prompt directory))))
          ;; Use normalized name as cache key
          (let ((normalized-key (ai--normalize-prompt-name-for-cache prompt)))
            (puthash normalized-key content cache-table)))))
    ;; Update modification time
    (puthash cache-key (ai--get-directory-modification-time directory) ai--instruction-directory-mtimes)
    (ai-utils--verbose-message "Updated system prompts cache for %s (%d prompts)" cache-key (hash-table-count cache-table))))

(defun ai--ensure-instructions-cache-updated (directory cache-table cache-key)
  "Ensure CACHE-TABLE is up to date for DIRECTORY identified by CACHE-KEY."
  (when (ai--directory-needs-cache-update-p directory cache-key)
    (ai--update-instructions-cache directory cache-table cache-key)))

(defun ai--ensure-system-prompts-cache-updated (directory cache-table cache-key)
  "Ensure system prompts CACHE-TABLE is up to date for DIRECTORY identified by CACHE-KEY."
  (when (ai--directory-needs-cache-update-p directory cache-key)
    (ai--update-system-prompts-cache directory cache-table cache-key)))

(defun ai-get-default-instructions (command-name)
  "Get default instructions for COMMAND-NAME from ai-mode package."
  (let ((directory (ai--get-default-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--default-instructions-cache "default")
    (gethash command-name ai--default-instructions-cache)))

(defun ai-get-global-instructions (command-name)
  "Get global instructions for COMMAND-NAME from ~/.ai/commands/."
  (let ((directory (ai--get-global-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--global-instructions-cache "global")
    (gethash command-name ai--global-instructions-cache)))

(defun ai-get-local-instructions (command-name)
  "Get local instructions for COMMAND-NAME from project .ai/commands/."
  (when-let ((directory (ai--get-local-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--local-instructions-cache "local")
    (gethash command-name ai--local-instructions-cache)))

(defun ai-get-default-system-prompt (prompt-name)
  "Get default system prompt for PROMPT-NAME from ai-mode package."
  (let ((directory (ai--get-default-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--default-system-prompts-cache "default-system")
    ;; Normalize the lookup name
    (gethash (ai--normalize-prompt-name prompt-name) ai--default-system-prompts-cache)))

(defun ai-get-global-system-prompt (prompt-name)
  "Get global system prompt for PROMPT-NAME from ~/.ai/system/."
  (let ((directory (ai--get-global-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--global-system-prompts-cache "global-system")
    ;; Normalize the lookup name
    (gethash (ai--normalize-prompt-name prompt-name) ai--global-system-prompts-cache)))

(defun ai-get-local-system-prompt (prompt-name)
  "Get local system prompt for PROMPT-NAME from project .ai/system/."
  (when-let ((directory (ai--get-local-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--local-system-prompts-cache "local-system")
    ;; Normalize the lookup name
    (gethash (ai--normalize-prompt-name prompt-name) ai--local-system-prompts-cache)))

(defun ai-get-instructions (command-name)
  "Get instructions for COMMAND-NAME using priority: Local > Global > Default."
  (or (ai-get-local-instructions command-name)
      (ai-get-global-instructions command-name)
      (ai-get-default-instructions command-name)))

(defun ai-get-system-prompt (prompt-name)
  "Get system prompt for PROMPT-NAME using priority: Local > Global > Default."
  (or (ai-get-local-system-prompt prompt-name)
      (ai-get-global-system-prompt prompt-name)
      (ai-get-default-system-prompt prompt-name)))

(defun ai--get-local-command-examples (command-name)
  "Get local examples for COMMAND-NAME from project .ai/commands/."
  (when-let ((directory (ai--get-local-instructions-directory)))
    (ai--read-instruction-file
     (ai--get-command-examples-file-path command-name directory))))

(defun ai--get-global-command-examples (command-name)
  "Get global examples for COMMAND-NAME from ~/.ai/commands/."
  (let ((directory (ai--get-global-instructions-directory)))
    (ai--read-instruction-file
     (ai--get-command-examples-file-path command-name directory))))

(defun ai--get-default-command-examples (command-name)
  "Get default examples for COMMAND-NAME from ai-mode package."
  (let ((directory (ai--get-default-instructions-directory)))
    (ai--read-instruction-file
     (ai--get-command-examples-file-path command-name directory))))

(defun ai--get-command-examples (command-name)
  "Get examples for COMMAND-NAME using priority: Local > Global > Default."
  (or (ai--get-local-command-examples command-name)
      (ai--get-global-command-examples command-name)
      (ai--get-default-command-examples command-name)))

(defun ai--get-rendered-command-examples (command context)
  "Get examples for COMMAND, render with CONTEXT, and return result."
  (when-let ((examples-content (ai--get-command-examples command)))
    (ai-utils--render-template examples-content context)))

(defun ai-get-all-instructions (command-name)
  "Get all instructions for COMMAND-NAME from all locations with priority info.
Returns a list of plists with :location, :content, and :priority keys."
  (let ((results nil))
    (when-let ((local-content (ai-get-local-instructions command-name)))
      (push `(:location "Local" :content ,local-content :priority 3) results))
    (when-let ((global-content (ai-get-global-instructions command-name)))
      (push `(:location "Global" :content ,global-content :priority 2) results))
    (when-let ((default-content (ai-get-default-instructions command-name)))
      (push `(:location "Default" :content ,default-content :priority 1) results))
    (sort results (lambda (a b) (> (plist-get a :priority) (plist-get b :priority))))))

(defun ai--get-all-file-based-command-names ()
  "Get all command names from instruction files only (not from config).
Commands are collected from all instruction locations but not from ai--commands-config-map."
  (let ((commands-set (make-hash-table :test 'equal))
        (configured-commands (mapcar #'car ai--commands-config-map)))

    ;; Mark configured commands to exclude them
    (dolist (cmd configured-commands)
      (puthash cmd t commands-set))

    ;; Collect commands from instruction files, excluding configured ones
    (let ((instruction-commands nil))
      ;; Collect from default instructions
      (let ((directory (ai--get-default-instructions-directory)))
        (ai--ensure-instructions-cache-updated directory ai--default-instructions-cache "default")
        (maphash (lambda (cmd _content)
                   (unless (gethash cmd commands-set)
                     (push cmd instruction-commands)
                     (puthash cmd t commands-set)))
                 ai--default-instructions-cache))

      ;; Collect from global instructions
      (let ((directory (ai--get-global-instructions-directory)))
        (ai--ensure-instructions-cache-updated directory ai--global-instructions-cache "global")
        (maphash (lambda (cmd _content)
                   (unless (gethash cmd commands-set)
                     (push cmd instruction-commands)
                     (puthash cmd t commands-set)))
                 ai--global-instructions-cache))

      ;; Collect from local instructions
      (when-let ((directory (ai--get-local-instructions-directory)))
        (ai--ensure-instructions-cache-updated directory ai--local-instructions-cache "local")
        (maphash (lambda (cmd _content)
                   (unless (gethash cmd commands-set)
                     (push cmd instruction-commands)
                     (puthash cmd t commands-set)))
                 ai--local-instructions-cache))

      ;; Sort instruction commands alphabetically
      (sort instruction-commands #'string<))))

(defun ai--get-all-available-command-names ()
  "Get all available command names from all instruction locations plus configured commands.
Commands from ai--commands-config-map appear first in their original order."
  (let ((configured-commands (mapcar #'car ai--commands-config-map))
        (file-based-commands (ai--get-all-file-based-command-names)))
    (append configured-commands file-based-commands)))

(defun ai--parse-command-modifiers (command-name)
  "Parse modifiers from COMMAND-NAME and return (base-name . config-plist).
Modifiers are separated by '__' and can be prefixes or suffixes.
Example: 'user__explain_code__show' -> ('explain_code' . (:user-input t :result-action show))"
  (let ((parts (split-string command-name "__"))
        (config '())
        (base-name command-name))

    (when (> (length parts) 1)
      ;; Multiple parts found, parse modifiers
      (let ((all-parts parts)
            (identified-modifiers '())
            (remaining-parts '()))

        ;; Identify all modifier parts
        (dolist (part all-parts)
          (cond
           ;; Check action modifiers
           ((assoc part ai--file-command-action-modifiers)
            (push (cons 'action part) identified-modifiers)
            (setq config (plist-put config :result-action
                                   (cdr (assoc part ai--file-command-action-modifiers)))))
           ;; Check behavior modifiers
           ((assoc part ai--file-command-behavior-modifiers)
            (push (cons 'behavior part) identified-modifiers)
            (let ((behavior-config (cdr (assoc part ai--file-command-behavior-modifiers))))
              (setq config (append config behavior-config))))
           ;; Check context modifiers
           ((assoc part ai--file-command-context-modifiers)
            (push (cons 'context part) identified-modifiers)
            (let ((context-config (cdr (assoc part ai--file-command-context-modifiers))))
              (setq config (append config context-config))))
           ;; Not a modifier, part of base name
           (t
            (push part remaining-parts))))

        ;; Construct base name from remaining non-modifier parts
        (setq base-name (if remaining-parts
                           (string-join (reverse remaining-parts) "__")
                         (car all-parts)))))

    (cons base-name config)))

(defun ai--get-file-command-config (command-name)
  "Get configuration for file-based command with modifier parsing.
Returns a configuration plist with parsed modifiers applied."
  (let* ((parsed (ai--parse-command-modifiers command-name))
         (base-name (car parsed))
         (modifier-config (cdr parsed))
         (base-config ai--command-config))

    ;; Merge base configuration with modifier configuration
    ;; Modifier config takes precedence over base config
    (let ((final-config base-config))
      (while modifier-config
        (let ((key (car modifier-config))
              (value (cadr modifier-config)))
          (setq final-config (plist-put final-config key value)))
        (setq modifier-config (cddr modifier-config)))
      final-config)))

(defun ai--get-command-display-name (command-name)
  "Get display name for COMMAND-NAME with indicators for instruction sources and modifiers.
Shows configured commands [C], instruction sources [D/G/L], and modifier indicators.
For file-based commands with modifiers, displays clean base name with modifier indicators."
  (let ((indicators nil)
        (configured-p (assoc command-name ai--commands-config-map))
        (display-name command-name))

    ;; Check for instruction files from different sources
    (when (ai-get-default-instructions command-name)
      (push "D" indicators))
    (when (ai-get-global-instructions command-name)
      (push "G" indicators))
    (when (ai-get-local-instructions command-name)
      (push "L" indicators))

    ;; Parse and display modifiers for file-based commands
    (unless configured-p
      (when (ai-get-instructions command-name)
        (let* ((parsed (ai--parse-command-modifiers command-name))
               (base-name (car parsed))
               (modifier-config (cdr parsed)))

          ;; Use clean base name if command has modifiers
          (when (and modifier-config (not (string= command-name base-name)))
            (setq display-name base-name))

          ;; Add modifier indicators
          (when (plist-get modifier-config :user-input)
            (push "U" indicators))  ; User input required
          (when (plist-get modifier-config :needs-buffer-context)
            (push "B" indicators))  ; Buffer context needed
          (when (plist-get modifier-config :needs-project-context)
            (push "P" indicators))  ; Project context needed

          ;; Add result action indicator
          (when-let ((result-action (plist-get modifier-config :result-action)))
            (let ((action-indicator (cond
                                    ((eq result-action 'show) "S")
                                    ((eq result-action 'eval) "E")
                                    ((eq result-action 'replace) "R")
                                    ((eq result-action 'insert-at-point) "I")
                                    ((eq result-action 'complete) "C")
                                    (t "?"))))
              (push action-indicator indicators)))

          ;; Add context size indicators
          (when (plist-member modifier-config :preceding-context-size)
            (let ((preceding (plist-get modifier-config :preceding-context-size))
                  (following (plist-get modifier-config :following-context-size)))
              (cond
               ((and (null preceding) (null following))
                (push "F" indicators))  ; Full context
               ((and (numberp preceding) (< preceding 10))
                (push "s" indicators))  ; Small context
               ((and (numberp preceding) (> preceding 15))
                (push "L" indicators))  ; Large context
               (t nil)))))))

    (format "%s%s %s"
            (if configured-p "[C]" "")
            (if indicators
                (format "[%s]" (string-join (reverse indicators) ""))
              "")
            display-name)))

(defun ai--get-memory-file-path (location)
  "Get memory file path for LOCATION (either 'global or 'local)."
  (cond
   ((eq location 'global)
    (expand-file-name ".ai/memory.md" "~"))
   ((eq location 'local)
    (when-let ((project-root (ai-common--get-project-root)))
      (expand-file-name ".ai/memory.md" project-root)))
   ((eq location 'local-team)
    (when-let ((project-root (ai-common--get-project-root)))
      (expand-file-name ".ai/memory.local.md" project-root)))))

(defun ai--read-memory-file (location)
  "Read memory file content from LOCATION."
  (when-let ((file-path (ai--get-memory-file-path location)))
    (ai--read-instruction-file file-path)))

(defun ai--get-memory-context ()
  "Get combined memory context from all available memory files."
  (let ((memory-contents nil))

    ;; Read global memory
    (when-let ((global-memory (ai--read-memory-file 'global)))
      (push (ai-common--make-typed-struct global-memory 'memory-content 'global-memory) memory-contents))

    ;; Read local project memory
    (when-let ((local-memory (ai--read-memory-file 'local)))
      (push (ai-common--make-typed-struct local-memory 'memory-content 'local-memory) memory-contents))

    ;; Read local team memory
    (when-let ((local-team-memory (ai--read-memory-file 'local-team)))
      (push (ai-common--make-typed-struct local-team-memory 'memory-content 'local-team-memory) memory-contents))

    (when memory-contents
      (ai-common--make-typed-struct memory-contents 'memory 'memory-files))))

(defun ai--get-command-for-editing ()
  "Get command name from user input with completion but allowing arbitrary input.
Shows configured commands first, then file-based commands."
  (let* ((configured-commands (mapcar #'car ai--commands-config-map))
         ;; Get all available commands but exclude those already in configured
         (all-commands (ai--get-all-available-command-names))
         (file-only-commands (cl-remove-if (lambda (cmd) (member cmd configured-commands)) all-commands))
         ;; Create ordered list: configured first, then file-only alphabetically sorted
         (ordered-commands (append configured-commands (sort file-only-commands #'string<)))
         (command-displays (mapcar #'ai--get-command-display-name ordered-commands))
         (command-alist (cl-mapcar #'cons command-displays ordered-commands))
         (input (completing-read "Enter command name: " command-displays nil nil)))
    ;; If input matches a display name, return the actual command name
    (or (cdr (assoc input command-alist))
        ;; Otherwise return the input as-is (for new commands)
        input)))

(defun ai--get-available-edit-locations (command-name)
  "Get available editing locations for COMMAND-NAME.
Returns a list of plists with :name, :symbol, :directory, :file-path, and :exists-p keys."
  (let ((locations nil))
    ;; Global location
    (when-let ((global-dir (ai--get-global-instructions-directory)))
      (let* ((file-path (ai--get-instruction-file-path command-name global-dir))
             (exists-p (and file-path (file-exists-p file-path))))
        (push `(:name "Global"
                :symbol global
                :directory ,global-dir
                :file-path ,file-path
                :exists-p ,exists-p)
              locations)))

    ;; Local location (project-specific)
    (when-let ((local-dir (ai--get-local-instructions-directory)))
      (let* ((file-path (ai--get-instruction-file-path command-name local-dir))
             (exists-p (and file-path (file-exists-p file-path))))
        (push `(:name "Local"
                :symbol local
                :directory ,local-dir
                :file-path ,file-path
                :exists-p ,exists-p)
              locations)))

    ;; Reverse to get Global first, then Local
    (reverse locations)))

(defun ai--format-location-display (location-info)
  "Format LOCATION-INFO for display in completing-read.
LOCATION-INFO is a plist with location details."
  (let* ((name (plist-get location-info :name))
         (file-path (plist-get location-info :file-path))
         (exists-p (plist-get location-info :exists-p))
         (status (if exists-p "[EXISTS]" "[NEW]")))
    (format "%s: %s %s" name file-path status)))

(defun ai--select-edit-location (command-name locations)
  "Select editing location for COMMAND-NAME from LOCATIONS.
Returns the selected location plist."
  (if (= (length locations) 1)
      ;; If only one location available, use it directly
      (car locations)
    ;; Otherwise, let user choose
    (let* ((location-displays (mapcar #'ai--format-location-display locations))
           (location-alist (cl-mapcar #'cons location-displays locations))
           (selected-display (completing-read "Select location to edit: " location-displays nil t)))
      (cdr (assoc selected-display location-alist)))))

(defun ai--open-instruction-file (command-name location-info)
  "Open instruction file for COMMAND-NAME at LOCATION-INFO for editing.
LOCATION-INFO is a plist with location details."
  (let* ((directory (plist-get location-info :directory))
         (file-path (plist-get location-info :file-path))
         (exists-p (plist-get location-info :exists-p)))

    (unless file-path
      (user-error "Cannot determine file path for command: %s" command-name))

    ;; Ensure directory exists
    (unless (file-directory-p directory)
      (make-directory directory t))

    ;; Open file for editing
    (find-file file-path)

    ;; If file is new, add template content
    (when (and (not exists-p) (= (point-min) (point-max)))
      (insert (format "# Instructions for command: %s\n\n" command-name)
              "Write your AI instructions here...\n"))))

(defun ai-edit-command-instructions ()
  "Interactively edit instruction files for commands with improved interface."
  (interactive)
  (let* ((command-name (ai--get-command-for-editing))
         (available-locations (ai--get-available-edit-locations command-name)))

    (unless available-locations
      (user-error "No editable locations available. Make sure you're in a project or global config is accessible"))

    (let ((selected-location (ai--select-edit-location command-name available-locations)))
      (ai--open-instruction-file command-name selected-location))))

(defun ai-edit-command-instructions-for-name (command-name)
  "Edit instruction file for specific COMMAND-NAME without user prompt."
  (let ((available-locations (ai--get-available-edit-locations command-name)))
    (unless available-locations
      (user-error "No editable locations available. Make sure you're in a project or global config is accessible"))
    (let ((selected-location (ai--select-edit-location command-name available-locations)))
      (ai--open-instruction-file command-name selected-location))))

(defun ai--get-available-modifiers ()
  "Get all available modifiers categorized by type.
Returns an alist with categories as keys and modifier lists as values."
  `((action . ,(mapcar #'car ai--file-command-action-modifiers))
    (behavior . ,(mapcar #'car ai--file-command-behavior-modifiers))
    (context . ,(mapcar #'car ai--file-command-context-modifiers))))

(defun ai--describe-command-modifiers (command-name)
  "Describe the modifiers used in COMMAND-NAME.
Returns a human-readable description of the command's configuration."
  (let* ((parsed (ai--parse-command-modifiers command-name))
         (base-name (car parsed))
         (config (cdr parsed))
         (descriptions '()))

    (when (plist-get config :result-action)
      (push (format "Result action: %s" (plist-get config :result-action)) descriptions))
    (when (plist-get config :user-input)
      (push "Requires user input" descriptions))
    (when (plist-get config :needs-buffer-context)
      (push "Uses full buffer context" descriptions))
    (when (plist-get config :needs-project-context)
      (push "Uses project context" descriptions))
    (when (plist-member config :preceding-context-size)
      (let ((preceding (plist-get config :preceding-context-size))
            (following (plist-get config :following-context-size)))
        (push (format "Context size: %s/%s"
                     (if preceding (number-to-string preceding) "full")
                     (if following (number-to-string following) "full"))
              descriptions)))

    (if descriptions
        (format "Command: %s\nModifiers: %s" base-name (string-join descriptions ", "))
      (format "Command: %s (no modifiers)" base-name))))

(defun ai-describe-command-modifiers ()
  "Interactively describe modifiers for a selected command."
  (interactive)
  (let* ((command (ai--get-command))
         (description (ai--describe-command-modifiers command)))
    (message "%s" description)))

(defun ai-create-modified-command ()
  "Interactively create a new file-based command with modifiers."
  (interactive)
  (let* ((base-name (read-string "Base command name: "))
         (available-modifiers (ai--get-available-modifiers))
         (selected-modifiers '())
         (modifier-string ""))

    ;; Select action modifier
    (when (y-or-n-p "Add result action modifier? ")
      (let ((action (completing-read "Select action: "
                                    (cdr (assoc 'action available-modifiers)))))
        (push action selected-modifiers)))

    ;; Select behavior modifiers
    (when (y-or-n-p "Add behavior modifiers? ")
      (let ((behaviors (completing-read-multiple "Select behaviors (comma-separated): "
                                                 (cdr (assoc 'behavior available-modifiers)))))
        (setq selected-modifiers (append behaviors selected-modifiers))))

    ;; Select context modifier
    (when (y-or-n-p "Add context size modifier? ")
      (let ((context (completing-read "Select context size: "
                                     (cdr (assoc 'context available-modifiers)))))
        (push context selected-modifiers)))

    ;; Construct command name
    (when selected-modifiers
      (setq modifier-string (string-join (reverse selected-modifiers) "__")))

    (let ((full-command-name (if (string-empty-p modifier-string)
                                base-name
                              (format "%s__%s" modifier-string base-name))))
      (message "Suggested command name: %s" full-command-name)
      (when (y-or-n-p "Create instruction file for this command? ")
        ;; Check if instructions already exist
        (let* ((global-instructions (ai-get-global-instructions full-command-name))
               (local-instructions (ai-get-local-instructions full-command-name))
               (file-exists (or global-instructions local-instructions)))
          (when file-exists
            (message "Warning: Instructions for command '%s' already exist" full-command-name))
          ;; Create instruction file for the specific command name
          (message "Creating instruction file for command: %s" full-command-name)
          (ai-edit-command-instructions-for-name full-command-name))))))

;; Usage statistics callback function
(defun ai--create-usage-statistics-callback ()
  "Create a callback function for displaying usage statistics."
  (lambda (usage-stats)
    (let ((input-tokens (plist-get usage-stats :input-tokens))
          (output-tokens (plist-get usage-stats :output-tokens))
          (total-tokens (plist-get usage-stats :total-tokens))
          (input-tokens-write-cache (plist-get usage-stats :input-tokens-write-cache))
          (input-tokens-read-cache (plist-get usage-stats :input-tokens-read-cache)))
      (message "AI Usage: Input=%s, Output=%s, Total=%s%s%s"
               (or input-tokens "?")
               (or output-tokens "?")
               (or total-tokens "?")
               (if input-tokens-write-cache (format ", Cache Write=%s" input-tokens-write-cache) "")
               (if input-tokens-read-cache (format ", Cache Read=%s" input-tokens-read-cache) "")))))

;; Progress indicator functions (buffer-local operations)
(defun ai--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let* ((elapsed (- (float-time) start-time))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun ai--progress-start (&optional message buffer)
  "Start progress indicator with optional MESSAGE in specified BUFFER or current buffer."
  (when ai--progress-indicator-enabled
    (with-current-buffer (or buffer (current-buffer))
      (setq ai--progress-active t
            ai--progress-counter 0
            ai--progress-start-time (float-time)
            ai--progress-message (or message "AI request in progress"))

      (cond
       ((eq ai--progress-indicator-style 'spinner)
        (ai--progress-start-spinner))
       ((eq ai--progress-indicator-style 'dots)
        (ai--progress-start-dots))
       ((eq ai--progress-indicator-style 'message)
        (force-mode-line-update))))))

(defun ai--progress-stop (&optional buffer)
  "Stop progress indicator in specified BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ai--progress-active
      (setq ai--progress-active nil
            ai--progress-start-time nil)
      (when ai--progress-timer
        (cancel-timer ai--progress-timer)
        (setq ai--progress-timer nil))
      (force-mode-line-update))))

(defun ai--progress-start-spinner ()
  "Start spinner-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai--progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (when ai--progress-active
                                  (setq ai--progress-counter (1+ ai--progress-counter))
                                  (force-mode-line-update)))))))))

(defun ai--progress-start-dots ()
  "Start dots-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai--progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (when ai--progress-active
                                  (setq ai--progress-counter (1+ ai--progress-counter))
                                  (force-mode-line-update)))))))))

(defun ai--progress-wrap-callback (original-callback &optional buffer)
  "Wrap ORIGINAL-CALLBACK to stop progress indicator when called in specified BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (lambda (&rest args)
      (ai--progress-stop target-buffer)
      (when original-callback
        (apply original-callback args)))))

;;;###autoload
(define-minor-mode ai-mode
  "Minor mode for AI interaction.

AI Mode provides a comprehensive set of tools for AI-assisted development
directly within Emacs. When enabled, it adds AI-powered capabilities for
code completion, explanation, refactoring, and more.

Key features when enabled:
- AI command execution with customizable actions
- Context-aware code completion and modification
- Project-wide AI analysis and indexing
- Progress tracking for AI operations
- File-based instruction system with hot-reloading
- Multiple AI backend support with easy switching

The mode integrates with the mode line to show current AI backend,
context settings, and progress indicators during AI operations.

Key bindings are available under the `ai-keymap-prefix' (default: C-c i).
Use \\[ai-perform] to execute AI commands with automatic result handling,
\\[ai-show] for informational queries, or \\[ai-execute] for code evaluation.

See the package commentary for detailed usage instructions."
  :keymap ai-mode-map
  :lighter (:eval (ai-mode-line-info))
  :group 'ai
  :after-hook (force-mode-line-update t)
  (progn
    (if ai-mode
        (progn
          (add-hook 'pre-command-hook 'ai-mode-pre-command)
          (add-hook 'post-command-hook 'ai-mode-post-command)
          (add-hook 'after-save-hook 'ai-mode--initialize-system))
      (remove-hook 'pre-command-hook 'ai-mode-pre-command)
      (remove-hook 'post-command-hook 'ai-mode-post-command)
      (remove-hook 'after-save-hook 'ai-mode--initialize-system))
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
        (ai-mode--initialize-system)
        (message "Global AI mode is now enabled!"))
    (message "Global AI mode is now disabled!")))

(add-hook 'global-ai-mode-hook 'ai-mode-global-init-hook)

(defun ai-mode--initialize-system ()
  "Initialize both command and system prompt caches."
  (ai-mode--update-commands-cache)
  (ai-mode--update-system-prompts-cache))

(defun ai-mode--update-commands-cache ()
  "Update command caches for all locations."
  (message "Updating AI mode command caches...")

  ;; Update default commands cache
  (let ((directory (ai--get-default-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--default-instructions-cache "default"))

  ;; Update global commands cache
  (let ((directory (ai--get-global-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--global-instructions-cache "global"))

  ;; Update local commands cache
  (when-let ((directory (ai--get-local-instructions-directory)))
    (ai--ensure-instructions-cache-updated directory ai--local-instructions-cache "local"))

  (message "AI mode command caches updated!"))

(defun ai-mode--update-system-prompts-cache ()
  "Update system prompts cache for all locations."
  (message "Updating AI mode system prompts caches...")

  ;; Update default system prompts cache
  (let ((directory (ai--get-default-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--default-system-prompts-cache "default-system"))

  ;; Update global system prompts cache
  (let ((directory (ai--get-global-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--global-system-prompts-cache "global-system"))

  ;; Update local system prompts cache
  (when-let ((directory (ai--get-local-system-prompts-directory)))
    (ai--ensure-system-prompts-cache-updated directory ai--local-system-prompts-cache "local-system"))

  (message "AI mode system prompts caches updated!"))

(cl-defun ai--execute-context (context success-callback &key (fail-callback nil) (model nil))
  "Execute CONTEXT using SUCCESS-CALLBACK and optional FAIL-CALLBACK with an optional MODEL."
  (ai-perform-async-backend-query context success-callback :fail-callback fail-callback :model model))

(cl-defun ai--execute-command (command success-callback &key (fail-callback nil) (model nil))
  "Execute COMMAND by dispatching to the appropriate backend using SUCCESS-CALLBACK.
Optionally use FAIL-CALLBACK and specify a MODEL."
  (let* ((execution-model (if model model (ai--get-current-model)))
         (execution-backend (map-elt execution-model :execution-backend))
         (context (ai--get-executions-context-for-command command :model execution-model)))
    (funcall execution-backend
             context
             execution-model
             :success-callback success-callback
             :fail-callback fail-callback
             :update-usage-callback (ai--create-usage-statistics-callback)
             :enable-caching ai--prompt-caching-enabled)))

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
   ((eq result-action 'complete)        "complete")
   ((eq result-action 'show)            "explain")
   ((eq result-action 'eval)            "eval")
   ((eq result-action 'replace)         "modify")
   ((eq result-action 'insert-at-point) "complete")
   (t                                   nil)))


(cl-defun ai--get-contextual-action-object (config &key preceding-context-size following-context-size)
  "Generate contextual action object based on CONFIG and optional context sizes PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE."
  (let* ((action (map-elt config :action))
         (result-action (map-elt config :result-action))
         (container-type (ai--get-container-type-by-result-action result-action)))

    (if (equal container-type "complete")
        (ai-common--make-action-object
         container-type
         (ai-common--assemble-completion-context
          :preceding-context-size preceding-context-size
          :following-context-size following-context-size)
         'contextual-action)
      (ai-common--make-action-object
       container-type
       (ai-common--assemble-edit-context)
       'contextual-action))))

(defun ai--get-current-buffer-context ()
  "Get the additional context for the current buffer if enabled."
  (when ai--current-buffer-additional-context
    (ai-common--make-file-context-from-buffer)))

(defun ai--should-include-current-buffer-content-context-p (config command full-context)
  "Determine if current-buffer-content-context should be included.
CONFIG is the command configuration, COMMAND is the command being executed,
and FULL-CONTEXT contains the complete context information.
Returns t if context should be included, nil otherwise."
  (let* ((result-action (map-elt config :result-action))
         (needs-buffer-context (map-elt config :needs-buffer-context))
         (container-type (ai--get-container-type-by-result-action result-action))
         (has-region (use-region-p)))

    (and ai--current-buffer-additional-context
         (or (and needs-buffer-context
                  (not (string= container-type "complete")))
             (and (eq result-action 'replace) has-region)))))

(defun ai--get-result-action-prompt (result-action context)
  "Get the prompt for RESULT-ACTION rendered with CONTEXT."
  (let ((prompt-name (format "result_action_%s" (symbol-name result-action))))
    (ai--get-rendered-system-prompt prompt-name context)))

(defun ai--get-user-input ()
  "Get user input using the configured function."
  (if (functionp ai--user-input-method)
      (funcall ai--user-input-method)
    (error "ai--user-input-method is not a valid function: %s" ai--user-input-method)))

(defun ai--process-external-context-item (item)
  "Process a single external context item ITEM for inclusion in execution context.
Handles both plain contexts and typed structs, including nested structures."
  (cond
   ;; Already a typed struct - return as is
   ((and (listp item) (keywordp (car item)))
    item)
   ;; Plain string or other format
   (t
    (ai-common--make-typed-struct
     (format "%s" item)
     'additional-context
     'external-context))))

(cl-defun ai--get-indexing-context (file-struct model)
  "Create execution context for indexing a single FILE-STRUCT with MODEL.
Returns a plist suitable for the backend."
  (let* ((instruction-command "index file")
         (config (ai--get-command-config-by-type instruction-command))
         (project-root (ai-common--get-project-root))
         ;; Basic context without existing summaries for parallel-independent strategy
         (basic-full-context `(:model-context ,model
                               :file-path ,(plist-get file-struct :relative-path)
                               :buffer-language ,(plist-get file-struct :buffer-language)
                               :file-size ,(plist-get file-struct :file-size)
                               :project-root ,project-root
                               :strategy ,ai--indexing-strategy))
         (command-instructions (ai-common--make-typed-struct
                                (ai--get-rendered-command-instructions instruction-command basic-full-context)
                                'agent-instructions
                                'command-specific-instructions))

         ;; File to be indexed
         (file-content-as-user-input (ai-common--make-typed-struct
                                      file-struct
                                      'user-input
                                      'file-to-index))

         ;; Simple message list for parallel processing
         (messages (list command-instructions file-content-as-user-input)))

    ;; Filter out any null messages
    (setq messages (ai-utils-filter-non-empty-content messages))

    ;; Log to prompt buffer for debugging
    (ai-utils-write-context-to-prompt-buffer messages)

    `(:messages ,messages :model-context ,model)))

(defun ai--index-completion-check (pending-count accumulated-summaries-ht target-buffer project-root start-time)
  "Check if indexing is complete and finalize the results.
PENDING-COUNT is the number of remaining requests.
ACCUMULATED-SUMMARIES-HT is the hash table containing completed summaries keyed by relative path.
TARGET-BUFFER is the buffer where the indexing was initiated.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (setq ai--progress-message (format "Indexing project files (%d remaining)" pending-count))
  (force-mode-line-update)

  (when (zerop pending-count)
    (ai--progress-stop target-buffer)
    ;; Store the list of summary structs for this project root
    (puthash project-root (hash-table-values accumulated-summaries-ht) ai--project-files-summary-index)
    ;; Save to disk
    (ai--save-current-index-to-disk project-root start-time)
    ;; Cleanup old versions
    (ai--cleanup-old-index-versions project-root)
    (message "Project files summary index updated with %d files for project '%s'."
             (hash-table-count accumulated-summaries-ht)
             project-root)))

(defun ai--create-file-summarization-success-callback (accumulated-summaries-ht pending-count-ref target-buffer original-file-struct project-root start-time)
  "Create success callback for file summarization.
ACCUMULATED-SUMMARIES-HT is a hash table to store summaries per file.
PENDING-COUNT-REF is a cons cell containing the pending requests counter.
TARGET-BUFFER is the buffer where indexing was initiated.
ORIGINAL-FILE-STRUCT is the original file struct passed for summarization.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (lambda (messages)
    (let ((summary-struct (ai-utils--get-message messages)))
      (when (and summary-struct (listp summary-struct) (plist-get summary-struct :type))
        (let* ((summary-content (plist-get summary-struct :content))
               (file-path (plist-get original-file-struct :file))
               (relative-path (plist-get original-file-struct :relative-path))
               (file-size (plist-get original-file-struct :file-size))
               (final-summary-struct (ai-common--make-file-summary-struct
                                      summary-content
                                      nil
                                      :file file-path
                                      :relative-path relative-path
                                      :file-size file-size)))
          ;; Store the summary struct in the local hash table, keyed by its relative path
          (puthash relative-path final-summary-struct accumulated-summaries-ht))))
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (ai--index-completion-check (car pending-count-ref)
                                    accumulated-summaries-ht
                                    target-buffer
                                    project-root
                                    start-time)))))

(defun ai--create-file-summarization-fail-callback (pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create failure callback for file summarization.
PENDING-COUNT-REF is a cons cell containing the pending requests counter.
ACCUMULATED-SUMMARIES-HT is a hash table to accumulate summaries.
TARGET-BUFFER is the buffer where indexing was initiated.
ORIGINAL-FILE-STRUCT is the original file struct that was sent for summarization.
PROJECT-ROOT is the root path of the project.
START-TIME is when the indexing process began."
  (lambda (request-data error-message)
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (message "Failed to summarize file '%s'. Error: %s"
                 (plist-get original-file-struct :relative-path)
                 (ai-common--get-text-content-from-struct error-message))))
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (ai--index-completion-check (car pending-count-ref)
                                    accumulated-summaries-ht
                                    target-buffer
                                    project-root
                                    start-time)))))

(defun ai--process-files-parallel-independent (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing in parallel without context sharing.
Each file is processed independently without any existing context.
START-TIME is when the indexing process began."
  (let ((execution-backend (map-elt current-model :execution-backend)))

    (ai-utils--verbose-message "Parallel independent strategy: processing %d files without context sharing"
                               (length file-structs))

    (dolist (file-struct file-structs)
      (setcar pending-count-ref (1+ (car pending-count-ref)))

      (let* ((indexing-context (ai--get-indexing-context file-struct current-model))
             (success-callback (ai--create-file-summarization-success-callback
                                accumulated-summaries-ht pending-count-ref target-buffer file-struct project-root start-time))
             (fail-callback (ai--create-file-summarization-fail-callback
                             pending-count-ref accumulated-summaries-ht target-buffer file-struct project-root start-time)))

        (funcall execution-backend
                 indexing-context
                 current-model
                 :success-callback success-callback
                 :fail-callback fail-callback
                 :update-usage-callback (ai--create-usage-statistics-callback)
                 :enable-caching ai--prompt-caching-enabled))

      ;; Introduce a configurable timeout between indexing calls
      (when (> ai--indexing-call-timeout 0)
        (sit-for ai--indexing-call-timeout)))))

(defun ai--process-files-sequential (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing sequentially with session context accumulation only.
Uses only files from current session for context accumulation, no existing index.
START-TIME is when the indexing process began."
  (let ((remaining-files (copy-sequence file-structs)))

    (ai-utils--verbose-message "Sequential strategy: processing %d files with session context accumulation only"
                               (length file-structs))

    (when remaining-files
      (ai--process-next-file-sequential remaining-files current-model pending-count-ref
                                        accumulated-summaries-ht target-buffer project-root start-time))))

(defun ai--process-next-file-sequential (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process the next file in REMAINING-FILES sequentially.
This is a recursive helper function that processes one file at a time.
Uses only current session context for accumulation, no existing index.
START-TIME is when the indexing process began."
  (if (null remaining-files)
      ;; No more files to process
      (when (buffer-live-p target-buffer)
        (with-current-buffer target-buffer
          (ai--index-completion-check (car pending-count-ref)
                                      accumulated-summaries-ht
                                      target-buffer
                                      project-root
                                      start-time)))
    ;; Process the next file
    (let* ((file-struct (car remaining-files))
           (rest-files (cdr remaining-files))
           (execution-backend (map-elt current-model :execution-backend))
           ;; Use only accumulated session summaries for context - no existing index
           (session-summaries (when ai--indexing-include-existing-context
                                (hash-table-values accumulated-summaries-ht))))

      (ai-utils--verbose-message "Sequential step: processing file '%s' with %d session summaries for context"
                                 (plist-get file-struct :relative-path)
                                 (length (or session-summaries '())))

      (setcar pending-count-ref (1+ (car pending-count-ref)))

      (let* ((indexing-context (ai--get-indexing-context-with-summaries file-struct current-model session-summaries))
             (success-callback (ai--create-sequential-success-callback
                                rest-files current-model pending-count-ref
                                accumulated-summaries-ht target-buffer
                                file-struct project-root start-time))
             (fail-callback (ai--create-sequential-fail-callback
                             rest-files current-model pending-count-ref
                             accumulated-summaries-ht target-buffer
                             file-struct project-root start-time)))

        (funcall execution-backend
                 indexing-context
                 current-model
                 :success-callback success-callback
                 :fail-callback fail-callback
                 :update-usage-callback (ai--create-usage-statistics-callback)
                 :enable-caching ai--prompt-caching-enabled)))))

(cl-defun ai--get-indexing-context-with-summaries (file-struct model existing-summaries)
  "Create execution context for indexing a single FILE-STRUCT with MODEL and EXISTING-SUMMARIES.
This version includes existing summaries for sequential processing.
Returns a plist suitable for the backend."
  (let* ((instruction-command "index file")
         (config (ai--get-command-config-by-type instruction-command))
         (project-root (ai-common--get-project-root))
         ;; Enhanced context with existing summaries information
         (enhanced-full-context `(:model-context ,model
                                  :file-path ,(plist-get file-struct :relative-path)
                                  :buffer-language ,(plist-get file-struct :buffer-language)
                                  :file-size ,(plist-get file-struct :file-size)
                                  :project-root ,project-root
                                  :has-existing-context ,(and ai--indexing-include-existing-context
                                                             existing-summaries
                                                             (> (length existing-summaries) 0))
                                  :context-source "session-accumulation"
                                  :context-count ,(length existing-summaries)
                                  :strategy ,ai--indexing-strategy))
         (command-instructions (ai-common--make-typed-struct
                                (ai--get-rendered-command-instructions instruction-command enhanced-full-context)
                                'agent-instructions
                                'command-specific-instructions))

         ;; Include existing context if enabled and available
         (existing-context-struct
          (when (and ai--indexing-include-existing-context
                     existing-summaries
                     (> (length existing-summaries) 0))
            (ai-common--make-typed-struct
             existing-summaries
             'additional-context
             'session-accumulated-files
             :count (length existing-summaries)
             :project-root project-root
             :strategy ai--indexing-strategy)))

         ;; File to be indexed
         (file-content-as-user-input (ai-common--make-typed-struct
                                      file-struct
                                      'user-input
                                      'file-to-index))

         ;; Combine all messages, filtering out nil values
         (messages (cl-remove-if #'null
                                (list command-instructions
                                      existing-context-struct
                                      file-content-as-user-input))))

    ;; Filter out any null messages
    (setq messages (ai-utils-filter-non-empty-content messages))

    ;; Log to prompt buffer for debugging
    (ai-utils-write-context-to-prompt-buffer messages)

    `(:messages ,messages :model-context ,model)))

(defun ai--create-sequential-success-callback (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create success callback for sequential file processing.
START-TIME is when the indexing process began."
  (lambda (messages)
    ;; Process the successful response
    (let ((summary-struct (ai-utils--get-message messages)))
      (when (and summary-struct (listp summary-struct) (plist-get summary-struct :type))
        (let* ((summary-content (plist-get summary-struct :content))
               (file-path (plist-get original-file-struct :file))
               (relative-path (plist-get original-file-struct :relative-path))
               (file-size (plist-get original-file-struct :file-size))
               (final-summary-struct (ai-common--make-file-summary-struct
                                      summary-content
                                      nil
                                      :file file-path
                                      :relative-path relative-path
                                      :file-size file-size)))
          ;; Store the summary struct in the accumulated hash table
          (puthash relative-path final-summary-struct accumulated-summaries-ht)
          (ai-utils--verbose-message "Sequential success: added summary for '%s', total session summaries: %d"
                                     relative-path
                                     (hash-table-count accumulated-summaries-ht)))))

    ;; Update counters and progress
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (setq ai--progress-message (format "Indexing project files (%d remaining)"
                                           (+ (length remaining-files) (car pending-count-ref))))
        (force-mode-line-update)))

    ;; Continue with next file
    (ai--process-next-file-sequential remaining-files current-model pending-count-ref
                                      accumulated-summaries-ht target-buffer project-root start-time)))

(defun ai--create-sequential-fail-callback (remaining-files current-model pending-count-ref accumulated-summaries-ht target-buffer original-file-struct project-root start-time)
  "Create failure callback for sequential file processing.
START-TIME is when the indexing process began."
  (lambda (request-data error-message)
    ;; Log the error but continue processing
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (message "Failed to summarize file '%s'. Error: %s"
                 (plist-get original-file-struct :relative-path)
                 (ai-common--get-text-content-from-struct error-message))))

    ;; Update counters and progress
    (setcar pending-count-ref (1- (car pending-count-ref)))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (setq ai--progress-message (format "Indexing project files (%d remaining)"
                                           (+ (length remaining-files) (car pending-count-ref))))
        (force-mode-line-update)))

    ;; Continue with next file despite the error
    (ai--process-next-file-sequential remaining-files current-model pending-count-ref
                                      accumulated-summaries-ht target-buffer project-root start-time)))

(defun ai--process-file-structs-for-indexing (file-structs current-model pending-count-ref accumulated-summaries-ht target-buffer project-root start-time)
  "Process FILE-STRUCTS for indexing using the configured strategy.
Dispatches to either parallel-independent or sequential processing based on ai--indexing-strategy.
START-TIME is when the indexing process began."
  (pcase ai--indexing-strategy
    ('parallel-independent
     (ai--process-files-parallel-independent file-structs current-model pending-count-ref
                                             accumulated-summaries-ht target-buffer project-root start-time))
    ('sequential
     (ai--process-files-sequential file-structs current-model pending-count-ref
                                   accumulated-summaries-ht target-buffer project-root start-time))
    (_
     ;; Fallback to parallel-independent for unknown strategies
     (ai--process-files-parallel-independent file-structs current-model pending-count-ref
                                             accumulated-summaries-ht target-buffer project-root start-time))))

(defun ai--update-project-files-summary-index ()
  "Update the project files summary index with current project files.
This command populates `ai--project-files-summary-index` with typed structures
from the current project for use in project AI summary context mode."
  (interactive)
  (if-let ((project-root (ai-common--get-project-root)))
      (let* ((filtered-file-structs (ai-common--get-filtered-project-files-as-structs))
             (total-files (length filtered-file-structs))
             ;; Local hash table to collect summaries for the *current* project indexing run
             (accumulated-summaries-for-this-run (make-hash-table :test 'equal))
             (pending-count-ref (cons 0 nil))
             (current-model (ai--get-current-model))
             (execution-backend (map-elt current-model :execution-backend))
             (target-buffer (current-buffer))
             (start-time (current-time)))

        (unless execution-backend
          (user-error "No AI execution backend defined for current model: %s" (map-elt current-model :name)))

        (when (zerop total-files)
          (message "No project files found to index or all are ignored. Clearing index for project '%s'." project-root)
          (remhash project-root ai--project-files-summary-index)
          (cl-return-from ai--update-project-files-summary-index))

        (message "Indexing %d project files using %s strategy for project '%s'... This may take a while."
                 total-files
                 (symbol-name ai--indexing-strategy)
                 project-root)
        (ai--progress-start (format "Indexing project files (%d remaining)" total-files) target-buffer)

        (ai--process-file-structs-for-indexing filtered-file-structs
                                               current-model
                                               pending-count-ref
                                               accumulated-summaries-for-this-run
                                               target-buffer
                                               project-root
                                               start-time))
    (message "Not in a project. Cannot update project files summary index."))
  nil)

(defun ai--switch-indexing-strategy ()
  "Interactively switch the indexing strategy."
  (interactive)
  (let* ((current-strategy ai--indexing-strategy)
         (strategies '(("parallel-independent" . parallel-independent)
                       ("sequential" . sequential)))
         (strategy-descriptions '((parallel-independent . "Parallel processing without context sharing between files")
                                  (sequential . "Sequential processing with accumulating context from current session only")))
         (prompt (format "Current strategy: %s. Select new indexing strategy: "
                        (cdr (assoc current-strategy strategy-descriptions))))
         (selected-name (completing-read prompt (mapcar #'car strategies)))
         (selected-strategy (cdr (assoc selected-name strategies))))

    (setq ai--indexing-strategy selected-strategy)
    (customize-save-variable 'ai--indexing-strategy selected-strategy)
    (message "Indexing strategy changed to: %s (%s)"
             selected-name
             (cdr (assoc selected-strategy strategy-descriptions)))))

(defun ai--get-full-project-context ()
  "Get project context by collecting all filtered project files.
Returns a typed struct containing the project files context, or nil if no project is detected."
  (when-let* ((project-root (ai-common--get-project-root))
              (files-list (ai-common--get-filtered-project-files t)) ; Request relative paths
              (project-files (ai-common--get-filtered-project-files-as-structs)))
    (when project-files
      (let* ((files-list-content (mapconcat (lambda (file-path)
                                              (format "- %s" file-path))
                                            files-list "\n"))
             (files-list-struct (ai-common--make-typed-struct
                                files-list-content
                                'files-list
                                'project-scan
                                :root project-root
                                :count (length files-list)))
             (files-struct (ai-common--make-typed-struct project-files 'files 'project-files))
             (project-struct (ai-common--make-typed-struct
                             (list files-list-struct files-struct)
                             'project-context
                             'project-indexer
                             :root project-root)))
        project-struct))))

(defun ai--get-project-ai-summary-context ()
  "Get project context using project AI summary mode with cached index.
Returns a typed struct containing the project files summary from cached index,
or nil if no project is detected or index is empty."
  (when-let* ((project-root (ai-common--get-project-root))
              (files-list (ai-common--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai--project-files-summary-index)))
    (let* ((files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count (length files-list)))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root)))
      project-struct)))

(defun ai--get-enhanced-project-ai-summary-context ()
  "Get enhanced project context using project AI summary mode with dependency awareness."
  (when-let* ((project-root (ai-common--get-project-root))
              (files-list (ai-common--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai--project-files-summary-index)))
    (let* ((files-count (length files-list))
           (indexed-count (length summaries-for-current-project))
           (files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count files-count
                              :indexed-count indexed-count))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index
                         :has-context (> indexed-count 0)))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root
                           :indexing-mode "enhanced")))
      project-struct)))

(defun ai--get-project-context ()
  "Get project context based on `ai--project-context-mode` setting.
Returns a typed struct containing the appropriate project context, or nil if disabled."
  (cond
   ((eq ai--project-context-mode 'full-project)
    (ai--get-full-project-context))
   ((eq ai--project-context-mode 'project-ai-summary)
    (ai--get-project-ai-summary-context))
   (t nil)))

(defun ai--switch-project-context-mode ()
  "Interactively switch the project context mode.
Allows user to select between different project context inclusion modes."
  (interactive)
  (let* ((current-mode ai--project-context-mode)
         (modes '(("disabled" . disabled)
                  ("full-project" . full-project)
                  ("project-ai-summary" . project-ai-summary)))
         (mode-descriptions '((disabled . "No project context")
                             (full-project . "Include all filtered project files")
                             (project-ai-summary . "Include project files summary from cached index")))
         (prompt (format "Current mode: %s. Select new project context mode: "
                        (cdr (assoc current-mode mode-descriptions))))
         (selected-name (completing-read prompt (mapcar #'car modes)))
         (selected-mode (cdr (assoc selected-name modes))))

    (setq ai--project-context-mode selected-mode)
    (customize-save-variable 'ai--project-context-mode selected-mode)
    (message "Project context mode changed to: %s (%s)"
             selected-name
             (cdr (assoc selected-mode mode-descriptions)))))

(defun ai--toggle-indexing-context ()
  "Toggle inclusion of existing context in indexing process."
  (interactive)
  (setq ai--indexing-include-existing-context
        (not ai--indexing-include-existing-context))
  (message "Indexing context inclusion: %s"
           (if ai--indexing-include-existing-context "enabled" "disabled")))

(defun ai--reindex-project-with-context ()
  "Reindex the entire project with existing context enabled."
  (interactive)
  (let ((ai--indexing-include-existing-context t))
    (ai--update-project-files-summary-index)))

(cl-defun ai--get-execution-context (buffer config command &key
                                            (preceding-context-size ai--current-precending-context-size)
                                            (following-context-size ai--current-forwarding-context-size)
                                            model
                                            (external-contexts nil))
  "Get full execution context for BUFFER.
CONFIG specifies configuration, COMMAND indicates the command, and options for context sizes are PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE.
EXTERNAL-CONTEXTS is an optional list of additional context structs to include.
Each context should be a plist with :type, :content, and other metadata."
  (with-current-buffer buffer
    (let* ((needs-buffer-context (map-elt config :needs-buffer-context))
           (actual-preceding-size (if needs-buffer-context nil preceding-context-size))
           (actual-following-size (if needs-buffer-context nil following-context-size))
           (completion-context (ai-utils--get-completion-params
                                :preceding-context-size actual-preceding-size
                                :following-context-size actual-following-size))
           (buffer-context (ai-utils--get-buffer-context (current-buffer)))
           (model-context (ai-utils--get-model-context model))
           (full-context (append completion-context buffer-context model-context))

           (basic-instructions (when-let ((content (ai--get-rendered-system-prompt "basic" full-context)))
                                 (ai-common--make-typed-struct content 'agent-instructions 'basic-prompt :group 'basic)))

           (file-metadata-context (when-let ((content (ai--get-rendered-system-prompt "file_metadata" full-context)))
                                    (ai-common--make-typed-struct content 'file-metadata 'file-metadata)))

           (command-instructions (when-let ((content (ai--get-rendered-command-instructions command full-context)))
                                   (ai-common--make-typed-struct content 'agent-instructions 'command-specific-instructions :group 'command)))

           (command-examples-instructions (when-let ((content (ai--get-rendered-command-examples command full-context)))
                                            (ai-common--make-typed-struct content 'agent-instructions 'command-examples :group 'command)))

           (action-type-object-instructions (when-let ((content (ai--get-action-type-object-instructions (ai--get-action-type-for-config config) full-context)))
                                              (ai-common--make-typed-struct content 'agent-instructions 'action-object-rules :group 'command)))

           (result-action-instructions (let ((result-action (map-elt config :result-action)))
                                         (when result-action
                                           (when-let ((content (ai--get-result-action-prompt result-action full-context)))
                                             (ai-common--make-typed-struct content 'agent-instructions 'result-action-format :group 'command)))))

           (config-instructions (when-let ((instructions (map-elt config :instructions)))
                                  (ai-common--make-typed-struct instructions 'agent-instructions 'config-instructions)))

           (additional-context
            (when-let ((context-pool (ai-common--get-context-pool)))
              (when context-pool
                (ai-common--make-typed-struct context-pool 'additional-context 'context-pool))))

           (external-contexts-structs
            (when external-contexts
              (let ((processed-contexts (mapcar #'ai--process-external-context-item external-contexts)))
                (ai-common--make-typed-struct processed-contexts 'additional-context 'external-context))))

           (project-context
            (ai--get-project-context))

           (memory-context
            (ai--get-memory-context))

           (current-buffer-content-context
            (when (ai--should-include-current-buffer-content-context-p config command full-context)
              (when-let ((context (ai--get-current-buffer-context)))
                (ai-common--make-typed-struct context 'additional-context 'current-buffer-content))))

           (user-input (when (map-elt config :user-input)
                         (let ((input-text (ai--get-user-input)))
                           ;; If user input is cancelled (ai--get-user-input returns nil),
                           ;; then stop the execution of the command immediately.
                           (unless input-text
                             (user-error "User input cancelled."))
                           (let ((user-input-struct
                                  (ai-common--make-typed-struct
                                   (ai-utils--render-template input-text full-context)
                                   'user-input
                                   'user-input
                                   :render-ignore-fields '(:source))))
                             user-input-struct))))

           (command-struct (when-let ((command-text (map-elt config :command)))
                             (ai-common--make-typed-struct
                              (ai-utils--render-template command-text full-context)
                              'user-input
                              'config-command)))

           (rendered-action-context (ai-common--make-typed-struct
                                     (ai--get-contextual-action-object
                                      config
                                      :preceding-context-size actual-preceding-size
                                      :following-context-size actual-following-size)
                                     'action-context
                                     'contextual-action))

           (global-system-prompts-context
            (when-let ((global-system-prompts (ai-common--get-global-system-prompts)))
              (when global-system-prompts
                (ai-common--make-typed-struct global-system-prompts 'agent-instructions 'global-system-prompts))))

           (global-memory-context
            (when-let ((global-memory (ai-common--get-global-memory)))
              (when global-memory
                (ai-common--make-typed-struct global-memory 'additional-context 'global-memory))))

           (buffer-bound-prompts-context
            (when-let ((buffer-bound-prompts (ai-common--get-buffer-bound-prompts)))
              (when buffer-bound-prompts
                (ai-common--make-typed-struct buffer-bound-prompts 'agent-instructions 'buffer-bound-prompts))))

           (messages
            (append
             '()
             (when ai--extended-instructions-enabled
               (cl-remove-if
                #'null
                (append
                 (list basic-instructions
                       global-system-prompts-context

                       command-instructions
                       config-instructions
                       action-type-object-instructions
                       result-action-instructions
                       command-examples-instructions

                       memory-context
                       project-context
                       file-metadata-context
                       current-buffer-content-context
                       global-memory-context
                       additional-context
                       external-contexts-structs
                       buffer-bound-prompts-context
                       rendered-action-context
                       user-input
                       command-struct))))))

           (messages (ai-utils-filter-non-empty-content messages))

           (_ (ai-utils-write-context-to-prompt-buffer messages))

           (result `(:messages ,messages :model-context ,model)))
      result)))

(defun ai--get-command-instructions (command)
  "Return instruction for COMMAND from commands directory only."
  (ai-get-instructions command))

(defun ai--get-system-prompt-instructions (prompt-name)
  "Return system prompt for PROMPT-NAME from system directory only."
  (ai-get-system-prompt prompt-name))

(defun ai--get-rendered-instruction (name context &optional is-system-prompt)
  "Get and render instruction, checking appropriate directory based on IS-SYSTEM-PROMPT."
  (let ((content (if is-system-prompt
                     (ai--get-system-prompt-instructions name)
                   (ai--get-command-instructions name))))
    (when content
      (ai-utils--render-template content context))))

(defun ai--get-rendered-command-instructions (command context)
  "Get instructions for COMMAND, render it with CONTEXT, and return the result.
If no instructions are found for COMMAND, returns nil."
  (ai--get-rendered-instruction command context nil))

(defun ai--get-rendered-system-prompt (prompt-name context)
  "Get system prompt for PROMPT-NAME, render it with CONTEXT, and return the result.
If no system prompt is found for PROMPT-NAME, returns nil."

  (ai--get-rendered-instruction prompt-name context t))

(defun ai--get-action-type-object-instructions (action-type context)
  "Get the instructions for ACTION-TYPE rendered with CONTEXT."
  (ai--get-rendered-system-prompt (format "%s_action_type_object" action-type) context))

(defun ai--get-command-config-by-type (command &optional default-result-action)
  "Get command config by COMMAND, applying DEFAULT-RESULT-ACTION for unknown commands.
Now supports modifier parsing for file-based commands with instruction files."
  (if-let (config (cdr (assoc command ai--commands-config-map)))
      ;; Command found in configuration map
      (append config `(:action ,command))
    (if (string= command "complete")
        ;; Special case for completion
        ai--completion-config
      ;; Check if this is a file-based command with instructions
      (if (ai-get-instructions command)
          ;; File-based command - parse modifiers and apply configuration
          (let ((file-config (ai--get-file-command-config command)))
            ;; Apply default result action if none specified by modifiers
            (unless (plist-get file-config :result-action)
              (when default-result-action
                (setq file-config (plist-put file-config :result-action default-result-action))))
            file-config)
        ;; Regular command without instructions - use base configuration
        (let ((base-config (append ai--command-config `(:command ,command))))
          (if default-result-action
              (plist-put base-config :result-action default-result-action)
            base-config))))))

(cl-defun ai--get-executions-context-for-command (command &key (model nil) (default-result-action nil) (external-contexts nil))
  "Get execution context for COMMAND with optional MODEL, DEFAULT-RESULT-ACTION, and EXTERNAL-CONTEXTS.
EXTERNAL-CONTEXTS is a list of additional context structs to include in the execution context."
  (let* ((config (ai--get-command-config-by-type command default-result-action))
         (execution-context (ai--get-execution-context (current-buffer) config command :model model :external-contexts external-contexts)))
    execution-context))

(defun ai-explain-code-region ()
  "Explain the selected code region and display the explanation in a help buffer."
  (interactive)
  (ai--execute-context (ai--get-executions-context-for-command "explain") 'ai-utils--show-explain-help-buffer))

(defun ai--get-ordered-command-names ()
  "Get all available command names ordered with configured commands first, then file-based commands."
  (let ((configured-commands (mapcar #'car ai--commands-config-map))
        (file-based-commands (ai--get-all-file-based-command-names)))
    (append configured-commands file-based-commands)))

(defun ai--get-command ()
  "Prompt the user to select the command using `completing-read`."
  (interactive)
  (let* ((all-commands (ai--get-ordered-command-names))
         (command-displays (mapcar #'ai--get-command-display-name all-commands))
         (command-alist (cl-mapcar #'cons command-displays all-commands))
         (selected-display (completing-read ai--command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

(defun ai--get-informational-command ()
  "Prompt the user to select an informational command, filtering by :result-action 'show'."
  (interactive)
  (let* ((configured-show-commands
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'show))
                   ai--commands-config-map)))
         ;; For unconfigured commands, we default to 'show', so include all available commands
         (all-commands (ai--get-ordered-command-names))
         (command-displays (mapcar #'ai--get-command-display-name all-commands))
         (command-alist (cl-mapcar #'cons command-displays all-commands))
         (selected-display (completing-read ai--command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

(defun ai--get-executable-command ()
  "Prompt the user to select an executable command, filtering by :result-action 'eval'."
  (interactive)
  (let* ((eval-commands
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'eval))
                   ai--commands-config-map)))
         (command-displays (mapcar #'ai--get-command-display-name eval-commands))
         (command-alist (cl-mapcar #'cons command-displays eval-commands))
         (selected-display (completing-read ai--command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

(defun ai--get-all-available-commands ()
  "Get all available commands including those from config and additional actions."
  (ai--get-ordered-command-names))

(defun ai--get-command-unrestricted ()
  "Prompt the user to select any available command without restrictions."
  (interactive)
  (ai--get-command))

(defun ai--set-execution-model (model)
  "Set the execution model and execute hooks.
MODEL is the model configuration to be set."
  (let ((setup-function (map-elt model :setup-function)))
    (when setup-function
      (funcall setup-function))
    (setq ai-mode--execution-model model)
    (customize-save-variable 'ai-mode--execution-model model) ; Save the setting persistently
    (run-hooks 'ai-mode-change-model-hook)
    (ai-mode-update-mode-line-info)))

(defun ai--change-execution-backend (&optional model-name)
  "Change command backend interactively, or use MODEL-NAME if given."
  (interactive)
  (let* ((models (mapcar (lambda (item) `(,(map-elt item :name) ,item)) (ai-mode--get-models)))
         (value (or model-name
                    (completing-read ai--change-execution-backend-prompt (mapcar #'car models))))
         (model (ai-utils--find-model-config-by-name value (ai-mode--get-models))))

    (message "Setup model: %s" (pp-to-string model))
    (ai--set-execution-model model)
    (message "AI mode backend changed to '%s'." value)))


(defun ai--switch-file-instructions-enabled ()
  "Toggle file instructions for the current buffer."
  (setq ai--project-file-instructions-enabled (not ai--project-file-instructions-enabled)))

(cl-defun ai-perform-async-backend-query (context success-callback &key
                                                  (fail-callback nil)
                                                  (extra-params nil)
                                                  (model nil))
  "Execute CONTEXT by current backend asynchronously.
After successful execution, call SUCCESS-CALLBACK. If execution fails, call FAIL-CALLBACK if provided. EXTRA-PARAMS is a list of additional parameters for backend configuration."
  (let* ((execution-model (if model model (ai--get-current-model)))
         (execution-backend (map-elt execution-model :execution-backend))
         (current-buffer (current-buffer))
         (wrapped-success-callback (ai--progress-wrap-callback success-callback current-buffer))
         (wrapped-fail-callback (ai--progress-wrap-callback fail-callback current-buffer)))

    ;; Start progress indicator in current buffer
    (ai--progress-start (format "Processing with %s" (map-elt execution-model :name)) current-buffer)

    (funcall execution-backend
             context
             execution-model
             :success-callback wrapped-success-callback
             :fail-callback wrapped-fail-callback
             :update-usage-callback (ai--create-usage-statistics-callback)
             :enable-caching ai--prompt-caching-enabled)))

(defun ai-show ()
  "Execute command and show the response in a special buffer, filtering by show-compatible commands."
  (interactive)
  (let* ((command (ai--get-informational-command))
         (context (ai--get-executions-context-for-command command :default-result-action 'show)))
    (ai--execute-context context 'ai-utils--show-response-buffer)))

(defun ai-execute ()
  "Execute command and show the response for evaluation, filtering by eval-compatible commands."
  (interactive)
  (let* ((command (ai--get-executable-command))
         (context (ai--get-executions-context-for-command command :default-result-action 'eval)))
    (ai--execute-context context 'ai-utils--show-and-eval-response)))


(defun ai-perform ()
  "Execute request and apply the result based on command's specified result action or default to replace.
   If result action is 'replace', it replaces the selected region or inserts in current buffer.
   If result action is 'show', it shows the response in a special buffer.
   If result action is 'eval', it shows the response and asks for permission to evaluate.
   If result action is 'insert-at-point', it inserts the response at the cursor position."
  (interactive)
  (let* ((command (ai--get-command-unrestricted))
         (context (ai--get-executions-context-for-command command :default-result-action 'replace))
         (config (ai--get-command-config-by-type command 'replace))
         (result-action (map-elt config :result-action))
         (current-buffer (current-buffer))
         (cursor-position (point)))
    (cond
     ((eq result-action 'show)
      ;; If the selected command is meant to be shown, delegate
      (message "Command '%s' is informational. Displaying in a new buffer." command)
      (ai--execute-context context 'ai-utils--show-response-buffer))
     ((eq result-action 'eval)
      ;; Show response and ask for permission to evaluate
      (message "Command '%s' will generate code for evaluation." command)
      (ai--execute-context context 'ai-utils--show-and-eval-response))
     ((eq result-action 'insert-at-point)
      ;; Insert at the cursor position captured when the command was invoked
      (message "Command '%s' will insert response at cursor position." command)
      (ai--execute-context context (ai-utils--create-insert-at-point-callback current-buffer cursor-position)))
     ((eq result-action 'replace)
      ;; Default replace behavior
      (ai--execute-context context (ai-utils--replace-region-or-insert-in-current-buffer)))
     (t
      ;; Fallback for unconfigured or new actions
      (message "Unknown or unspecified result action for command '%s'. Defaulting to replace." command)
      (ai--execute-context context 'ai-utils--show-response-buffer)))))

(defun ai-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one."
  (interactive)
  (ai-completions--coordinator :action-type (ai--get-command-unrestricted) :strategy 'replace))

(defun ai-debug ()
  "Debug AI mode by printing region status and execution context."
  (interactive)
  (ai-utils--show-context-debug (ai--get-executions-context-for-command (ai--get-command-unrestricted) :model (ai--get-current-model))))

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

(defun ai--get-project-context-indicator ()
  "Return a single character indicator for the current project context mode."
  (cond
   ((eq ai--project-context-mode 'full-project) "P")
   ((eq ai--project-context-mode 'project-ai-summary) "S")
   ((eq ai--project-context-mode 'disabled) "D")
   (t "")))

(defun ai-mode-line-info ()
  "Return a formatted string describing the current AI mode state for the mode line."
  (let* ((model (ai--get-current-model))
         (project-indicator (ai--get-project-context-indicator))
         (cache-indicator (if ai--prompt-caching-enabled "C" ""))
         (progress-indicator (cond
                              ((and ai--progress-active
                                    (eq ai--progress-indicator-style 'spinner))
                               (let ((elapsed-time (when ai--progress-start-time
                                                     (ai--format-elapsed-time ai--progress-start-time))))
                                 (format "%s%s"
                                         (nth (% ai--progress-counter (length ai--progress-spinner-chars)) ai--progress-spinner-chars)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              ((and ai--progress-active
                                    (eq ai--progress-indicator-style 'dots))
                               (let ((elapsed-time (when ai--progress-start-time
                                                     (ai--format-elapsed-time ai--progress-start-time))))
                                 (format "%s%s"
                                         (make-string (% ai--progress-counter 4) ?.)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              (ai--progress-active "⚡")
                              (t "")))
         (context-info (if ai--progress-active
                           (when ai--progress-start-time
                             (format "%s" (if (string-empty-p progress-indicator) "" (format "%s" progress-indicator))))
                         (format "%s%s|%d/%d"
                                 project-indicator
                                 cache-indicator
                                 ai--current-precending-context-size
                                 ai--current-forwarding-context-size)))
         (ai-mode-line-section
          (format " AI[%s|%s]"
                  (map-elt model :name)
                  context-info)))
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
