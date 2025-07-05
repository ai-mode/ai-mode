;;; ai-mode.el --- AI interaction mode for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

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
;; 4. Use `C-c i r` to start executing AI commands
;;
;; Key Bindings (default prefix C-c i):
;; - C-c i r   - Execute AI command with replace action
;; - C-c i s   - Show AI response in separate buffer
;; - C-c i x   - Execute and evaluate AI-generated code
;; - C-c i c c - Start AI chat session
;; - C-c i c m - Change AI model (alias)
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

(require 'ai)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-completions)
(require 'ai-chat)
(require 'ai-mode-line)
(require 'ai-prompt-management)
(require 'ai-command-management)
(require 'ai-model-management)
(require 'ai-context-management)
(require 'ai-execution)
(require 'ai-mode-indexing)
(require 'ai-response-processors)

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

(defvar ai-command-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "c c") 'ai-chat)
    (define-key keymap (kbd "c m") 'ai-change-model)
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
    (define-key keymap (kbd "p s") 'ai-context-management--switch-project-context-mode)
    (define-key keymap (kbd "p u") 'ai-mode-indexing-update-project-files-summary-index)
    (define-key keymap (kbd "e i") 'ai-command-management-edit-command-instructions)
    (define-key keymap (kbd "m d") 'ai-command-management-describe-command-modifiers)
    (define-key keymap (kbd "m c") 'ai-command-management-create-modified-command)
    (define-key keymap (kbd "i t") 'ai-mode-indexing-toggle-indexing-context)
    (define-key keymap (kbd "i r") 'ai-mode-indexing-reindex-project-with-context)
    (define-key keymap (kbd "i s") 'ai-mode-indexing-switch-indexing-strategy)
    (define-key keymap (kbd "i v") 'ai-mode-indexing-select-index-version)
    (define-key keymap (kbd "i l") 'ai-mode-indexing-list-index-versions)
    (define-key keymap (kbd "i d") 'ai-mode-indexing-delete-old-index-versions)
    (define-key keymap (kbd "c t") 'ai-execution--toggle-prompt-caching)
    (define-key keymap (kbd "p t") 'ai-execution--toggle-replace-action-use-patch)
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
    (ai-mode-line-update-mode-line-info)))

;;;###autoload
(define-globalized-minor-mode global-ai-mode ai-mode ai-mode-on
  :group 'ai)

(defun ai-mode-on ()
  "Turn on AI mode."
  (interactive)
  (ai-mode 1)
  (ai-mode-line-update-mode-line-info))

(defun ai-mode-pre-command ()
  "Function called before each command in `ai-mode`.")

(defun ai-mode-post-command ()
  "Function called after each command in `ai-mode`.")

(defun ai-mode-global-init-hook ()
  "Function that runs when `global-ai-mode` is initialized."
  (message "Global AI mode is now enabled or disabled!")
  (ai-mode-line-update-mode-line-info)
  (if global-ai-mode
      (progn
        (ai-mode--initialize-system)
        (message "Global AI mode is now enabled!"))
    (message "Global AI mode is now disabled!")))

(add-hook 'global-ai-mode-hook 'ai-mode-global-init-hook)

(defun ai-mode--initialize-system ()
  "Initialize prompt caches."
  (ai-prompt-management--update-caches))

(defun ai-explain-code-region ()
  "Explain the selected code region and display the explanation in a help buffer."
  (interactive)
  (ai-execution--execute-context (ai-context-management--get-executions-context-for-command "explain") 'ai-response-processors--show-explain-help-buffer))

(defun ai-change-model (&optional model-name)
  "Change the current AI model/backend.

This is a convenient alias for `ai-model-management-change'.
If MODEL-NAME is provided, change to that model directly.
Otherwise, prompt the user to select from available models."
  (interactive)
  (ai-model-management-change model-name))

(defun ai-show ()
  "Execute command and show the response in a special buffer, filtering by show-compatible commands."
  (interactive)
  (let* ((command (ai-command-management--get-informational-command))
         (context (ai-context-management--get-executions-context-for-command command :default-result-action 'show)))
    (ai-execution--execute-context context 'ai-response-processors--show-response-buffer)))

(defun ai-execute ()
  "Execute command and show the response for evaluation, filtering by eval-compatible commands."
  (interactive)
  (let* ((command (ai-command-management--get-executable-command))
         (context (ai-context-management--get-executions-context-for-command command :default-result-action 'eval)))
    (ai-execution--execute-context context 'ai-response-processors--show-and-eval-response)))

(defun ai-perform ()
  "Execute request and apply the result based on command's specified result action or default to replace.
   If result action is 'replace', it replaces the selected region or inserts in current buffer.
   If result action is 'show', it shows the response in a special buffer.
   If result action is 'eval', it shows the response and asks for permission to evaluate.
   If result action is 'insert-at-point', it inserts the response at the cursor position."
  (interactive)
  (let* ((command (ai-command-management--get-command-unrestricted))
         (context (ai-context-management--get-executions-context-for-command command :default-result-action 'replace))
         (config (ai-command-management--get-command-config-by-type command 'replace))
         (result-action (map-elt config :result-action))
         (current-buffer (current-buffer))
         (cursor-position (point)))
    (cond
     ((eq result-action 'show)
      ;; If the selected command is meant to be shown, delegate
      (message "Command '%s' is informational. Displaying in a new buffer." command)
      (ai-execution--execute-context context 'ai-response-processors--show-response-buffer))
     ((eq result-action 'eval)
      ;; Show response and ask for permission to evaluate
      (message "Command '%s' will generate code for evaluation." command)
      (ai-execution--execute-context context 'ai-response-processors--show-and-eval-response))
     ((eq result-action 'insert-at-point)
      ;; Insert at the cursor position captured when the command was invoked
      (message "Command '%s' will insert response at cursor position." command)
      (ai-execution--execute-context context (ai-response-processors--create-insert-at-point-callback current-buffer cursor-position)))
     ((eq result-action 'replace)
      ;; Apply patch if patch mode is enabled, otherwise use regular replace behavior
      (if (bound-and-true-p ai-execution--replace-action-use-patch)
          (progn
            (message "Command '%s' will generate and apply a unified patch." command)
            (ai-execution--execute-context context (ai-response-processors--create-patch-apply-callback current-buffer)))
        (ai-execution--execute-context context (ai-response-processors--replace-region-or-insert-in-current-buffer))))
     (t
      ;; Fallback for unconfigured or new actions
      (message "Unknown or unspecified result action for command '%s'. Defaulting to replace." command)
      (ai-execution--execute-context context 'ai-response-processors--show-response-buffer)))))

(defun ai-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one."
  (interactive)
  (ai-completions--coordinator :action-type (ai-command-management--get-command-unrestricted) :strategy 'replace))

(defun ai-debug ()
  "Debug AI mode by printing region status and execution context."
  (interactive)
  (ai-utils--show-context-debug (ai-context-management--get-executions-context-for-command (ai-command-management--get-command-unrestricted) :model (ai-model-management-get-current))))

(add-hook 'ai-model-management-change-hook 'ai-mode-line-update-mode-line-info)

(provide 'ai-mode)

;;; ai-mode.el ends here
