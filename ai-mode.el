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
;; see https://www.gnu.org/licenses/.

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
;;
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
;; - C-c i c b - Open chat bound to current buffer (with configurable startup context)
;; - C-c i c s - Send selection/region to the bound chat as context
;; - C-c i c f - Send entire buffer to the bound chat as context
;; - C-c i c o - Choose and open a chat session bound to the current buffer
;; - C-c i p c - Execute AI coordinator (smart completion)
;;
;; Command Management (prefix C-c i m):
;; - C-c i m e - Edit command instructions
;; - C-c i m d - Describe command modifiers
;; - C-c i m c - Create command with modifiers
;; - C-c i m r - Refresh command registry
;; - C-c i m s - Show command registry status
;;
;; Context Management (prefix C-c i k):
;; - C-c i k a - Add to context pool
;; - C-c i k r - Add region to context pool
;; - C-c i k f - Add file to context pool
;; - C-c i k c - Clear context pool
;; - C-c i k x - Remove from context pool
;; - C-c i k s - Switch project context mode
;; - C-c i k b - Add buffer-bound prompts
;; - C-c i k B - Clear buffer-bound prompts
;; - C-c i k m - Add to global memory
;; - C-c i k M - Clear global memory
;;
;; Execution Control (prefix C-c i e):
;; - C-c i e c - Toggle prompt caching
;; - C-c i e p - Toggle replace action patch mode
;;
;; Indexing (prefix C-c i i):
;; - C-c i i u - Update project files summary index
;; - C-c i i t - Toggle indexing context
;; - C-c i i R - Reindex project with context
;; - C-c i i s - Switch indexing strategy
;; - C-c i i v - Select index version
;; - C-c i i l - List index versions
;; - C-c i i d - Delete old index versions
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
(require 'ai-core)

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
    ;; Main commands
    (define-key keymap (kbd "c c") 'ai-chat)
    (define-key keymap (kbd "c m") 'ai-change-model)
    (define-key keymap (kbd "c b") 'ai-chat-for-buffer)
    (define-key keymap (kbd "c s") 'ai-chat-send-selection-to-chat)
    (define-key keymap (kbd "c f") 'ai-chat-send-file-to-chat)
    (define-key keymap (kbd "c o") 'ai-chat-choose-session)
    (define-key keymap (kbd "p c") 'ai-perform-coordinator)
    (define-key keymap (kbd "r") 'ai-perform)
    (define-key keymap (kbd "s") 'ai-show)
    (define-key keymap (kbd "x") 'ai-execute)

    ;; Command management (prefix: m)
    (define-key keymap (kbd "m e") 'ai-command-edit-instructions)
    (define-key keymap (kbd "m d") 'ai-command-describe-modifiers)
    (define-key keymap (kbd "m c") 'ai-command-create-modified)
    (define-key keymap (kbd "m r") 'ai-command-registry-refresh)
    (define-key keymap (kbd "m s") 'ai-command-registry-status)

    ;; Context management (prefix: k)
    (define-key keymap (kbd "k a") 'ai-context-add-to-pool)
    (define-key keymap (kbd "k r") 'ai-context-add-region)
    (define-key keymap (kbd "k f") 'ai-context-add-file)
    (define-key keymap (kbd "k c") 'ai-context-clear-pool)
    (define-key keymap (kbd "k x") 'ai-context-remove-from-pool)
    (define-key keymap (kbd "k s") 'ai-context-switch-project-mode)
    (define-key keymap (kbd "k b") 'ai-context-add-buffer-prompts)
    (define-key keymap (kbd "k B") 'ai-context-clear-buffer-prompts)
    (define-key keymap (kbd "k m") 'ai-context-add-to-memory)
    (define-key keymap (kbd "k M") 'ai-context-clear-memory)

    ;; Execution control (prefix: e)
    (define-key keymap (kbd "e c") 'ai-execution-toggle-caching)
    (define-key keymap (kbd "e p") 'ai-execution-toggle-patch-mode)

    ;; Indexing (prefix: i)
    (define-key keymap (kbd "i u") 'ai-indexing-update)
    (define-key keymap (kbd "i t") 'ai-indexing-toggle-context)
    (define-key keymap (kbd "i R") 'ai-indexing-reindex-with-context)
    (define-key keymap (kbd "i s") 'ai-indexing-switch-strategy)
    (define-key keymap (kbd "i v") 'ai-indexing-select-version)
    (define-key keymap (kbd "i l") 'ai-indexing-list-versions)
    (define-key keymap (kbd "i d") 'ai-indexing-delete-old-versions)

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
  "Initialize prompt caches and command registry."
  (ai-prompt-management--update-caches)
  ;; Refresh command registry to ensure it's up to date
  (ai-command-management--refresh-command-registry))

;; Core command aliases
(defun ai-change-model (&optional model-name)
  "Change the current AI model/backend.

This is a convenient alias for `ai-model-management-change'.
If MODEL-NAME is provided, change to that model directly.
Otherwise, prompt the user to select from available models."
  (interactive)
  (ai-model-management-change model-name))

(defun ai-show ()
  "Execute command and show the response in a special buffer.
Uses the new unified command system with proper filtering."
  (interactive)
  ;; Use new command system with filtering for show-compatible commands
  (ai-core-show))

(defun ai-execute ()
  "Execute command and show the response for evaluation.
Uses the new unified command system with proper filtering."
  (interactive)
  ;; Use new command system with filtering for eval-compatible commands
  (ai-core-execute))

(defun ai-perform ()
  "Execute request and apply the result based on command's specified result action.
Uses the new unified command system for enhanced command selection."
  (interactive)
  ;; Use new command system for enhanced command handling
  (ai-core-perform))

(defun ai-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one."
  (interactive)
  (ai-core-perform-coordinator))

(defun ai-debug ()
  "Debug AI mode by printing region status and execution context."
  (interactive)
  (ai-core-debug))

;; Chat aliases (per-buffer chat helpers)
;;;###autoload
(defun ai-chat-for-buffer (&optional force-new)
  "Open or create a chat bound to the current buffer.

With prefix argument FORCE-NEW, always start a new chat session for the buffer.
Startup context (buffer, region, and/or user instructions) is controlled by
`ai-chat-startup-context-strategy'."
  (interactive "P")
  (ai-chat-open-for-current-buffer force-new))

;;;###autoload
(defun ai-chat-send-selection-to-chat ()
  "Send the active region (or prompt to use the entire buffer) to the bound chat as context.

The data is sent according to `ai-chat-send-enrichment-as' (additional-context,
file-context, or selection)."
  (interactive)
  (ai-chat-send-region-to-bound-chat))

;;;###autoload
(defun ai-chat-send-file-to-chat ()
  "Send the entire current buffer to the bound chat as context.

The data is sent according to `ai-chat-send-enrichment-as' (additional-context
or file-context)."
  (interactive)
  (ai-chat-send-buffer-to-bound-chat))

;;;###autoload
(defun ai-chat-choose-session ()
  "Choose and open one of the chat sessions bound to the current buffer."
  (interactive)
  (ai-chat-choose-session-for-current-buffer))

;; Command management aliases (prefix: ai-command-)
(defun ai-command-edit-instructions ()
  "Edit instruction files for commands with improved interface."
  (interactive)
  (ai-command-management-edit-command-instructions))

(defun ai-command-describe-modifiers ()
  "Interactively describe modifiers for a selected command."
  (interactive)
  (ai-command-management-describe-command-modifiers))

(defun ai-command-create-modified ()
  "Interactively create a new file-based command with modifiers."
  (interactive)
  (ai-command-management-create-modified-command))

(defun ai-command-registry-refresh ()
  "Manually refresh command registry.
Forces an update of all command providers and rebuilds the command registry."
  (interactive)
  (ai-command-management--refresh-command-registry)
  (message "Command registry refreshed"))

(defun ai-command-registry-status ()
  "Show status of command registry.
Displays the current number of commands and registry statistics."
  (interactive)
  (ai-command-management--ensure-registry-updated)
  (let ((count (hash-table-count ai-command-management--command-registry))
        (providers (length ai-command-management--command-providers)))
    (message "Command registry contains %d commands from %d providers" count providers)))

;; Context management aliases (prefix: ai-context-)
(defun ai-context-add-to-pool ()
  "Capture user input and add it to the context pool."
  (interactive)
  (ai-context-management--capture-user-input))

(defun ai-context-add-region ()
  "Capture region snippet and add it to the context pool."
  (interactive)
  (ai-context-management--capture-region-snippet))

(defun ai-context-add-file ()
  "Capture entire file content and add it to the context pool."
  (interactive)
  (ai-context-management--capture-file-context))

(defun ai-context-clear-pool ()
  "Clear the temporary context pool."
  (interactive)
  (ai-context-management--clear-context-pool))

(defun ai-context-remove-from-pool ()
  "Remove selected item from context pool."
  (interactive)
  (ai-context-management--remove-from-context-pool))

(defun ai-context-switch-project-mode ()
  "Interactively switch the project context mode."
  (interactive)
  (ai-context-management--switch-project-context-mode))

(defun ai-context-add-buffer-prompts ()
  "Add buffer-specific instructions from region or user input."
  (interactive)
  (ai-context-management--add-buffer-bound-prompts
   (if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (read-string "Enter buffer bound prompt: "))))

(defun ai-context-clear-buffer-prompts ()
  "Clear all buffer-specific instructions."
  (interactive)
  (ai-context-management--clear-buffer-bound-prompts))

(defun ai-context-add-to-memory ()
  "Add content to global memory context."
  (interactive)
  (ai-context-management--add-to-global-memory
   (if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (read-string "Enter instruction: "))))

(defun ai-context-clear-memory ()
  "Clear the global memory context."
  (interactive)
  (ai-context-management--clear-global-memory))

;; Execution control aliases (prefix: ai-execution-)
(defun ai-execution-toggle-caching ()
  "Toggle prompt caching for AI requests."
  (interactive)
  (ai-execution--toggle-prompt-caching))

(defun ai-execution-toggle-patch-mode ()
  "Toggle unified patch mode for replace actions."
  (interactive)
  (ai-execution--toggle-replace-action-use-patch))

;; Indexing aliases (prefix: ai-indexing-)
(defun ai-indexing-update ()
  "Update the project files summary index."
  (interactive)
  (ai-mode-indexing-update-project-files-summary-index))

(defun ai-indexing-toggle-context ()
  "Toggle inclusion of existing context in indexing process."
  (interactive)
  (ai-mode-indexing-toggle-indexing-context))

(defun ai-indexing-reindex-with-context ()
  "Reindex the entire project with existing context enabled."
  (interactive)
  (ai-mode-indexing-reindex-project-with-context))

(defun ai-indexing-switch-strategy ()
  "Interactively switch the indexing strategy."
  (interactive)
  (ai-mode-indexing-switch-indexing-strategy))

(defun ai-indexing-select-version ()
  "Interactively select and load an index version for the current project."
  (interactive)
  (ai-mode-indexing-select-index-version))

(defun ai-indexing-list-versions ()
  "List all available index versions for the current project."
  (interactive)
  (ai-mode-indexing-list-index-versions))

(defun ai-indexing-delete-old-versions ()
  "Interactively delete old index versions beyond retention depth."
  (interactive)
  (ai-mode-indexing-delete-old-index-versions))

(add-hook 'ai-model-management-change-hook 'ai-mode-line-update-mode-line-info)

(provide 'ai-mode)

;;; ai-mode.el ends here
