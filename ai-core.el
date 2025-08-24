;;; ai-core.el --- Core AI functionality and command orchestration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; This file is part of ai-mode.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module serves as the central orchestrator for AI-mode, containing
;; the core business logic for AI command execution. It provides the main
;; functions that handle different types of AI operations:
;;
;; - ai-core-show: Execute informational AI commands and display results
;; - ai-core-execute: Execute AI commands that generate code for evaluation
;; - ai-core-perform: Execute AI commands with automatic result handling
;; - ai-core-perform-coordinator: Smart completion and continuation logic
;; - ai-core-debug: Debug and introspection utilities
;; - ai-core-show-audit: Show request audit for current project
;; - ai-core-open-extended-chat: Open chat with enriched context from all sources
;;
;; The module coordinates between command management, context management,
;; execution, and response processing to provide a seamless AI experience.
;;
;; New Architecture Support:
;; This module has been updated to work with the unified command registry
;; system, supporting ai-command structures and the new modifier system
;; while maintaining backward compatibility.

;;; Code:

(require 'ai-command-management)
(require 'ai-context-management)
(require 'ai-execution)
(require 'ai-response-processors)
(require 'ai-completions)
(require 'ai-model-management)
(require 'ai-utils)
(require 'ai-structs)
(require 'ai-request-audit)
(require 'ai-logging)
(require 'ai-chat)

;; ============================================================================
;; Core execution functions (using ai-command-management functions)
;; ============================================================================

(defun ai-core-show ()
  "Execute informational command using new command system."
  (let* ((command-struct (ai-command-management--get-show-commands))
         (context (ai-context-management--get-executions-context-for-command
                   command-struct
                   :model (ai-model-management-get-current))))
    (ai-execution--execute-context context 'ai-response-processors--show-response-buffer)))

(defun ai-core-execute ()
  "Execute command for code evaluation using new command system."
  (let* ((command-struct (ai-command-management--get-eval-commands))
         (context (ai-context-management--get-executions-context-for-command
                   command-struct
                   :model (ai-model-management-get-current))))
    (ai-execution--execute-context context 'ai-response-processors--show-and-eval-response)))

(defun ai-core-perform ()
  "Execute request and apply result based on command's specified result action or default to replace.
Uses new command system with ai-command structures when available."
  (let* ((command-struct (ai-command-management--get-unrestricted-command))
         (context (ai-context-management--get-executions-context-for-command
                   command-struct
                   :model (ai-model-management-get-current))))
    (ai-core--execute-with-result-action context)))


(defun ai-core--execute-with-result-action (context)
  "Execute CONTEXT with appropriate handler based on command's result action."
  (let* ((command-struct (ai-structs--get-execution-context-ai-command context))
         (result-action (ai-structs--get-result-action command-struct))
         (command-name (ai-structs--get-command-name command-struct))
         (buffer-state (ai-structs--get-execution-context-buffer-state context))
         (current-buffer (when buffer-state (ai-buffer-state-buf-obj buffer-state)))
         (cursor-position (when buffer-state (ai-buffer-state-cur-point buffer-state))))

    (cond
     ((eq result-action 'show)
      ;; If the selected command is meant to be shown, delegate
      (ai-logging--message 'info "core" "Command '%s' is informational. Displaying in a new buffer." command-name)
      (ai-execution--execute-context context 'ai-response-processors--show-response-buffer :fail-callback 'ai-response-processors--create-error-message-callback))

     ((eq result-action 'eval)
      ;; Show response and ask for permission to evaluate
      (ai-logging--message 'info "core" "Command '%s' will generate code for evaluation." command-name)
      (ai-execution--execute-context context 'ai-response-processors--show-and-eval-response :fail-callback 'ai-response-processors--create-error-message-callback))

     ((eq result-action 'insert-at-point)
      ;; Insert at the cursor position captured when the command was invoked
      (ai-logging--message 'info "core" "Command '%s' will insert response at cursor position." command-name)
      (ai-execution--execute-context context (ai-response-processors--create-insert-at-point-callback current-buffer cursor-position) :fail-callback 'ai-response-processors--create-error-message-callback))

     ((eq result-action 'replace)
      ;; Use smart replace callback that handles both patch and direct replacement
      (ai-execution--execute-context context (ai-response-processors--create-smart-replace-callback current-buffer) :fail-callback 'ai-response-processors--create-error-message-callback))

     (t
      ;; Fallback for unconfigured or new actions
      (ai-logging--message 'warn "core" "Unknown or unspecified result action for command '%s'. Defaulting to show." command-name)
      (ai-execution--execute-context context 'ai-response-processors--show-response-buffer :fail-callback 'ai-response-processors--create-error-message-callback)))))


(defun ai-core-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one.
Uses new command system when possible."
  (let* ((command-struct (ai-command-management--get-coordinator-command))
         (command-name (ai-structs--get-command-canonical-name command-struct)))
    (ai-completions--coordinator :action-type command-name :strategy 'replace)))

(defun ai-core-debug ()
  "Debug AI mode by printing region status and execution context.
Uses new command system when possible."
  (let* ((command-struct (ai-command-management--get-debug-command))
         (context (ai-context-management--get-executions-context-for-command
                   command-struct
                   :model (ai-model-management-get-current))))
    (ai-utils--show-context-debug context)))

(defun ai-core-show-audit ()
  "Show request audit for the current project."
  (interactive)
  (ai-request-audit-show-requests-buffer))

(defun ai-core-open-extended-chat ()
  "Open chat for current buffer with extended context including all available context sources.
This function creates a new chat session and enriches it with comprehensive context from:
- Project context (files, summaries)
- Memory files (global, local, team)
- Buffer-specific context
- Context pool items
- Global system prompts and buffer-bound prompts

The extended context is automatically added to the chat history to provide
rich background information for the conversation."
  (interactive)
  (ai-logging--message 'info "core" "Opening extended chat with enriched context for buffer: %s" (buffer-name))

  ;; Check if there is any extended context available
  (let ((has-context (ai-context-management--has-extended-context-p)))
    (if (not has-context)
        (progn
          (ai-logging--message 'info "core" "No extended context available, opening regular chat")
          (ai-chat-open-for-current-buffer))

      ;; Create new chat session with extended context strategy
      (let ((chat-buffer (ai-chat--find-or-create-session-for-buffer
                          (current-buffer)
                          :force-new t
                          :initial-strategy 'extended)))
        (when chat-buffer
          (pop-to-buffer-same-window chat-buffer)
          (let ((extended-contexts (ai-context-management--get-all-extended-context)))
            (ai-logging--message 'info "core" "Extended chat created with %d context items"
                                 (length extended-contexts))
            (message "Extended chat opened with enriched context (%d items)"
                     (length extended-contexts))))))))

(provide 'ai-core)

;;; ai-core.el ends here
