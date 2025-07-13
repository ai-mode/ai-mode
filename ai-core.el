;;; ai-core.el --- Core AI functionality and command orchestration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; This file is part of ai-mode.

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
;;
;; The module coordinates between command management, context management,
;; execution, and response processing to provide a seamless AI experience.

;;; Code:

(require 'ai-command-management)
(require 'ai-context-management)
(require 'ai-execution)
(require 'ai-response-processors)
(require 'ai-completions)
(require 'ai-model-management)
(require 'ai-utils)

(defun ai-core-show ()
  "Execute command and show the response in a special buffer, filtering by show-compatible commands."
  (let* ((command (ai-command-management--get-informational-command))
         (context (ai-context-management--get-executions-context-for-command command :default-result-action 'show)))
    (ai-execution--execute-context context 'ai-response-processors--show-response-buffer)))

(defun ai-core-execute ()
  "Execute command and show the response for evaluation, filtering by eval-compatible commands."
  (let* ((command (ai-command-management--get-executable-command))
         (context (ai-context-management--get-executions-context-for-command command :default-result-action 'eval)))
    (ai-execution--execute-context context 'ai-response-processors--show-and-eval-response)))

(defun ai-core-perform ()
  "Execute request and apply the result based on command's specified result action or default to replace.
   If result action is 'replace', it replaces the selected region or inserts in current buffer.
   If result action is 'show', it shows the response in a special buffer.
   If result action is 'eval', it shows the response and asks for permission to evaluate.
   If result action is 'insert-at-point', it inserts the response at the cursor position."
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

(defun ai-core-perform-coordinator ()
  "Decide whether to continue the previous process of supplementation or to start a new one."
  (ai-completions--coordinator :action-type (ai-command-management--get-command-unrestricted) :strategy 'replace))

(defun ai-core-debug ()
  "Debug AI mode by printing region status and execution context."
  (ai-utils--show-context-debug (ai-context-management--get-executions-context-for-command (ai-command-management--get-command-unrestricted) :model (ai-model-management-get-current))))

(defun ai-core-show-audit ()
  "Show request audit for the current project."
  (interactive)
  (ai-request-audit-show-requests-buffer))

(provide 'ai-core)

;;; ai-core.el ends here
