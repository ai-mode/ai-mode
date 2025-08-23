;;; ai-execution.el --- AI request execution for AI mode -*- lexical-binding: t -*-

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
;; This module manages AI request execution and asynchronous interaction with AI backends,
;; handling success and failure callbacks for the AI mode package.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-prompt-management)
(require 'ai-model-management)
(require 'ai-command-management)
(require 'ai-context-management)
(require 'ai-usage)
(require 'ai-progress)
(require 'ai-request-audit)


(defun ai-execution--wrap-callback-with-progress-and-audit (callback context request-id callback-type &optional target-buffer)
  "Wrap CALLBACK with progress stopping and audit handling.
CONTEXT is the original execution context passed as first parameter.
REQUEST-ID is the audit request identifier.
CALLBACK-TYPE should be 'success' or 'fail'.
TARGET-BUFFER is the buffer where progress should be stopped."
  (lambda (messages &optional usage-stats)
    ;; Stop progress first
    (ai-progress-stop-single-request target-buffer)

    ;; Handle audit
    (cond
     ((eq callback-type 'success)
      (ai-request-audit-complete-request request-id messages usage-stats))
     ((eq callback-type 'fail)
      (ai-request-audit-fail-request request-id messages)))

    ;; Call the original callback if provided
    (when (functionp callback)
      (if (eq callback-type 'success)
          (funcall callback context messages usage-stats)
        (funcall callback context messages)))))

;; Caching functionality moved from ai-mode.el
(defcustom ai-execution--prompt-caching-enabled nil
  "Enable prompt caching for AI requests.
When enabled, AI providers that support prompt caching will evaluate
content for caching based on the provider's specific rules."
  :type 'boolean
  :group 'ai-mode)

(defcustom ai-execution--replace-action-use-patch nil
  "Use unified patch format for replace actions instead of direct replacement.
When enabled, AI models will generate unified patches that are applied
using standard patch utilities. When disabled, AI responses directly
replace the selected content."
  :type 'boolean
  :group 'ai-mode)

(defun ai-execution--toggle-prompt-caching ()
  "Toggle prompt caching for AI requests."
  (setq ai-execution--prompt-caching-enabled (not ai-execution--prompt-caching-enabled))
  (customize-save-variable 'ai-execution--prompt-caching-enabled ai-execution--prompt-caching-enabled)
  (message "Prompt caching %s"
           (if ai-execution--prompt-caching-enabled "enabled" "disabled")))

(defun ai-execution--toggle-replace-action-use-patch ()
  "Toggle unified patch mode for replace actions."
  (setq ai-execution--replace-action-use-patch (not ai-execution--replace-action-use-patch))
  (customize-save-variable 'ai-execution--replace-action-use-patch ai-execution--replace-action-use-patch)
  (message "Replace action patch mode %s"
           (if ai-execution--replace-action-use-patch "enabled" "disabled")))

(cl-defun ai-execution-perform-async-backend-query (context success-callback &key
                                                           (fail-callback nil)
                                                           (extra-params nil)
                                                           (model nil))
  "Execute CONTEXT by current backend asynchronously.
After successful execution, call SUCCESS-CALLBACK with context as first parameter.
If execution fails, call FAIL-CALLBACK with context as first parameter.
EXTRA-PARAMS is a list of additional parameters for backend configuration.
MODEL can optionally specify a specific model to use."
  (let* ((execution-model (if model model (ai-model-management-get-current)))
         (execution-backend (map-elt execution-model :execution-backend))
         (current-buffer (current-buffer))
         (request-id (plist-get context :request-id))
         (command-config (plist-get context :command-config))
         (command (plist-get command-config :command))

         ;; Start audit if enabled
         (audit-request-id (ai-request-audit-start-request
                            request-id
                            command
                            execution-model
                            context))

         ;; Wrap callbacks with progress and audit handling
         (wrapped-success-callback
          (ai-execution--wrap-callback-with-progress-and-audit
           success-callback context audit-request-id 'success current-buffer))

         (wrapped-fail-callback
          (ai-execution--wrap-callback-with-progress-and-audit
           fail-callback context audit-request-id 'fail current-buffer)))

    ;; Start single request progress indicator in current buffer
    (ai-progress-start-single-request (format "Processing with %s" (map-elt execution-model :name)) current-buffer)

    (funcall execution-backend
             context
             execution-model
             :request-id audit-request-id
             :success-callback wrapped-success-callback
             :fail-callback wrapped-fail-callback
             :update-usage-callback (ai-usage-create-usage-statistics-callback)
             :enable-caching ai-execution--prompt-caching-enabled)))

(cl-defun ai-execution--execute-context (context success-callback &key (fail-callback nil) (model nil))
  "Execute CONTEXT using SUCCESS-CALLBACK and optional FAIL-CALLBACK with an optional MODEL.
SUCCESS-CALLBACK and FAIL-CALLBACK will receive context as first parameter."
  (ai-execution-perform-async-backend-query context success-callback :fail-callback fail-callback :model model))

(cl-defun ai-execution--execute-command (command success-callback &key (fail-callback nil) (model nil))
  "Execute COMMAND by dispatching to the appropriate backend using SUCCESS-CALLBACK.
SUCCESS-CALLBACK and FAIL-CALLBACK will receive context as first parameter.
Optionally use FAIL-CALLBACK and specify a MODEL."
  (let* ((execution-model (if model model (ai-model-management-get-current)))
         (execution-backend (map-elt execution-model :execution-backend))
         (context (ai-context-management--get-executions-context-for-command command :model execution-model))
         (current-buffer (current-buffer))
         (request-id (plist-get context :request-id))
         (command-config (plist-get context :command-config))
         (command-from-config (or (plist-get command-config :command)
                                 (plist-get command-config :action)
                                 command))

         ;; Start audit if enabled
         (audit-request-id (ai-request-audit-start-request
                            request-id
                            command-from-config
                            execution-model
                            context))

         ;; Wrap callbacks with progress and audit handling
         (wrapped-success-callback
          (ai-execution--wrap-callback-with-progress-and-audit
           success-callback context audit-request-id 'success current-buffer))

         (wrapped-fail-callback
          (ai-execution--wrap-callback-with-progress-and-audit
           fail-callback context audit-request-id 'fail current-buffer)))

    ;; Start single request progress indicator
    (ai-progress-start-single-request (format "Executing %s with %s" command-from-config (map-elt execution-model :name)) current-buffer)

    (funcall execution-backend
             context
             execution-model
             :request-id audit-request-id
             :success-callback wrapped-success-callback
             :fail-callback wrapped-fail-callback
             :update-usage-callback (ai-usage-create-usage-statistics-callback)
             :enable-caching ai-execution--prompt-caching-enabled)))

(provide 'ai-execution)

;;; ai-execution.el ends here
