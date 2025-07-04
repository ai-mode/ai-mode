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

;; Progress tracking variables
(defvar ai-execution--progress-message nil
  "Current progress message displayed in mode line.")

(defvar ai-execution--progress-start-time nil
  "Time when current progress operation started.")

(defvar ai-execution--progress-target-buffer nil
  "Buffer where progress was initiated.")

;; Timer-based progress variables
(defvar-local ai-execution--progress-timer nil
  "Timer for progress indicator animation.")

(defvar-local ai-execution--progress-counter 0
  "Counter for progress indicator animation.")

(defvar-local ai-execution--progress-active nil
  "Flag indicating if progress indicator is currently active.")

;; Callback registration system
(defvar ai-execution--progress-start-callbacks nil
  "List of callback functions called when progress starts.")

(defvar ai-execution--progress-stop-callbacks nil
  "List of callback functions called when progress stops.")

(defun ai-execution-register-progress-start-callback (callback)
  "Register a CALLBACK function to be called when progress starts.
The callback will be called with the same arguments as `ai-execution--progress-start'."
  (when (functionp callback)
    (add-to-list 'ai-execution--progress-start-callbacks callback)))

(defun ai-execution-register-progress-stop-callback (callback)
  "Register a CALLBACK function to be called when progress stops.
The callback will be called with the same arguments as `ai-execution--progress-stop'."
  (when (functionp callback)
    (add-to-list 'ai-execution--progress-stop-callbacks callback)))

(defun ai-execution--progress-start (&optional message target-buffer)
  "Start progress tracking with optional MESSAGE in TARGET-BUFFER or current buffer."
  (let ((buffer (or target-buffer (current-buffer))))
    (setq ai-execution--progress-message (or message "AI request in progress")
          ai-execution--progress-start-time (current-time)
          ai-execution--progress-target-buffer buffer)

    ;; Set buffer-local progress variables
    (with-current-buffer buffer
      (setq ai-execution--progress-active t
            ai-execution--progress-counter 0))

    ;; Call registered start callbacks
    (dolist (callback ai-execution--progress-start-callbacks)
      (condition-case err
          (funcall callback message buffer)
        (error (message "Error in progress start callback: %s" (error-message-string err)))))

    (force-mode-line-update)))

(defun ai-execution--progress-stop (&optional target-buffer)
  "Stop progress tracking in TARGET-BUFFER or current buffer."
  (let ((buffer (or target-buffer ai-execution--progress-target-buffer (current-buffer))))
    (when (or (null ai-execution--progress-target-buffer)
              (eq buffer ai-execution--progress-target-buffer))

      ;; Call registered stop callbacks before clearing state
      (dolist (callback ai-execution--progress-stop-callbacks)
        (condition-case err
            (funcall callback buffer)
          (error (message "Error in progress stop callback: %s" (error-message-string err)))))

      (setq ai-execution--progress-message nil
            ai-execution--progress-start-time nil
            ai-execution--progress-target-buffer nil)

      ;; Clean up buffer-local progress variables
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq ai-execution--progress-active nil)
          (when ai-execution--progress-timer
            (cancel-timer ai-execution--progress-timer)
            (setq ai-execution--progress-timer nil))))

      (force-mode-line-update))))

(defun ai-execution--progress-wrap-callback (callback &optional target-buffer)
  "Wrap CALLBACK to stop progress in TARGET-BUFFER when called.
Returns the original callback if it's a function, or a simple progress stopper if callback is nil."
  (let ((buffer (or target-buffer (current-buffer))))
    (if callback
        (lambda (&rest args)
          (ai-execution--progress-stop buffer)
          (apply callback args))
      (lambda (&rest args)
        (ai-execution--progress-stop buffer)))))

(defun ai-execution--get-progress-info ()
  "Get current progress information for mode line display.
Returns a plist with progress details, or nil if no progress.
The plist contains:
:message (string) - current message
:start-time (time) - start time of the request
:active (boolean) - true if progress is active
:counter (integer) - current animation counter
:target-buffer (buffer) - buffer where progress was initiated"
  (when ai-execution--progress-message
    (let ((target-buffer (or ai-execution--progress-target-buffer (current-buffer))))
      `(:message ,ai-execution--progress-message
        :start-time ,ai-execution--progress-start-time
        :active ,(and (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-execution--progress-active))
        :counter ,(if (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-execution--progress-counter)
                    0)
        :target-buffer ,target-buffer))))

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
  (interactive)
  (setq ai-execution--prompt-caching-enabled (not ai-execution--prompt-caching-enabled))
  (customize-save-variable 'ai-execution--prompt-caching-enabled ai-execution--prompt-caching-enabled)
  (message "Prompt caching %s"
           (if ai-execution--prompt-caching-enabled "enabled" "disabled")))

(defun ai-execution--toggle-replace-action-use-patch ()
  "Toggle unified patch mode for replace actions."
  (interactive)
  (setq ai-execution--replace-action-use-patch (not ai-execution--replace-action-use-patch))
  (customize-save-variable 'ai-execution--replace-action-use-patch ai-execution--replace-action-use-patch)
  (message "Replace action patch mode %s"
           (if ai-execution--replace-action-use-patch "enabled" "disabled")))

(cl-defun ai-execution-perform-async-backend-query (context success-callback &key
                                                           (fail-callback nil)
                                                           (extra-params nil)
                                                           (model nil))
  "Execute CONTEXT by current backend asynchronously.
After successful execution, call SUCCESS-CALLBACK. If execution fails, call FAIL-CALLBACK if provided.
EXTRA-PARAMS is a list of additional parameters for backend configuration.
MODEL can optionally specify a specific model to use."
  (let* ((execution-model (if model model (ai-model-management-get-current)))
         (execution-backend (map-elt execution-model :execution-backend))
         (current-buffer (current-buffer))
         (wrapped-success-callback (ai-execution--progress-wrap-callback success-callback current-buffer))
         (wrapped-fail-callback (ai-execution--progress-wrap-callback fail-callback current-buffer)))

    ;; Start progress indicator in current buffer
    (ai-execution--progress-start (format "Processing with %s" (map-elt execution-model :name)) current-buffer)

    (funcall execution-backend
             context
             execution-model
             :success-callback wrapped-success-callback
             :fail-callback wrapped-fail-callback
             :update-usage-callback (ai-usage-create-usage-statistics-callback)
             :enable-caching ai-execution--prompt-caching-enabled)))

(cl-defun ai-execution--execute-context (context success-callback &key (fail-callback nil) (model nil))
  "Execute CONTEXT using SUCCESS-CALLBACK and optional FAIL-CALLBACK with an optional MODEL."
  (ai-execution-perform-async-backend-query context success-callback :fail-callback fail-callback :model model))

(cl-defun ai-execution--execute-command (command success-callback &key (fail-callback nil) (model nil))
  "Execute COMMAND by dispatching to the appropriate backend using SUCCESS-CALLBACK.
Optionally use FAIL-CALLBACK and specify a MODEL."
  (let* ((execution-model (if model model (ai-model-management-get-current)))
         (execution-backend (map-elt execution-model :execution-backend))
         (context (ai-context-management--get-executions-context-for-command command :model execution-model)))
    (funcall execution-backend
             context
             execution-model
             :success-callback success-callback
             :fail-callback fail-callback
             :update-usage-callback (ai-usage-create-usage-statistics-callback)
             :enable-caching ai-execution--prompt-caching-enabled)))

(provide 'ai-execution)

;;; ai-execution.el ends here
