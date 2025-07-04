;;; ai-telemetry.el --- Telemetry and usage statistics for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, telemetry

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
;; This module handles the collection and reporting of telemetry data
;; for usage statistics and performance monitoring in AI mode.
;; Features include:
;; - Request logging and monitoring
;; - Prompt and context debugging

;;; Code:

(require 'cl-lib)
(require 'ai-usage)

(defcustom ai-telemetry-enabled t
  "Enable telemetry collection for AI mode.
When enabled, collects usage statistics and performance metrics."
  :type 'boolean
  :group 'ai-mode)

(defcustom ai-telemetry-write-log-buffer nil
  "If non-nil, enables logging to a special buffer."
  :type 'boolean
  :group 'ai-telemetry)

(defcustom ai-telemetry-verbose-log nil
  "If non-nil, enables verbose logging."
  :type 'boolean
  :group 'ai-telemetry)

(defcustom ai-telemetry-write-to-prompt-buffer nil
  "If non-nil, writes to the prompt buffer."
  :type 'boolean
  :group 'ai-telemetry)

(defvar ai-telemetry--log-buffer-name "*AI-request-log*"
  "Name of the buffer used for logging API requests.")

(defvar ai-telemetry--prompt-buffer-name "*AI prompt*"
  "Name of the buffer used for storing prompts to the language model.")

(defun ai-telemetry-create-usage-statistics-callback ()
  "Create a callback function for displaying usage statistics."
  (ai-usage-create-usage-statistics-callback))

(defun ai-telemetry-show-session-stats ()
  "Display session telemetry statistics."
  (interactive)
  (ai-usage-show-session-stats))

(defun ai-telemetry-reset-session-stats ()
  "Reset session telemetry statistics."
  (interactive)
  (ai-usage-reset-session-stats))

(defun ai-telemetry-toggle ()
  "Toggle telemetry collection on/off."
  (interactive)
  (setq ai-telemetry-enabled (not ai-telemetry-enabled))
  (customize-save-variable 'ai-telemetry-enabled ai-telemetry-enabled)
  (message "AI telemetry %s" (if ai-telemetry-enabled "enabled" "disabled")))

(defun ai-telemetry-toggle-log-buffer ()
  "Toggle logging to buffer on/off."
  (interactive)
  (setq ai-telemetry-write-log-buffer (not ai-telemetry-write-log-buffer))
  (customize-save-variable 'ai-telemetry-write-log-buffer ai-telemetry-write-log-buffer)
  (message "AI telemetry log buffer %s" (if ai-telemetry-write-log-buffer "enabled" "disabled")))

(defun ai-telemetry-toggle-verbose-log ()
  "Toggle verbose logging on/off."
  (interactive)
  (setq ai-telemetry-verbose-log (not ai-telemetry-verbose-log))
  (customize-save-variable 'ai-telemetry-verbose-log ai-telemetry-verbose-log)
  (message "AI telemetry verbose log %s" (if ai-telemetry-verbose-log "enabled" "disabled")))

(defun ai-telemetry-toggle-prompt-buffer ()
  "Toggle writing to prompt buffer on/off."
  (interactive)
  (setq ai-telemetry-write-to-prompt-buffer (not ai-telemetry-write-to-prompt-buffer))
  (customize-save-variable 'ai-telemetry-write-to-prompt-buffer ai-telemetry-write-to-prompt-buffer)
  (message "AI telemetry prompt buffer %s" (if ai-telemetry-write-to-prompt-buffer "enabled" "disabled")))

(defun ai-telemetry-get-session-stats ()
  "Return current session telemetry statistics."
  (ai-usage-get-session-stats))

;; Logging functions

(defun ai-telemetry--write-log (output)
  "Write OUTPUT to the log buffer.

Logs OUTPUT into `ai-telemetry--log-buffer-name' if `ai-telemetry-write-log-buffer' is enabled."
  (when ai-telemetry-write-log-buffer
    (ai-telemetry--write-output-to-log-buffer output)
    (ai-telemetry--write-output-to-log-buffer "\n")))

(defun ai-telemetry--log-and-error (log-message)
  "Log LOG-MESSAGE and raise an error."
  (ai-telemetry--write-log log-message)
  (ai-telemetry--write-log "\n")
  (error (format "%s\n" log-message)))

(defun ai-telemetry--log-request (request-id method url headers body)
  "Log an HTTP request with REQUEST-ID, METHOD, URL, HEADERS, and BODY."
  (message "Sending %s request[%s] to '%s'" method request-id url)
  (ai-telemetry--write-log (format "REQUEST[%s]: %s %s\n" request-id method url))
  (ai-telemetry--write-log (format "HEADERS[%s]\n" headers))
  ;; TODO: write headers
  (ai-telemetry--write-log (format "%s\n" body)))

(defun ai-telemetry--log-response (request-id response)
  "Log a RESPONSE for a HTTP request with REQUEST-ID."
  (ai-telemetry--write-log (format "RESPONSE[%s]:\n" request-id))
  (ai-telemetry--write-log (format "%s\n\n" response)))

(defun ai-telemetry--write-output-to-log-buffer (output)
  "Write OUTPUT to the `ai-telemetry--log-buffer-name' buffer."
  (let ((buffer (get-buffer ai-telemetry--log-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create ai-telemetry--log-buffer-name)))
    (with-current-buffer buffer
      (set-buffer-file-coding-system 'utf-8-unix) ;; Setup buffer encoding
      (goto-char (point-max))
      (insert output)
      (goto-char (point-max)))))

(defun ai-telemetry--clear-log-buffer ()
  "Clear the contents of the `ai-telemetry--log-buffer-name' buffer."
  (interactive)
  (let ((buffer (get-buffer ai-telemetry--log-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

(defun ai-telemetry--verbose-message (&rest args)
  "Output a message if `ai-telemetry-verbose-log' is non-nil.
ARGS are the message parts."
  (when ai-telemetry-verbose-log
    (apply 'message args)))

;; Prompt and context debugging functions

(defun ai-telemetry-write-context-to-prompt-buffer (messages)
  "Extract 'context' from MESSAGES and write it to the prompt buffer if enabled."
  (when ai-telemetry-write-to-prompt-buffer
    (let ((buffer (get-buffer-create ai-telemetry--prompt-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)   ; Clear the buffer before writing new content
        (dolist (message messages)
          (let ((content (when (fboundp 'ai-common--render-struct-to-string)
                          (ai-common--render-struct-to-string message))))
            (when content
              (insert content "\n"))))))))

(defun ai-telemetry-write-context-to-prompt-buffer-debug (messages)
  "Extract 'context' from MESSAGES and display it in the debug buffer without switching focus.
This is an alternative to `ai-telemetry-write-context-to-prompt-buffer` that uses
`ai-debug-show-context-debug` for richer display, but does not switch the
current window to the debug buffer.
Requires `ai-debug` to be loaded."
  (when (and ai-telemetry-write-to-prompt-buffer (fboundp 'ai-debug-show-context-debug))
    (let ((debug-context (plist-put '() :messages messages)))
      (save-selected-window
        ;; Ensure ai-debug is loaded before calling its function
        (require 'ai-debug nil 'noerror)
        (when (fboundp 'ai-debug-show-context-debug)
          (ai-debug-show-context-debug debug-context))))))

(provide 'ai-telemetry)

;;; ai-telemetry.el ends here
