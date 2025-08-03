;;; ai-logging.el --- Logging utilities for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, logging

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
;; This module provides logging utilities for AI mode, including:
;; - Message logging to *Messages* buffer
;; - Multi-level verbose logging control
;; - Request/response logging
;; - Buffer-based logging utilities

;;; Code:

(require 'cl-lib)

(defcustom ai-logging-verbose-level 'info
  "Logging level for AI mode.
Possible values are:
- 'silent: No logging
- 'error: Only error messages
- 'warn: Warning and error messages
- 'info: Information, warning and error messages (default)
- 'debug: Debug, information, warning and error messages
- 'trace: All messages including detailed traces"
  :type '(choice (const :tag "Silent" silent)
                 (const :tag "Error only" error)
                 (const :tag "Warning" warn)
                 (const :tag "Info" info)
                 (const :tag "Debug" debug)
                 (const :tag "Trace" trace))
  :group 'ai-logging)

(defun ai-logging-set-level (level)
  "Set logging level to LEVEL.
LEVEL should be one of: 'silent, 'error, 'warn, 'info, 'debug, 'trace."
  (interactive (list (intern (completing-read "Logging level: "
                                             '("silent" "error" "warn" "info" "debug" "trace")
                                             nil t))))
  (setq ai-logging-verbose-level level)
  (customize-save-variable 'ai-logging-verbose-level ai-logging-verbose-level)
  (message "AI logging level set to %s" level))

(defun ai-logging--level-enabled-p (level)
  "Check if logging LEVEL is enabled based on current configuration."
  (let ((current-level ai-logging-verbose-level)
        (level-hierarchy '((silent . 0)
                          (error . 1)
                          (warn . 2)
                          (info . 3)
                          (debug . 4)
                          (trace . 5))))
    (and (not (eq current-level 'silent))
         (>= (alist-get current-level level-hierarchy 0)
             (alist-get level level-hierarchy 0)))))

(defun ai-logging--message (level component format-string &rest args)
  "Log a message with LEVEL from COMPONENT using FORMAT-STRING and ARGS if level is enabled."
  (when (ai-logging--level-enabled-p level)
    (let ((prefix (pcase level
                    ('error "[ERROR] AI")
                    ('warn "[WARN] AI")
                    ('info "[INFO] AI")
                    ('debug "[DEBUG] AI")
                    ('trace "[TRACE] AI")
                    (_ "AI"))))
      (apply 'message (concat prefix ":" component ": " format-string) args))))

(defun ai-logging--verbose-message (&rest args)
  "Output a message if logging is enabled.
ARGS are the message parts. This function is deprecated, use ai-logging--message instead."
  (when (ai-logging--level-enabled-p 'info)
    (apply 'message args)))

(defun ai-logging--truncate-content (content max-length)
  "Truncate CONTENT to MAX-LENGTH characters, adding ellipsis if needed."
  (if (> (length content) max-length)
      (concat (substring content 0 max-length) "...")
    content))

(defun ai-logging--log-request (request-id method url headers body)
  "Log an HTTP request with REQUEST-ID, METHOD, URL, HEADERS, and BODY."
  (ai-logging--message 'debug "network" "Sending %s request[%s] to '%s'" method request-id url)
  (ai-logging--log-request-headers request-id method url headers)
  (ai-logging--log-request-body request-id body))

(defun ai-logging--log-request-headers (request-id method url headers)
  "Log request headers for REQUEST-ID with METHOD, URL, and HEADERS."
  (ai-logging--message 'debug "request" "REQUEST[%s]: %s %s - Headers: %s"
                      request-id method url headers))

(defun ai-logging--log-request-body (request-id body)
  "Log request body for REQUEST-ID with BODY content."
  (when (and body (ai-logging--level-enabled-p 'trace))
    (let ((truncated-body (ai-logging--truncate-content body 500)))
      (ai-logging--message 'trace "request" "REQUEST BODY[%s]: %s" request-id truncated-body))))

(defun ai-logging--log-response (request-id response)
  "Log a RESPONSE for a HTTP request with REQUEST-ID."
  (ai-logging--message 'debug "network" "RESPONSE[%s]: %s characters received" request-id (length (format "%s" response)))
  (when (ai-logging--level-enabled-p 'trace)
    (let ((response-str (format "%s" response)))
      (ai-logging--message 'trace "request" "RESPONSE BODY[%s]: %s" request-id
                           (ai-logging--truncate-content response-str 1000)))))

(defun ai-logging--log-error (request-id error-data)
  "Log error data for REQUEST-ID with ERROR-DATA."
  (ai-logging--message 'error "network" "ERROR[%s]: %s" request-id error-data))

(defun ai-logging--log-processed-response (request-id processed-response)
  "Log processed response for REQUEST-ID with PROCESSED-RESPONSE."
  (ai-logging--message 'debug "request" "PROCESSED[%s]: response processed successfully" request-id)
  (when (and processed-response (ai-logging--level-enabled-p 'trace))
    (ai-logging--message 'trace "request" "PROCESSED CONTENT[%s]: %s" request-id
                        (if (stringp processed-response)
                            (ai-logging--truncate-content processed-response 500)
                          processed-response))))


(provide 'ai-logging)

;;; ai-logging.el ends here
