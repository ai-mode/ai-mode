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
;; - Verbose logging control
;; - Request/response logging
;; - Buffer-based logging utilities

;;; Code:

(require 'cl-lib)

(defcustom ai-logging-verbose-log nil
  "If non-nil, enables verbose logging."
  :type 'boolean
  :group 'ai-logging)

(defun ai-logging-toggle-verbose-log ()
  "Toggle verbose logging on/off."
  (interactive)
  (setq ai-logging-verbose-log (not ai-logging-verbose-log))
  (customize-save-variable 'ai-logging-verbose-log ai-logging-verbose-log)
  (message "AI logging verbose log %s" (if ai-logging-verbose-log "enabled" "disabled")))

(defun ai-logging--verbose-message (&rest args)
  "Output a message if `ai-logging-verbose-log' is non-nil.
ARGS are the message parts."
  (when ai-logging-verbose-log
    (apply 'message args)))

(defun ai-logging--log-request (request-id method url headers body)
  "Log an HTTP request with REQUEST-ID, METHOD, URL, HEADERS, and BODY."
  (message "Sending %s request[%s] to '%s'" method request-id url)
  (ai-logging--verbose-message "AI REQUEST[%s]: %s %s - Headers: %s" request-id method url headers))

(defun ai-logging--log-response (request-id response)
  "Log a RESPONSE for a HTTP request with REQUEST-ID."
  (ai-logging--verbose-message "AI RESPONSE[%s]: %s characters received" request-id (length (format "%s" response))))

(defun ai-logging--log-error (request-id error-data)
  "Log error data for REQUEST-ID with ERROR-DATA."
  (ai-logging--verbose-message "AI ERROR[%s]: %s" request-id error-data))

(defun ai-logging--log-processed-response (request-id processed-response)
  "Log processed response for REQUEST-ID with PROCESSED-RESPONSE."
  (ai-logging--verbose-message "AI PROCESSED[%s]: response processed successfully" request-id))

(provide 'ai-logging)

;;; ai-logging.el ends here
