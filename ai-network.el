;;; ai-network.el --- Network communication and connectivity for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, network

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
;; This module manages network communication and connectivity for AI mode,
;; including HTTP request handling, connection pooling, retry logic, timeout
;; management, and network-related error handling for AI service interactions.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-telemetry)

(defvar url-http-end-of-headers)

(defcustom ai-network--default-request-timeout 60
  "Default timeout for HTTP requests in seconds."
  :type '(choice integer (const nil))
  :group 'ai-mode)

(cl-defun ai-network--async-request (api-url method body headers callback
                                             &key (timeout ai-network--default-request-timeout)
                                             (request-id (ai-common--generate-request-id)))
  "Send an asynchronous HTTP request to API-URL.

METHOD specifies the HTTP method.
BODY is the request body.
HEADERS is a list of request headers.
CALLBACK is a function called with the response.
TIMEOUT specifies the request timeout.
REQUEST-ID is an optional unique identifier for the request."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data body))
    (ai-telemetry--log-request request-id url-request-method api-url headers body)
    (url-retrieve api-url
                  (lambda (_events)
                    (progn
                      (ai-telemetry--log-response request-id (buffer-string))
                      (goto-char url-http-end-of-headers)
                      (condition-case-unless-debug request-error
                          (let ((result (json-read-from-string
                                         (decode-coding-string
                                          (buffer-substring-no-properties url-http-end-of-headers (point-max))
                                          'utf-8))))
                            (funcall callback result))
                        (error (ai-telemetry--log-and-error (format "Error while parsing response body: %s" (error-message-string request-error)))))))
                  nil nil timeout)))

(cl-defun ai-network--sync-request (api-url method body headers
                                            &key (timeout ai-network--default-request-timeout)
                                            (request-id (ai-common--generate-request-id)))
  "Send a synchronous HTTP request to API-URL and return the response content.

METHOD specifies the HTTP method.
BODY is the request body.
HEADERS is a list of request headers.
TIMEOUT specifies the request timeout.
REQUEST-ID is an optional unique identifier for the request."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data body)
        (buffer (url-retrieve-synchronously api-url 'silent nil timeout)))
    (ai-telemetry--log-request request-id url-request-method api-url headers body)
    (if buffer
        (with-current-buffer buffer
          (ai-telemetry--log-response request-id (buffer-string))
          (goto-char url-http-end-of-headers)
          (condition-case-unless-debug _request-error
              (json-read-from-string
               (decode-coding-string
                (buffer-substring-no-properties url-http-end-of-headers (point-max))
                'utf-8))
            (error (ai-telemetry--log-and-error "Error while parsing response body"))))
      (ai-telemetry--log-and-error (format "Failed to send request %s to %s" request-id api-url)))))

(provide 'ai-network)

;;; ai-network.el ends here
