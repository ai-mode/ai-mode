;;; ai-openai.el --- Integration with OpenAI API -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: help, tools


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
;; The module includes common functions for working with the OpenAI API.

;;; Code:


(require 'ai-utils)


(defgroup ai-openai nil
  "Integration with OpenAI API."
  :prefix "ai-mode"
  :group 'ai-mode
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defcustom ai-openai--model-temperature 0.7
  "What sampling temperature to use, between 0 and 2."
  :type '(choice integer (const nil))
  :group 'ai-openai)

(defcustom ai-openai--completion-choices 1
  "How many completions to generate for each prompt."
  :type '(choice integer (const nil))
  :group 'ai-openai)

(defcustom ai-openai--default-max-tokens 1000
  "The maximum number of tokens to generate in the completion."
  :type '(choice integer (const nil))
  :group 'ai-openai)

(defcustom ai-openai--api-key ""
  "Key for OpenAI API."
  :type 'string
  :group 'ai-openai)


(defcustom ai-openai-request-timeout 60
  "OpenAI request timeout."
  :type '(choice integer (const nil))
  :group 'ai-openai)



(cl-defun ai-openai-async-request (api-url method body headers callback &key (timeout ai-openai-request-timeout))
  "Prepare and execute async request to API-URL.

METHOD is HTTP method.
BODY is request body.
HEADERS is request headers.
CALLBACK is function called upon successful response.
TIMEOUT is timeout for request execution."
  (let* (

         (request-id (ai-utils--get-random-uuid))
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data body))

    (ai-utils--log-request request-id url-request-method api-url headers body)

    (url-retrieve api-url
                  (lambda (events)
                    (progn
                      (ai-utils--log-response request-id (buffer-string))
                      (goto-char url-http-end-of-headers)

                      (condition-case request-error
                          (let (
                                ;; (json-object-type 'plist)
                                ;; (json-key-type 'symbol)
                                ;; (json-array-type 'vector)
                                )
                            (let ((result (json-read-from-string
                                           (decode-coding-string
                                            (buffer-substring-no-properties url-http-end-of-headers (point-max))
                                            'utf-8))))
                              (funcall callback result)))
                        (error (progn
                                 (ai-utils--log-and-error (format "Error while parsing response body: %s" (error-message-string request-error))))))))
                  nil nil timeout)))


(cl-defun ai-openai--sync-request (api-url method body headers &key (timeout ai-openai-request-timeout))
  "Performing a synchronous request to the OpenAI API.
Return response content or raise an error.

API-URL is a full API address.
METHOD is a request method.
BODY is request body content.
HEADERS is a list of headers.
TIMEOUT is timeout for request execution."
  (let* (
         (request-id (ai-utils--get-random-uuid))
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data body)
         (buffer (url-retrieve-synchronously api-url 'silent nil timeout))
         response)

    (ai-utils--log-request request-id url-request-method api-url headers body)

    (if buffer
        (with-current-buffer buffer
          (ai-utils--log-response request-id (buffer-string))
          (goto-char url-http-end-of-headers)

          (condition-case request-error
              (let ((result (json-read-from-string
                             (decode-coding-string
                              (buffer-substring-no-properties url-http-end-of-headers (point-max))
                              'utf-8))))
                result)
            (error (progn
                     (ai-utils--log-and-error  "Error while parsing response body")))))
      (ai-utils--log-and-error (format "Failed to send request %s to %s" request-id api-url)))))



(defun ai-openai--get-response-choices (response)
  "Extract choices list from RESPONSE."
  (cdr (assoc 'choices response)))


(cl-defun ai-openai--extract-response-or-error (response)
  "Extract success response from RESPONSE or raise error."
  (if (assoc 'error response)
      (error (cdr (assoc 'message (cdr (assoc 'error response)))))
    response))

(cl-defun ai-openai--extract-error-messages (response)
  "Extract error message from RESPONSE."
  (if (assoc 'error response)
      (cdr (assoc 'message (cdr (assoc 'error response))))
    "unknown"))


(provide 'ai-openai)

;;; ai-openai.el ends here
