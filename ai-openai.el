;;; ai-openai.el --- Integration with OpenAI API -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai, chatgpt, gpt


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


;;; Code:

(require 'ai-utils)

(defgroup ai-openai nil
  "Integration with OpenAI API."
  :prefix "ai-mode"
  :group 'ai-mode
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ai-mode"))

(defcustom ai--openai--model-temperature 0.7
  ""

  :type '(choice integer (const nil))
  :group 'ai-openai
  )

(defcustom ai--openai--completion-choices 1
  ""

  :type '(choice integer (const nil))
  :group 'ai-openai
  )

(defcustom ai--openai--default-max-tokens 1000
  ""
  :type '(choice integer (const nil))
  :group 'ai-openai
  )

(defcustom ai--openai--api-key ""
  "Key for OpenAI API."
  :type 'string
  :group 'ai-openai
  )


(defcustom ai--openai-request-timeout 60
  ""
  :type '(choice integer (const nil))
  :group 'ai-openai)



(cl-defun ai--openai-async-request (api-url method body headers callback &key (timeout ai--openai-request-timeout))
  ""
  (let* (

         (request-id (ai--get-random-uuid))
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data body))

    (ai--log-request request-id url-request-method api-url headers body)

    (url-retrieve api-url
                  (lambda (events)
                    (progn
                      (ai--log-response request-id (buffer-string))
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
                              (funcall callback result)
                              ))
                        (error (progn
                                 (ai--log-and-error (format "Error while parsing response body: %s" (error-message-string request-error)))
                                 )))
                      ))
                  nil nil timeout)))


(cl-defun ai--openai--sync-request (api-url method body headers &key (timeout ai--openai-request-timeout))
  "Performing a synchronous request to the OpenAI API.
Return response content or raise an error.

API-URL is a full API address.
METHOD is a request method.
BODY is request body content.
HEADERS is a list of headers.
"
  (let* (
         (request-id (ai--get-random-uuid))
         (url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data body)
         (buffer (url-retrieve-synchronously api-url 'silent nil timeout))
         response)

    (ai--log-request request-id url-request-method api-url headers body)

    (if buffer
        (with-current-buffer buffer
          (ai--log-response request-id (buffer-string))
          (goto-char url-http-end-of-headers)

          (condition-case request-error
              (let ((result (json-read-from-string
                             (decode-coding-string
                              (buffer-substring-no-properties url-http-end-of-headers (point-max))
                              'utf-8))))
                result)
            (error (progn
                     (ai--log-and-error  "Error while parsing response body")))))
      (ai--log-and-error (format "Failed to send request %s to %s" request-id api-url)))))



(defun ai--openai--get-response-choices (response)
  "Extract choices list from RESPONSE."
  (cdr (assoc 'choices response)))


(cl-defun ai--openai--extract-response-or-error (response)
  "Extract success response from RESPONSE or raise error."
  (if (assoc 'error response)
      (error (cdr (assoc 'message (cdr (assoc 'error response)))))
    response))

(cl-defun ai--openai--extract-error-messages (response)
  "Extract error message from RESPONSE."
  (if (assoc 'error response)
      (cdr (assoc 'message (cdr (assoc 'error response))))
    "unknown"))


(provide 'ai-openai)

;;; ai-openai.el ends here
