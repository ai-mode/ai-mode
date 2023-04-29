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


(defcustom ai--openai--human-lang "english"
  ""
  :type 'string
  :group 'ai-openai
  )


(defcustom ai--openai-request-timeout 60
  ""
  :type '(choice integer (const nil))
  :group 'ai-openai)


(defcustom ai--openai--query-type-map
  '(("spellcheck" . "Spellcheck this text: %s")
    ("elaborate" . "Elaborate on this text: %s")
    ("explain" . "Explain the following: %s")
    ("document" . "Please add the documentation for the following code: %s")
    ("fix" . "Here is a bug in the following function, please help me fix it: %s")
    ("improve" . "Improve and extend the following code: %s")
    ("optimize" . "Optimize the following code: %s")
    ("refactor" . "Refactor the following code: %s")
    )
  "An association list that maps query types to their corresponding format strings."
  :type '(alist :key-type (string :tag "Query Type")
                :value-type (string :tag "Format String"))
  :group 'ai-openai)


(cl-defun ai--openai-async-request (api-url method body headers callback &optional (timeout ai--openai-request-timeout))
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
                  nil nil timeout)
    )
  )


(cl-defun ai--openai--sync-request (api-url method body headers &optional (timeout ai--openai-request-timeout))
  ""

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
              (decode-coding-string (json-read) 'utf-8)
            (error (progn
                     (ai--log-and-error  "Error while parsing response body")
                     )))
          )
      (ai--log-and-error (format "Failed to send request %s to %s" request-id api-url))
      )))




(defun ai--openai--get-response-choices (response)
  ""
  (cdr (assoc 'choices response))
  )


(cl-defun ai--openai--extract-response-or-error (response)
  ""
  (if (assoc 'error response)
      (error (cdr (assoc 'message (cdr (assoc 'error response)))))
    response))

(cl-defun ai--openai--extract-error-messages (response)
  ""
  (if (assoc 'error response)
      (cdr (assoc 'message (cdr (assoc 'error response))))
    "unknown"))

(defun ai--openai--query-type-format-string (query-type)
  ""
  (cdr (assoc query-type ai--openai--query-type-map)))


(provide 'ai-openai)

;;; ai-openai.el ends here
