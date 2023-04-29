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



;;; Code:

(require 'ai-utils)
(require 'ai-openai)

(defvar ai--openai--chat-url "https://api.openai.com/v1/chat/completions")

(defcustom ai--openai--chat-model-version "gpt-3.5-turbo"
  "The used OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'ai-openai)


(cl-defun ai--openai--async-chat-request (messages callback &optional
                                                   (api-url ai--openai--chat-url)
                                                   (model ai--openai--chat-model-version)
                                                   (temperature ai--openai--model-temperature)
                                                   (max-tokens ai--openai--default-max-tokens)
                                                   (timeout ai--openai-request-timeout)
                                                   (n ai--openai--completion-choices)
                                                   )
  ""
  (when (null ai--openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((request-data  (encode-coding-string  (json-encode `(("model" . ,model)
                                                              ("messages" . ,messages)
                                                              ("temperature" . ,temperature)
                                                              ("max_tokens" . ,max-tokens)
                                                              ("n" . ,n)
                                                              ))
                                               'utf-8))

         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai--openai--api-key))))
         )

    (ai--openai-async-request api-url "POST" request-data headers callback timeout)
    )
  )

(cl-defun ai--openai--chat-sync-request (messages &optional
                                        (api-url ai--openai--chat-url)
                                        (model ai--openai--chat-model-version)
                                        (temperature ai--openai--model-temperature)
                                        (max-tokens ai--openai--default-max-tokens)
                                        (timeout ai--openai-request-timeout)
                                        (n ai--openai--completion-choices)
                                        )
  ""
  (when (null ai--openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((request-data (encode-coding-string (json-encode `(("model" . ,model)
                                                            ("messages" . ,messages)
                                                            ("temperature" . ,temperature)
                                                            ("max_tokens" . ,max-tokens)
                                                            ("n" . ,n)
                                                            ))
                                             'utf-8
                                             ))

         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai--openai--api-key))))
         )

    (condition-case processing-error

        (let ((response (ai--openai-request api-url "POST" request-data headers timeout)))
          (ai--openai--extract-response-or-error response)
          )
      (error (progn
               (ai--log-and-error  (format "Process chat request error: %s" (error-message-string processing-error)))
               ))
      )
    )

  )


(cl-defun ai--openai--chat-async-query-by-type (query-type query callback)
  ""

  (if-let (format-string (cdr (assoc query-type ai--openai--query-type-map)))
      (ai--openai--chat-async-send-query  `((("role" . "user") ("content" . ,(format format-string query)))) callback)
    (ai--openai--chat-async-send-query  `((("role" . "user") ("content" . ,(format "%s\n\n%s" query-type query))))  callback)))


(cl-defun ai--openai--chat-sync-send-query (input)
  ""
  (let* ((response (ai--openai--chat-sync-request input)))
    (ai--openai--chat-get-choice response)))


(cl-defun ai--openai--chat-async-send-query (input callback)
  ""
  (ai--openai--async-chat-request
   input
   (lambda (response)
     (progn
       (funcall callback (ai--openai--extract-response-or-error response))))))


(cl-defun ai--openai--chat-get-choice (response &optional (choice-id 0))
  ""
  (if (> (length (ai--openai--get-response-choices response)) 0)
      (let
          ((choice (elt (ai--openai--get-response-choices response) choice-id)))
        (cdr (assoc 'message choice))
        )))


(defun ai--region-or-buffer-query-by-type (callback)
  ""
  (interactive)

  (let* ((query-type (completing-read ai--query-type-prompt (mapcar #'car ai--openai--query-type-map))))
    (if (region-active-p)
        (ai--openai--completions-async-query-by-type query-type (ai--get-region-content) callback)
      (ai--openai--completions-async-query-by-type (ai--get-buffer-content) callback))))


(defun ai--openai--chat-explain-text (text callback)
  ""

  (ai--openai--chat-async-query-by-type
   "explain"
   text
   (lambda (success-response)
     (funcall callback (cdr (assoc 'content (ai--openai--chat-get-choice success-response))))
     )
   )
  )



(provide 'ai-openai-chatgpt)

;;; ai-openai-chatgpt.el ends here
