;;; ai-openai-chat.el --- Integration with OpenAI API -*- lexical-binding: t -*-

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
;; The module includes functions for creating requests
;; to the OpenAI ChatGPT API and processing responses.


;;; Code:

(require 'ai-utils)
(require 'ai-openai)

(defvar ai-openai-chat--url "https://api.openai.com/v1/chat/completions")

(defcustom ai-openai-chat--model-version "gpt-3.5-turbo"
  "The used OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'ai-openai)


(cl-defun ai-openai-chat--async-request (messages callback &key
                                                   (api-url ai-openai-chat--url)
                                                   (model ai-openai-chat--model-version)
                                                   (temperature ai-openai--model-temperature)
                                                   (max-tokens ai-openai--default-max-tokens)
                                                   (timeout ai-openai-request-timeout)
                                                   (n ai-openai--completion-choices)
                                                   (extra-params nil))
  "Asynchronous MESSAGES execution of a request to the OpenAI ChatGPT API.

In case of a successful request execution, a CALLBACK function is called.

API-URL - is a full API address.
MODEL - an AI model that needs to be used to process the request.
TEMPERATURE - sampling temperature to use, between 0 and 2.
MAX-TOKENS - The maximum number of tokens to generate answer.
TIMEOUT - the duration limit for the execution of the request.
N - How many completions to generate for each prompt.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."
  (when (null ai-openai--api-key)
    (error "OpenAI API key is not set"))


  (let* ((request-data `(("model" . ,model)
                         ("messages" . ,messages)
                         ("temperature" . ,temperature)
                         ("max_tokens" . ,max-tokens)
                         ("n" . ,n)))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))

         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai-openai--api-key)))))
    (ai-openai-async-request api-url "POST" encoded-request-data headers callback :timeout timeout)))

(cl-defun ai-openai-chat--sync-request (messages &key
                                                  (api-url ai-openai-chat--url)
                                                  (model ai-openai-chat--model-version)
                                                  (temperature ai-openai--model-temperature)
                                                  (max-tokens ai-openai--default-max-tokens)
                                                  (timeout ai-openai-request-timeout)
                                                  (n ai-openai--completion-choices)
                                                  (extra-params nil))
  "Synchronous MESSAGES execution of a request to the OpenAI ChatGPT API.

API-URL - is a full API address.
MODEL - an AI model that needs to be used to process the request.
TEMPERATURE - sampling temperature to use, between 0 and 2.
MAX-TOKENS - The maximum number of tokens to generate answer.
TIMEOUT - the duration limit for the execution of the request.
N - How many completions to generate for each prompt.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."
  (when (null ai-openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((request-data `(("model" . ,model)
                         ("messages" . ,messages)
                         ("temperature" . ,temperature)
                         ("max_tokens" . ,max-tokens)
                         ("n" . ,n)))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))

         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai-openai--api-key)))))
    (condition-case-unless-debug processing-error
        (let ((response (ai-openai--sync-request api-url "POST" encoded-request-data headers :timeout timeout)))
          (ai-openai--extract-response-or-error response))
      (error (progn
               (ai-utils--log-and-error  (format "Process chat request error: %s" (error-message-string processing-error))))))))


(cl-defun ai-openai-chat--async-send-query (input callback &key
                                                   (api-url ai-openai-chat--url)
                                                   (model ai-openai-chat--model-version)
                                                   (temperature ai-openai--model-temperature)
                                                   (max-tokens ai-openai--default-max-tokens)
                                                   (timeout ai-openai-request-timeout)
                                                   (n ai-openai--completion-choices)
                                                   (extra-params nil))
  "Async execute INPUT, exract message from response and call CALLBACK.

API-URL - is a full API address.
MODEL - an AI model that needs to be used to process the request.
TEMPERATURE - sampling temperature to use, between 0 and 2.
MAX-TOKENS - The maximum number of tokens to generate answer.
TIMEOUT - the duration limit for the execution of the request.
N - How many completions to generate for each prompt.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."
  (ai-openai-chat--async-request
   `((("role" . "user") ("content" . ,input)))
   (lambda (response)
     (progn
       (funcall callback (cdr (assoc 'content (ai-openai-chat--get-choice (ai-openai--extract-response-or-error response))))))))
  :api-url api-url
  :model model
  :temperature temperature
  :max-tokens max-tokens
  :timeout timeout
  :n n
  :extra-params extra-params)


(cl-defun ai-openai-chat--async-send-context (input callback &key
                                                     (api-url ai-openai-chat--url)
                                                     (model ai-openai-chat--model-version)
                                                     (temperature ai-openai--model-temperature)
                                                     (max-tokens ai-openai--default-max-tokens)
                                                     (timeout ai-openai-request-timeout)
                                                     (n ai-openai--completion-choices)
                                                     (extra-params nil))
  "Async execute INPUT, exract message from response and call CALLBACK.

API-URL - is a full API address.
MODEL - an AI model that needs to be used to process the request.
TEMPERATURE - sampling temperature to use, between 0 and 2.
MAX-TOKENS - The maximum number of tokens to generate answer.
TIMEOUT - the duration limit for the execution of the request.
N - How many completions to generate for each prompt.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters"
  (ai-openai-chat--async-request
   (ai-openai-chat--internal-to-messages input)
   (lambda (response)
     (let* ((response-content (ai-openai--extract-response-or-error response))
            (choices (ai-openai--get-response-choices response-content))
            (messages (ai-openai-chat--messages-to-internal-context choices)))
       (funcall callback messages))))
  :api-url api-url
  :model model
  :temperature temperature
  :max-tokens max-tokens
  :timeout timeout
  :n n
  :extra-params extra-params)


(cl-defun ai-openai-chat--sync-send-query (input &key
                                                  (api-url ai-openai-chat--url)
                                                  (model ai-openai-chat--model-version)
                                                  (temperature ai-openai--model-temperature)
                                                  (max-tokens ai-openai--default-max-tokens)
                                                  (timeout ai-openai-request-timeout)
                                                  (n ai-openai--completion-choices)
                                                  (extra-params nil))
  "Sync execute INPUT, exract message from response and return.

API-URL - is a full API address.
MODEL - an AI model that needs to be used to process the request.
TEMPERATURE - sampling temperature to use, between 0 and 2.
MAX-TOKENS - The maximum number of tokens to generate answer.
TIMEOUT - the duration limit for the execution of the request.
N - How many completions to generate for each prompt.
EXTRA-PARAMS is a list of properties (plist) that can be used to store parameters."
  (let* ((messages `((("role" . "user") ("content" . ,input))))
         (success-response (ai-openai-chat--sync-request messages :api-url api-url :model model :temperature temperature :max-tokens max-tokens :timeout timeout :n n :extra-params extra-params))
         (content (cdr (assoc 'content (ai-openai-chat--get-choice success-response)))))
    content))


(defun ai-openai-chat--internal-to-messages (context)
  "Convert common CONTEXT structure into OpenAI Chat API messages."
  (mapcar (lambda (message) message) context))


(defun ai-openai-chat--messages-to-internal-context (messages)
  "Convert MESSAGES into internal representation."
  (mapcar (lambda (item)
            (let* ((message (cdr (assoc 'message item))))
              `(("role" . ,(cdr (assoc 'role message)))
                ("content" . ,(cdr (assoc 'content message))))))
          messages))


(cl-defun ai-openai-chat--get-choice (response &optional (choice-id 0))
  "Extract message from RESPONSE by CHOICE-ID."
  (if (> (length (ai-openai--get-response-choices response)) 0)
      (let
          ((choice (elt (ai-openai--get-response-choices response) choice-id)))
        (cdr (assoc 'message choice)))))


(provide 'ai-openai-chat)

;;; ai-openai-chat.el ends here
