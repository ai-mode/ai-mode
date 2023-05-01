;;; ai-openai-completions.el --- Integration with completions OpenAI API -*- lexical-binding: t -*-

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
(require 'ai-openai)
(require 'map)

(defcustom ai--openai--completions-model-version "text-davinci-003"
  "The used OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'ai-openai)


(defvar ai--openai--completions-url "https://api.openai.com/v1/completions")


(cl-defun ai--openai--completions-async-request (input callback &key
                                                       (api-url ai--openai--completions-url)
                                                       (model ai--openai--completions-model-version)
                                                       (temperature ai--openai--model-temperature)
                                                       (max-tokens ai--openai--default-max-tokens)
                                                       (timeout ai--openai-request-timeout)
                                                       (extra-params nil))
  ""
  (when (null ai--openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((request-data `(("model" . ,model)
                         ("prompt" . ,input)
                         ("temperature" . ,temperature)
                         ("max_tokens" . ,max-tokens)))
         (encoded-request-data (encode-coding-string (json-encode request-data) 'utf-8))
         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai--openai--api-key)))))
    (ai--openai-async-request api-url "POST" encoded-request-data headers callback :timeout timeout)))


(cl-defun ai--openai--completions-sync-request (input &key
                                                      (api-url ai--openai--completions-url)
                                                      (model ai--openai--completions-model-version)
                                                      (temperature ai--openai--model-temperature)
                                                      (max-tokens ai--openai--default-max-tokens)
                                                      (timeout ai--openai-request-timeout)
                                                      (extra-params nil))
  ""
  (when (null ai--openai--api-key)
    (error "OpenAI API key is not set"))

  (let* ((request-data `(("model" . ,model)
                         ("prompt" . ,input)
                         ("temperature" . ,temperature)
                         ("max_tokens" . ,max-tokens)))
         (encoded-request-data (encode-coding-string (json-encode request-data)
                                                     'utf-8))

         (headers  `(("Content-Type" . "application/json")
                     ("Authorization" . ,(format "Bearer %s" ai--openai--api-key)))))
    (condition-case-unless-debug processing-error
        (let ((response (ai--openai--sync-request api-url "POST" encoded-request-data headers :timeout timeout)))
          (ai--openai--extract-response-or-error response))
      (error  (progn
                (ai--log-and-error  (format "Process chat request error: %s" (error-message-string processing-error))))))))


(cl-defun ai--openai--completions-async-send-query (input callback &key
                                                          (api-url ai--openai--completions-url)
                                                          (model ai--openai--completions-model-version)
                                                          (temperature ai--openai--model-temperature)
                                                          (max-tokens ai--openai--default-max-tokens)
                                                          (timeout ai--openai-request-timeout)
                                                          (extra-params nil))
  ""
  (ai--openai--completions-async-request
   input
   (lambda (response)
     (let* ((success-response (ai--openai--extract-response-or-error response))
            (choice  (ai--openai--completions-get-choice success-response)))
       (funcall callback choice)))
   :api-url api-url
   :model model
   :temperature temperature
   :max-tokens max-tokens
   :timeout timeout
   :extra-params extra-params))


(cl-defun ai--openai--completions-sync-send-query (input &key
                                                         (api-url ai--openai--completions-url)
                                                         (model ai--openai--completions-model-version)
                                                         (temperature ai--openai--model-temperature)
                                                         (max-tokens ai--openai--default-max-tokens)
                                                         (timeout ai--openai-request-timeout)
                                                         (extra-params nil))
  ""
  (let* ((message input)
         (success-response (ai--openai--completions-sync-request input :api-url api-url :model model :temperature temperature :max-tokens max-tokens :timeout timeout :extra-params extra-params))
         (content (ai--openai--completions-get-choice success-response)))
    content))


(cl-defun ai--openai--completions-get-choice (response &optional (choice-id 0))
  ""

  (if (> (length (ai--openai--get-response-choices response)) 0)
      (let* ((choice (elt (ai--openai--get-response-choices response) choice-id)))
        (cdr (assoc 'text choice)))))


(defun ai--openai--completions-completion-backend (code callback)
  ""
  (ai--openai--completions-async-request
   code
   (lambda (response)
     (let* ((success-response (ai--openai--extract-response-or-error response))
            (choices  (ai--openai--get-response-choices success-response))
            (candidates (ai--openai--completion-choices-to-candidates choices)))
       (funcall callback candidates)))))


(defun ai--openai--completion-choices-to-candidates (choices)
  "Convert completion result CHOICES into internal candidates."
  (mapcar (lambda (item) (cdr (assoc 'text item))) choices))



(provide 'ai-openai-completions)

;;; ai-openai-completions.el ends here
