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
(require 'url)


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
