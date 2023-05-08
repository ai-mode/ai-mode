;;; ai-utils.el --- ai-mode utils -*- lexical-binding: t -*-

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
;; Helper functions for AI mode.


;;; Code:

(require 'url)

(defcustom ai-utils--write-log-buffer nil
  "Switch to turn on/off logging to a special buffer."
  :type 'boolean
  :group 'ai)

(defvar url-http-end-of-headers)

(defvar ai-utils--explanation-buffer-name "*AI explanation*"
  "The name of the buffer for explanation.")

(defvar ai-utils--response-buffer-name "*AI response*"
  "Tha name of the buffer for AI response.")

(defvar ai-utils--log-buffer-name "*AI-request-log*"
  "The name of the buffer for logging API requests.")

(defcustom ai-utils--default-request-timeout 60
  "HTTP request timeout."
  :type '(choice integer (const nil))
  :group 'ai-mode)


(defun ai-utils--write-log (output)
  "Write OUTPUT into `ai-utils--log-buffer-name`.

If `ai-utils--write-log-buffer` is enabled."
  (when ai-utils--write-log-buffer
    (ai-utils--write-output-to-log-buffer output)
    (ai-utils--write-output-to-log-buffer "\n")))


(defun ai-utils--log-and-error (log-message)
  "Write LOG-MESSAGE into `ai-utils--log-buffer-name` and raise an error."
  (ai-utils--write-log log-message)
  (ai-utils--write-log "\n")
  (error log-message))


(defun ai-utils--log-request (request-id method url headers body)
  "Write log request into *Messages* buffer.

REQUEST-ID - request id
METHOD - request method
URL - requested url
HEADERS - request headers
BODY - request body"
  (message "Sending %s request[%s] to '%s" method request-id url)
  (ai-utils--write-log (format "REQUEST[%s]: %s %s\n" request-id method url))
  (ai-utils--write-log (format "HEADERS[%s]\n" headers))
  ;; TODO: write headers
  (ai-utils--write-log (format "%s\n" body)))


(defun ai-utils--log-response (request-id response)
  "Write RESPONSE and REQUEST-ID into *Messages* buffer."
  (ai-utils--write-log (format "RESPONSE[%s]:\n" request-id))
  (ai-utils--write-log (format "%s\n\n" response)))


(defun ai-utils--write-output-to-log-buffer (output)
  "Write OUTPUT into `ai-utils--log-buffer-name' buffer."
  (let ((buffer (get-buffer ai-utils--log-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create ai-utils--log-buffer-name)))
    (with-current-buffer buffer
      (progn
        (insert output)))))


(defun ai-utils--get-query-content ()
  "Return region content or full buffer."
  (if (region-active-p)
      (ai-utils--get-region-content)
    (ai-utils--get-buffer-content)))


(defun ai-utils--get-region-content ()
  "Return region content without properties."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun ai-utils--get-buffer-content ()
  "Return buffer content without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ai-utils--get-buffer-content-before-point ()
  "Return buffer content from beginning to current point."
  (buffer-substring-no-properties (point-min) (point)))

(defun ai-utils--insert-after-region (text)
  "Insert the given TEXT after the region."
  (when (region-active-p)
    (goto-char (region-end))
    (insert text)))

(defun ai-utils--insert-at-point (text)
  "Insert the given TEXT after the region."
  (goto-char (point))
  (insert text))

(defun ai-utils--insert-completion (text)
  "Insert TEXT after selected region or at cursor position."
  (if (region-active-p)
      (ai-utils--insert-after-region text)
    (ai-utils--insert-at-point text)))


(cl-defun ai-utils--replace-or-insert (text &optional beginning end)
  "Replace the selected region with TEXT or the area between BEGINNING and END."
  (cond
   ((region-active-p)
    (delete-region (region-beginning) (region-end))
    (insert text))
   ((and beginning end)
    (delete-region beginning end)
    (insert text))
   (t (insert text))))


(defun ai-utils--replace-tag-in-region (tag text)
  "Replace TAG in the selected region with TEXT."
  (when (region-active-p)
    (let ((region-content (ai-utils--get-region-content)))
      (delete-region (region-beginning) (region-end))
      (insert (replace-regexp-in-string tag text region-content)))))


(cl-defun ai-utils--with-current-buffer-callback (callback &optional (trim t))
  "Create a CALLBACK function that will use the context of the current buffer.

The TRIM parameter allows trimming the content passed to the CALLBACK function."
  (let ((buffer (current-buffer)))
    (lambda (content)
      (with-current-buffer buffer
        (if trim
            (funcall callback (string-trim-left content))
          (funcall callback content))))))


(cl-defun ai-utils--replace-region-or-insert-in-current-buffer (&optional (trim t))
  "Create scoped `ai-replace-or-insert` with the context of the current buffer.

The TRIM parameter allows trimming the content
 passed to the `ai-replace-or-insert`."
  (let ((buffer (current-buffer))
        (beginning (if (region-active-p) (region-beginning) (point)))
        (end (if (region-active-p) (region-end) (point))))
    (lambda (content)
      (with-current-buffer buffer
        (if trim
            (funcall 'ai-utils--replace-or-insert (string-trim-left content) beginning end)
          (funcall 'ai-utils--replace-or-insert content beginning end))))))


(cl-defun ai-utils--with-current-buffer-tagged-callback (callback tag &optional (trim t))
  "Create a CALLBACK function that will use the context of the current buffer.

TAG - a marker for placing the content passed to the CALLBACK function.

The TRIM parameter allows trimming the content passed to the CALLBACK function."
  (let ((buffer (current-buffer)))
    (lambda (content)
      (with-current-buffer buffer
        (if trim
            (funcall callback tag (string-trim-left content))
          (funcall callback tag content))))))


(defun ai-utils--get-explaination-help-buffer ()
  "Return explaination buffer."
  (get-buffer-create ai-utils--explanation-buffer-name))


(defun ai-utils--get-response-buffer ()
  "Return buffer for AI responses."
  (get-buffer-create ai-utils--response-buffer-name))


(defun ai-utils--show-explain-help-buffer (text)
  "Write the TEXT into the explanation buffer and display it."
  (save-excursion
    (with-help-window (ai-utils--get-explaination-help-buffer)
      (princ "AI Explanation below: \n")
      (princ text)
      (switch-to-buffer (ai-utils--get-explaination-help-buffer)))))


(defun ai-utils--show-response-buffer (text)
  "Write the TEXT into the response buffer and display it."
  (save-excursion
    (with-help-window (ai-utils--get-response-buffer)
      (princ text)
      (switch-to-buffer (ai-utils--get-response-buffer)))))


(defun ai-utils--get-random-uuid ()
  "Insert a UUID.

This uses a simple hashing of variable data.

Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5 ,
 which is not cryptographically safe.
 I'm not sure what's the implication of its use here.

Version 2015-01-30
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'."
  ;; by Christopher Wellons, 2011-11-18. Editted by Xah Lee.
  ;; Edited by Hideki Saito further to generate all valid variants for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.

  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))

    (format "%s-%s-4%s-%s%s-%s"
            (substring myStr 0 8)
            (substring myStr 8 12)
            (substring myStr 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring myStr 17 20)
            (substring myStr 20 32))))



(cl-defun ai-utils--async-request (api-url method body headers callback
                                           &key (timeout ai-utils--default-request-timeout))
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
                  (lambda (_events)
                    (progn
                      (ai-utils--log-response request-id (buffer-string))
                      (goto-char url-http-end-of-headers)

                      (condition-case request-error
                          (let ((result (json-read-from-string
                                         (decode-coding-string
                                          (buffer-substring-no-properties url-http-end-of-headers (point-max))
                                          'utf-8))))
                            (funcall callback result))
                        (error (ai-utils--log-and-error (format "Error while parsing response body: %s" (error-message-string request-error)))))))
                  nil nil timeout)))


(cl-defun ai-utils--sync-request (api-url method body headers
                                          &key (timeout ai-utils--default-request-timeout))
  "Performing a synchronous request to API-URL.
Return response content or raise an error.

API-URL is a full API address.
METHOD is a request method.
BODY is request body content.
HEADERS is a list of headers.
TIMEOUT is timeout for request execution."
  (let* ((request-id (ai-utils--get-random-uuid))
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

          (condition-case _request-error
              (let ((result (json-read-from-string
                             (decode-coding-string
                              (buffer-substring-no-properties url-http-end-of-headers (point-max))
                              'utf-8))))
                result)
            (error (progn
                     (ai-utils--log-and-error  "Error while parsing response body")))))
      (ai-utils--log-and-error (format "Failed to send request %s to %s" request-id api-url)))))


(provide 'ai-utils)

;;; ai-utils.el ends here
