;;; ai-utils.el --- ai-mode utils -*- lexical-binding: t -*-

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


(defcustom ai--write-log-buffer nil
  ""
  :type 'boolean
  :group 'ai
  )

(defvar ai--log-buffer-name "*AI-request-log*")

(defun ai--write-log (output)
  ""
  (when ai--write-log-buffer
    (ai--write-output-to-log-buffer output)
    (ai--write-output-to-log-buffer "\n")
    )
  )


(defun ai--log-and-error (log-message)
  ""
  (ai--write-log log-message)
  (ai--write-log "\n")
  (error log-message)
  )


(defun ai--log-request (request-id method url headers body)
  ""

  (message "Sending %s request[%s] to '%s" method request-id url)
  (ai--write-log (format "REQUEST[%s]: %s %s\n" request-id method url))
  ;; TODO: write headers
  (ai--write-log (format "%s\n" body))
  )

(defun ai--log-response (request-id response)
  ""
  (ai--write-log (format "RESPONSE[%s]:\n" request-id))
  (ai--write-log (format "%s\n\n" response))
  )

(defun ai--write-output-to-log-buffer (output)
  ""
  (let ((buffer (get-buffer ai--log-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create ai--log-buffer-name)))

    (with-current-buffer buffer
      (progn
        (let ((beginning-of-input (goto-char (point-max))))
          (insert output)
          )
        )
      )
    )
  )

(defun ai--get-region-content ()
  ""
  (buffer-substring-no-properties (region-beginning) (region-end))
  )

(defun ai--get-buffer-content ()
  (buffer-substring-no-properties (point-min) (point-max))
  )

(defun ai--get-buffer-content-before-point ()
  ""
  (buffer-substring-no-properties (point-min) (point))
  )

(defun ai--insert-after-region (text)
  "Insert the given TEXT after the region."
  (when (region-active-p)
    (goto-char (region-end))
    (insert text)))

(defun ai--insert-at-point (text)
  "Insert the given TEXT after the region."
  (goto-char (point))
  (insert text))

(defun ai--insert-completion (text)
  ""
  (if (region-active-p)
      (ai--insert-after-region text)
    (ai--insert-at-point text))
  )

(defun ai--get-completion-context ()
  ""
  (ai--get-buffer-content-before-point)
  )


(defun ai--replace-region (text)
  ""
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))
    (insert text)))

(defun ai--replace-tag-in-region (tag text)
  ""
  (when (region-active-p)
    (let ((region-content (ai--get-region-content)))
      (delete-region (region-beginning) (region-end))
      (insert (replace-regexp-in-string tag text region-content)))
    )
  )


(defun ai--get-random-uuid ()
  "Insert a UUID. This uses a simple hashing of variable data.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

Note: this code uses https://en.wikipedia.org/wiki/Md5 , which is not cryptographically safe. I'm not sure what's the implication of its use here.

Version 2015-01-30
URL `http://ergoemacs.org/emacs/elisp_generate_uuid.html'
"
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



(provide 'ai-utils)

;;; ai-utils.el ends here
