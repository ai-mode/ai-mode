;;; ai-utils.el --- Utilities for AI Mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, ai

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
;; This file provides various utility functions to support AI mode in Emacs.
;; Features include buffer manipulation, content extraction, and more.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'ai-user-input)


(defvar ai-utils--debug-buffer-name "*AI debug buffer*"
  "Name of the buffer used for debugging.")

(defun ai-utils--get-query-content ()
  "Return the content of the active region or the full buffer."
  (if (region-active-p)
      (ai-utils--get-region-content)
    (ai-utils--get-buffer-content)))

(defun ai-utils--get-region-content ()
  "Return the content of the active region without properties."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun ai-utils--get-buffer-content ()
  "Return the content of the current buffer without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ai-utils--get-buffer-content-before-point ()
  "Return the content of the current buffer from the beginning up to the current point."
  (buffer-substring-no-properties (point-min) (point)))

(defun ai-utils--insert-after-region (text)
  "Insert TEXT after the selected region."
  (when (region-active-p)
    (goto-char (region-end))
    (insert text)))

(defun ai-utils--insert-at-point (text)
  "Insert TEXT at the current point."
  (goto-char (point))
  (insert text))

(defun ai-utils--insert-completion (text)
  "Insert TEXT after the selected region or at the cursor position."
  (if (region-active-p)
      (ai-utils--insert-after-region text)
    (ai-utils--insert-at-point text)))

(defun ai-utils--insert-completion-at-point (pos content)
  "Insert CONTENT at position POS in the buffer."
  (goto-char pos)
  (insert content))

(cl-defun ai-utils--replace-or-insert (text &optional beginning end)
  "Replace the currently selected region with TEXT or the area between BEGINNING and END."
  (cond
   ((region-active-p)
    (delete-region (region-beginning) (region-end))
    (insert text))
   ((and beginning end)
    (delete-region beginning end)
    (insert text))
   (t (insert text))))

(defun ai-utils--replace-tag-in-region (tag text)
  "Replace occurrences of TAG in the selected region with TEXT."
  (when (region-active-p)
    (let ((region-content (ai-utils--get-region-content)))
      (delete-region (region-beginning) (region-end))
      (insert (replace-regexp-in-string tag text region-content)))))

(defun ai-utils--get-debug-buffer ()
  "Return the buffer used for debugging."
  (get-buffer-create ai-utils--debug-buffer-name))

(defun ai-utils--show-context-debug (context)
  "Display debugging information for CONTEXT."
  (save-excursion
    (with-help-window (ai-utils--get-debug-buffer)
      (princ (format "%s" (pp-to-string context))))))

(defun ai-utils--get-random-uuid ()
  "Generate and return a random UUID.

This function uses a hashing of various variable data.
Note: It uses MD5 which is not cryptographically secure."
  ;; Original function by Christopher Wellons, 2011-11-18. Edits by Xah Lee.
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

(defun ai-utils--get-buffer-root-path (buffer)
  "Attempt to retrieve the root directory of BUFFER's project or its local directory."
  (interactive)
  (with-current-buffer buffer
    (or (and (projectile-project-p) (projectile-project-root))
        (and (buffer-file-name) (file-name-directory (buffer-file-name))))))

(defun ai-utils--clone-buffer (source-buffer)
  "Create a copy of SOURCE-BUFFER and return its name."
  (with-current-buffer source-buffer
    (let ((buffer-content (buffer-string))
          (point-pos (point))
          (mark-pos (mark))
          (temp-buffer (generate-new-buffer (concat "*copy of " (buffer-name source-buffer) "*"))))
      (with-current-buffer temp-buffer
        (insert buffer-content)
        (goto-char point-pos)
        (set-mark mark-pos)
        (setq mark-active t))
      (buffer-name temp-buffer))))

(defun ai-utils--describe-hash-table-variable (variable)
  "Display a description of a hash table VARIABLE and its contents."
  (interactive
   (list (intern (completing-read "Describe hash table variable: " obarray
                                  (lambda (x)
                                    (and (boundp x)
                                         (hash-table-p (symbol-value x))))
                                  t nil 'variable-name-history))))
  (let ((htable (symbol-value variable))
        (buffer-name (format "*Hash Table: %s*" variable)))
    (if (hash-table-p htable)
        (with-help-window buffer-name
          (princ (format "Hash Table: %s %s\n\n" (symbol-name variable) htable))
          (princ (format "Size: %d\nTest: %s\n\n" (hash-table-count htable) (hash-table-test htable)))
          (princ "Contents:\n")
          (maphash (lambda (key value)
                     (princ (format "  Key: %s\n  Value: %s\n\n" key value)))
                   htable))
      (error "Variable is not a hash table"))))

(defun my-show-diff-between-two-strings (text-a text-b &optional buffer-name)
  "Display a diff between TEXT-A and TEXT-B in a BUFFER-NAME, if provided."
  (let* ((buf-name (or buffer-name "*My String Diff*"))
         (buffer (get-buffer-create buf-name))
         (file-a (make-temp-file "diff-a-elisp-"))
         (file-b (make-temp-file "diff-b-elisp-")))
    (unwind-protect
        (progn
          ;; Write texts to temporary files
          (with-temp-file file-a
            (insert text-a))
          (with-temp-file file-b
            (insert text-b))

          ;; Prepare the buffer and call diff
          (with-current-buffer buffer
            (read-only-mode -1)
            (erase-buffer)
            ;; Run diff and insert output into buffer
            (let ((exit-status
                   (call-process "diff" nil buffer nil "-u" file-a file-b)))
              (if (and (numberp exit-status)
                       (<= exit-status 1)) ;; 'diff' exit status 0 and 1 are normal
                  (progn
                    (diff-mode)                 ;; Switch to diff-mode
                    (font-lock-fontify-buffer)) ;; Enable syntax highlighting
                (error "Diff error, exit status %s" exit-status)))
            (read-only-mode 1)
            (goto-char (point-min)))
          (display-buffer buffer))
      ;; Cleanup temporary files
      (delete-file file-a)
      (delete-file file-b))))

(defun ai-utils-generate-unique-buffer-name (base-name)
  "Generate a unique buffer name starting with BASE-NAME by adding a suffix."
  (let ((counter 1)
        (unique-name base-name))
    (while (get-buffer unique-name)
      (setq unique-name (format "%s<%d>" base-name counter))
      (setq counter (1+ counter)))
    unique-name))

(defun ai-utils-escape-format-specifiers (str)
  "Escape all '%' specifiers in STR by replacing them with '%%'."
  (replace-regexp-in-string "%" "%%" str))


(defun ai-utils--html-tag-to-string (tag)
  "Convert an HTML TAG structure to a string."
  (let ((name (symbol-name (car tag)))
        (attrs (cadr tag))
        (content (cddr tag)))
    (concat
     "<" name
     (mapconcat (lambda (attr)
                  (format " %s=\"%s\"" (car attr) (cadr attr)))
                attrs
                "")
     ">"
     (mapconcat (lambda (item)
                  (if (listp item)
                      (ai-utils--html-tag-to-string item)
                    (format "%s" item)))
                content
                "")
     "</" name ">")))

(provide 'ai-utils)

;;; ai-utils.el ends here
