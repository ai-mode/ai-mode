;;; ai-utils.el --- Utilities for AI Mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

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
;; Features include logging, buffer manipulation, HTTP requests, content extraction, and more.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

(defcustom ai-utils--write-log-buffer nil
  "If non-nil, enables logging to a special buffer."
  :type 'boolean
  :group 'ai)

(defcustom ai-utils--verbose-log nil
  "If non-nil, enables verbose logging."
  :type 'boolean
  :group 'ai)

(defvar url-http-end-of-headers)

(defvar ai-utils--explanation-buffer-name "*AI explanation*"
  "Name of the buffer used for explanations.")

(defvar ai-utils--response-buffer-name "*AI response*"
  "Name of the buffer used to display AI responses.")

(defvar ai-utils--log-buffer-name "*AI-request-log*"
  "Name of the buffer used for logging API requests.")

(defvar ai-utils--debug-buffer-name "*AI debug buffer*"
  "Name of the buffer used for debugging.")

(defvar ai-utils--prompt-buffer-name "*AI prompt*"
  "Name of the buffer used for storing prompts to the language model.")

(defcustom ai-utils--write-to-prompt-buffer nil
  "If non-nil, writes to the prompt buffer."
  :type 'boolean
  :group 'ai-utils)


(defcustom ai-utils--default-request-timeout 60
  "Default timeout for HTTP requests in seconds."
  :type '(choice integer (const nil))
  :group 'ai-mode)


(defcustom ai-utils--default-preceding-context-size 20
  "Number of lines to include in the preceding context."
  :type 'integer
  :group 'ai-utils)


(defcustom ai-utils--default-following-context-size 20
  "Number of lines to include in the following context."
  :type 'integer
  :group 'ai-utils)


(defvar ai-utils--ai-subdirectory ".ai/"
  "Subdirectory where AI-related files are stored.")


(defcustom ai-utils--instruction-file-extension ".ins"
  "File extension used for instruction files."
  :type 'string
  :group 'ai-utils)


(defun ai-utils--write-log (output)
  "Write OUTPUT to the log buffer.

Logs OUTPUT into `ai-utils--log-buffer-name' if `ai-utils--write-log-buffer' is enabled."
  (when ai-utils--write-log-buffer
    (ai-utils--write-output-to-log-buffer output)
    (ai-utils--write-output-to-log-buffer "\n")))


(defun ai-utils--log-and-error (log-message)
  "Log LOG-MESSAGE and raise an error."
  (ai-utils--write-log log-message)
  (ai-utils--write-log "\n")
  (error (format "%s\n" log-message)))


(defun ai-utils--log-request (request-id method url headers body)
  "Log an HTTP request with REQUEST-ID, METHOD, URL, HEADERS, and BODY."
  (message "Sending %s request[%s] to '%s'" method request-id url)
  (ai-utils--write-log (format "REQUEST[%s]: %s %s\n" request-id method url))
  (ai-utils--write-log (format "HEADERS[%s]\n" headers))
  ;; TODO: write headers
  (ai-utils--write-log (format "%s\n" body)))


(defun ai-utils--log-response (request-id response)
  "Log a RESPONSE for a HTTP request with REQUEST-ID."
  (ai-utils--write-log (format "RESPONSE[%s]:\n" request-id))
  (ai-utils--write-log (format "%s\n\n" response)))


(defun ai-utils--write-output-to-log-buffer (output)
  "Write OUTPUT to the `ai-utils--log-buffer-name' buffer."
  (let ((buffer (get-buffer ai-utils--log-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create ai-utils--log-buffer-name)))
    (with-current-buffer buffer
      (set-buffer-file-coding-system 'utf-8-unix) ;; Setup buffer encoding
      (goto-char (point-max))
      (insert output)
      (goto-char (point-max)))))


(defun ai-utils--clear-log-buffer ()
  "Clear the contents of the `ai-utils--log-buffer-name' buffer."
  (interactive)
  (let ((buffer (get-buffer ai-utils--log-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))


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


(cl-defun ai-utils--with-current-buffer-callback (callback &optional (trim t))
  "Create a CALLBACK function that will operate in the context of the current buffer.

If TRIM is non-nil, trims the content passed to CALLBACK."
  (let ((buffer (current-buffer)))
    (lambda (messages)
      (let ((content (ai-utils--extract-content-from-messages messages)))
        (with-current-buffer buffer
          (funcall callback (if trim (string-trim-left content) content)))))))


(cl-defun ai-utils--replace-region-or-insert-in-current-buffer (&optional (trim t))
  "Create a function to replace the active region or insert into the current buffer.

Replaces only the selected part if a region is active, otherwise replaces the entire buffer.
If TRIM is non-nil, trims the content passed to the `ai-replace-or-insert`."
  (let* ((buffer (current-buffer))
        (region-active (region-active-p))
        (beginning (if region-active (region-beginning) (point-min)))
        (end (if region-active (region-end) (point-max))))
    (lambda (messages)
      (let* ((raw-content (ai-utils--extract-content-from-messages messages))
            (content (if trim (string-trim-left raw-content) raw-content)))
        (when (string-empty-p content)
          (user-error "No content to insert or replace"))
        (with-current-buffer buffer
          (when region-active
            (deactivate-mark))
          (ai-utils--replace-or-insert content beginning end))))))


(cl-defun ai-utils--get-message (messages &optional (message-id 0))
  "Retrieve a message from MESSAGES by its MESSAGE-ID, defaulting to the first message."
  (when (and (listp messages) (> (length messages) 0))
    (if (>= message-id 0)
        (elt messages message-id)
      (car messages))))


(defun ai-utils--extract-content-from-messages (messages)
  "Extract the content from MESSAGES."
  (let ((message-element (ai-utils--get-message messages)))
    (ai-common--get-text-content-from-struct message-element)))


(cl-defun ai-utils--with-current-buffer-tagged-callback (callback tag &optional (trim t))
  "Create a CALLBACK function for inserting tagged content in the current buffer.

TAG is a marker for placing content passed to the CALLBACK function.
If TRIM is non-nil, trims content passed to the CALLBACK."
  (let ((buffer (current-buffer)))
    (lambda (messages)
      (let ((content (ai-utils--extract-content-from-messages messages)))
        (with-current-buffer buffer
          (funcall callback tag (if trim (string-trim-left content) content)))))))


(defun ai-utils--get-explaination-help-buffer ()
  "Return the buffer used for explanations."
  (get-buffer-create ai-utils--explanation-buffer-name))


(defun ai-utils--get-debug-buffer ()
  "Return the buffer used for debugging."
  (get-buffer-create ai-utils--debug-buffer-name))


(defun ai-utils--get-response-buffer ()
  "Return the buffer used for displaying AI responses."
  (get-buffer-create ai-utils--response-buffer-name))


(defun ai-utils--show-explain-help-buffer (text)
  "Display TEXT in the explanation buffer."
  (save-excursion
    (with-help-window (ai-utils--get-explaination-help-buffer)
      (princ "AI Explanation below: \n")
      (princ text)
      (switch-to-buffer (ai-utils--get-explaination-help-buffer)))))


(defun ai-utils--show-response-buffer (messages)
  "Display MESSAGES content in the response buffer."
  (let ((content (ai-utils--extract-content-from-messages messages))
        (buffer-name (ai-utils--get-response-buffer)))
    (with-help-window buffer-name
      (with-current-buffer buffer-name
        (let ((beginning (point-min))
              (end (point-max)))
          (ai-utils--replace-or-insert content beginning end))))))


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


(cl-defun ai-utils--async-request (api-url method body headers callback
                                           &key (timeout ai-utils--default-request-timeout))
  "Send an asynchronous HTTP request to API-URL.

METHOD specifies the HTTP method.
BODY is the request body.
HEADERS is a list of request headers.
CALLBACK is a function called with the response.
TIMEOUT specifies the request timeout."
  (let ((request-id (ai-utils--get-random-uuid))
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
  "Send a synchronous HTTP request to API-URL and return the response content.

METHOD specifies the HTTP method.
BODY is the request body.
HEADERS is a list of request headers.
TIMEOUT specifies the request timeout."
  (let ((request-id (ai-utils--get-random-uuid))
        (url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data body)
        (buffer (url-retrieve-synchronously api-url 'silent nil timeout)))
    (ai-utils--log-request request-id url-request-method api-url headers body)
    (if buffer
        (with-current-buffer buffer
          (ai-utils--log-response request-id (buffer-string))
          (goto-char url-http-end-of-headers)
          (condition-case _request-error
              (json-read-from-string
               (decode-coding-string
                (buffer-substring-no-properties url-http-end-of-headers (point-max))
                'utf-8))
            (error (ai-utils--log-and-error  "Error while parsing response body"))))
      (ai-utils--log-and-error (format "Failed to send request %s to %s" request-id api-url)))))


(defun ai-utils--get-buffer-path (buffer)
  "Return the file path associated with the BUFFER, or nil if none."
  (interactive)
  (with-current-buffer buffer
    (if (projectile-project-p)
        (let ((project-root (projectile-project-root))
              (file-name (buffer-file-name)))
          (and project-root file-name (file-relative-name file-name project-root)))
      (buffer-file-name))))


(defun ai-utils--get-model-context (model)
  "Get the context information for the given MODEL."
  (let ((name (map-elt model :name ""))
        (provider (map-elt model :provider "")))
    `(:model ,name
      :provider ,provider)))


(defun ai-utils--get-buffer-context (buffer)
  "Retrieve buffer-related context information for BUFFER."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name)))
      `(:buffer-name ,(buffer-name buffer)
        :buffer-language ,(ai-utils--get-buffer-type-or-language buffer)
        :buffer ,buffer
        :file-path ,(ai-utils--get-buffer-path buffer)
        :project-root ,(and (projectile-project-p) (projectile-project-root))
        :file-name ,file-name))))


(cl-defun ai-utils--get-completion-params (&key (preceding-context-size ai-utils--default-preceding-context-size)
                                              (following-context-size ai-utils--default-following-context-size))
  "Retrieve completion parameters including context and cursor information."
  (let* ((precending-context-beginning
          (if (region-active-p)
              (region-beginning)
            (if (or (equal preceding-context-size -1)
                    (null preceding-context-size))
                (point-min)
              (max 1 (- (point) preceding-context-size)))))
         (precending-context-end (if (region-active-p) (region-end) (point)))
         (region-content (and (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end))))
         (cursor-point (point))
         (cursor-offset precending-context-end)
         (preceding-context-content (buffer-substring-no-properties precending-context-beginning precending-context-end))
         (preceding-context-size (length preceding-context-content))
         (following-context-beginning (min (+ precending-context-end 1) (point-max)))
         (following-context-end (if (or (equal following-context-size -1)
                                        (null following-context-size))
                                    (point-max)
                                  (min (point-max) (+ following-context-beginning following-context-size))))
         (following-context-content (buffer-substring-no-properties following-context-beginning following-context-end))
         (following-context-size (length following-context-content)))

    `(:cursor-point ,cursor-point
                    :cursor-offset ,cursor-offset
                    :region-content ,region-content
                    :preceding-context-beginning ,precending-context-beginning
                    :preceding-context-end ,precending-context-end
                    :preceding-context-content ,preceding-context-content
                    :preceding-context-size ,preceding-context-size
                    :following-context-beginning ,following-context-beginning
                    :following-context-end ,following-context-end
                    :following-context-content ,following-context-content
                    :following-context-size ,following-context-size
                    :cursor-line-number ,(line-number-at-pos)
                    :cursor-column-number ,(current-column))))


(defvar ai-utils--language-alist
  '((nil . "text")
    (text-mode . "Plain text")
    (c-mode . "C")
    (c-ts-mode . "C")
    (clojure-mode . "Clojure")
    (clojurec-mode . "Clojure")
    (clojurescript-mode . "Clojure")
    (coffee-mode . "CoffeeScript")
    (cc-mode . "C++")
    (c++-mode . "C++")
    (c++-ts-mode . "C++")
    (csharp-mode . "C#")
    (csharp-tree-sitter-mode . "C#")
    (csharp-ts-mode . "C#")
    (css-mode . "CSS")
    (css-ts-mode . "CSS")
    (cuda-mode . "CUDA")
    (dockerfile-mode . "Dockerfile")
    (dockerfile-ts-mode . "Dockerfile")
    (go-dot-mod-mode . "Go")
    (go-mod-ts-mode . "Go")
    (go-mode . "Go")
    (go-ts-mode . "Go")
    (groovy-mode . "Groovy")
    (haskell-mode . "Haskell")
    (terraform-mode . "Terraform")
    (html-mode . "HTML")
    (sgml-mode . "HTML")
    (mhtml-mode . "HTML")
    (java-mode . "Java")
    (java-ts-mode . "Java")
    (jdee-mode . "Java")
    (ecmascript-mode . "JavaScript")
    (javascript-mode . "JavaScript")
    (js-mode . "JavaScript")
    (js2-mode . "JavaScript")
    (js-ts-mode . "JavaScript")
    (rjsx-mode . "JavaScript")
    (json-mode . "JSON")
    (json-ts-mode . "JSON")
    (julia-mode . "Julia")
    (ess-julia-mode . "Julia")
    (kotlin-mode . "Kotlin")
    (kotlin-ts-mode . "Kotlin")
    (latex-mode . "LaTeX")
    (less-mode . "Less")
    (less-css-mode . "Less")
    (lua-mode . "Lua")
    (lsp--render-markdown . "Markdown")
    (markdown-mode . "Markdown")
    (gfm-mode . "Markdown")
    (objc-mode . "Objective-C")
    (perl-mode . "Perl")
    (cperl-mode . "Perl")
    (php-mode . "PHP")
    (python-mode . "Python")
    (python-ts-mode . "Python")
    (cython-mode . "Python")
    (ess-r-mode . "R")
    (ruby-mode . "Ruby")
    (enh-ruby-mode . "Ruby")
    (ruby-ts-mode . "Ruby")
    (rust-mode . "Rust")
    (rust-ts-mode . "Rust")
    (rustic-mode . "Rust")
    (sass-mode . "Sass")
    (ssass-mode . "Sass")
    (scala-mode . "Scala")
    (scss-mode . "SCSS")
    (sh-mode . "Shell")
    (ebuild-mode . "Shell")
    (pkgbuild-mode . "Shell")
    (sql-mode . "SQL")
    (swift-mode . "Swift")
    (ts-mode . "TypeScript")
    (typescript-mode . "TypeScript")
    (typescript-ts-mode . "TypeScript")
    (nxml-mode . "XML")
    (xml-mode . "XML")
    (yaml-mode . "YAML")
    (yaml-ts-mode . "YAML")
    (conf-toml-mode . "TOML")
    (toml-ts-mode . "TOML")
    (dart-mode . "Dart")
    (caml-mode . "OCaml")
    (tuareg-mode . "OCaml")
    (cmake-mode . "CMake")
    (cmake-ts-mode . "CMake")
    (pascal-mode . "Pascal")
    (elixir-mode . "Elixir")
    (elixir-ts-mode . "Elixir")
    (heex-ts-mode . "Elixir")
    (fsharp-mode . "F#")
    (lisp-mode . "Lisp")
    (emacs-lisp-mode . "Emacs Lisp")))


(defun ai-utils--get-buffer-type-or-language (buffer)
  "Get the programming language for BUFFER from `ai-utils--language-alist'."
  (with-current-buffer buffer
    (let ((mode major-mode))
      (while (not (alist-get mode ai-utils--language-alist))
	(setq mode (get mode 'derived-mode-parent)))
      (alist-get mode ai-utils--language-alist))))


(defun ai-utils--render-template (template keyword-list)
  "Replace variables in TEMPLATE with values from KEYWORD-LIST.
Variables in TEMPLATE are denoted by {{:key}}, where 'key'
corresponds to a keyword in KEYWORD-LIST."
  (let ((result template))
    (while keyword-list
      (let ((key (car keyword-list))
            (value (cadr keyword-list)))
        (setq result
              (replace-regexp-in-string
               (format "{{:%s}}" (substring (symbol-name key) 1))
               (if (stringp value) value (format "%s" value))
               result
               'fixedcase 'literal))
        (setq keyword-list (cddr keyword-list))))
    result))


(defun ai-utils--get-query-type-instructions-file (command-name)
  "Return the path to the instructions file for COMMAND-NAME.
The file is located in the .ai/ subdirectory with
the extension from `ai-utils--instruction-file-extension'."
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name (concat ai-utils--ai-subdirectory
                              (replace-regexp-in-string " " "_" command-name)
                              ai-utils--instruction-file-extension)
                      current-dir)))


(defun ai-utils--get-buffer-root-path (buffer)
  "Attempt to retrieve the root directory of BUFFER's project or its local directory."
  (interactive)
  (with-current-buffer buffer
    (or (and (projectile-project-p) (projectile-project-root))
        (and (buffer-file-name) (file-name-directory (buffer-file-name))))))


(defun ai-utils--get-instructions-from-file (path)
  "Return the string contents from a file at PATH if readable.
Output is logged if `ai-utils--verbose-log' is non-nil."
  (when ai-utils--verbose-log
    ;; (message "Loading instructions from file: %s" path)
    )
  (if (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))))


(defun ai-utils--verbose-message (&rest args)
  "Output a message if `ai-utils--verbose-log' is non-nil.
ARGS are the message parts."
  (when ai-utils--verbose-log
    (apply 'message args)))


(defun ai-utils--file-instructions-for-command (command-name base-path)
  "Return the path to the instruction file for COMMAND-NAME at BASE-PATH.
The file is located in the .ai/ subdirectory and is suffixed
with `ai-utils--instruction-file-extension'."
  (let ((file-path (expand-file-name (concat ai-utils--ai-subdirectory
                                             (replace-regexp-in-string " " "_" command-name)
                                             ai-utils--instruction-file-extension)
                                     base-path)))
    (ai-utils--get-instructions-from-file file-path)))


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


(defun ai-utils--get-model-key (model key)
  "Retrieve the value for KEY from MODEL."
  (map-elt model key))


(defun ai-utils--find-model-config-by-name (name elements)
  "Search ELEMENTS for a model configuration matching NAME and return it."
  (cl-find-if (lambda (element)
                (string= (cdr (assoc :name element)) name))
              elements))


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


(defun ai-utils--user-input ()
  "Prompt the user for multiline input in a temporary buffer and return it as a string."
  (interactive)
  (let ((original-buffer (current-buffer))
        (original-window (selected-window))
        (original-point (point))
        (buffer (get-buffer-create "*Multiline Input*")))
    ;; Setup the temporary buffer
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (local-set-key (kbd "C-c C-c") #'exit-recursive-edit))
    ;; Display to user
    (pop-to-buffer buffer)
    (message "Type your text and press C-c C-c to finish. Press C-g to cancel.")
    ;; Handle input and cancel
    (condition-case nil
        (progn
          (recursive-edit) ;; Wait for completion
          (let ((input-text (with-current-buffer buffer
                              (buffer-substring-no-properties (point-min) (point-max)))))
            (kill-buffer buffer)
            (select-window original-window)
            (with-current-buffer original-buffer
              (goto-char original-point))
            input-text))
      (quit  ;; Handle C-g
       (kill-buffer buffer)
       (select-window original-window)
       (with-current-buffer original-buffer
         (goto-char original-point))
       (message "Input cancelled.")
       nil))))


(defun ai-utils-escape-format-specifiers (str)
  "Escape all '%' specifiers in STR by replacing them with '%%'."
  (replace-regexp-in-string "%" "%%" str))


(defun ai-utils--is-empty-message (message)
  "Determine if MESSAGE has an empty or missing :content field."
  (let ((content (plist-get message :content)))
    (or (null content)
        (string-empty-p content))))


(defun ai-utils-filter-non-empty-content (messages)
  "Filter out messages with empty content from MESSAGES."

  (cl-remove-if #'ai-utils--is-empty-message messages))


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


(defun ai-utils-write-context-to-prompt-buffer (messages)
  "Extract 'context' from MESSAGES and write it to the prompt buffer if enabled."
  (when ai-utils--write-to-prompt-buffer
    (let ((buffer (get-buffer-create ai-utils--prompt-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)   ; Clear the buffer before writing new content
        (dolist (message messages)
          (let ((content (ai-common--get-text-content-from-struct message)))
            (when content
              (insert content "\n"))))))))



(defun ai-utils--show-and-eval-response (messages)
  "Show MESSAGES in a buffer and ask user for permission to evaluate the Emacs Lisp code."
  (let* ((content (ai-utils--extract-content-from-messages messages))
         (buffer-name "*AI Generated Code*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (emacs-lisp-mode)
      (insert content)
      (goto-char (point-min))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure)))

    (pop-to-buffer buffer)

    (when (yes-or-no-p "The AI has generated Emacs Lisp code. Do you want to evaluate it? ")
      (condition-case err
          (progn
            (eval-buffer buffer)
            (message "Code evaluated successfully."))
        (error
         (message "Error evaluating code: %s" (error-message-string err)))))))


(provide 'ai-utils)

;;; ai-utils.el ends here
