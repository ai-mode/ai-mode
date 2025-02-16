;;; ai-common.el --- Common AI logic -*- lexical-binding: t -*-
;;
;; This file is part of GNU Emacs.
;;
;; Copyright (C) 2023 Alex (https://github.com/lispython)
;;
;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools
;;
;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;; Commentary:
;; This file contains common logic for AI functionalities, focusing on managing
;; contexts and memories related to AI interactions.
;;
;; The main features include:
;; - Global and buffer-specific memory management
;; - Context and snippet handling for AI interactions
;; - Support for user input and context pool management
;;
;;; Code:

(require 'ai-utils)

;;; Memory and context variables for AI interaction

(defvar ai--global-memo-context nil
  "Global memory context for storing important information that persists across all buffers.
This variable holds data that should be available to AI in any interaction session.")

(defvar ai-common--context-pool nil
  "Temporary context pool for the current interaction session.
Unlike global memory, this holds context that is only relevant for the current
AI operation and will be cleared after completion.")

(defvar-local ai-common--buffer-bound-prompts nil
  "Buffer-local instructions that apply only to the current buffer.
These prompts are specifically tied to a particular buffer and do not affect
other buffers or global AI behavior.")

(defcustom ai-common--global-system-prompts nil
  "Global system instructions for AI.
These define the fundamental behavior and capabilities of the AI assistant
across all interactions and buffers."
  :type 'string
  :group 'ai)

(defun ai-common--add-to-global-memory (input)
  "Add INPUT to `ai--global-memo-context'.
INPUT is a string representing the context or information to be remembered globally."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter instruction: "))))
  (let ((struct (ai-common--make-typed-struct input 'global-memory-item)))
    (setq ai--global-memo-context (append ai--global-memo-context `(,struct)))
    (message "AI memory context added")))

(defun ai-common--clear-global-memory ()
  "Clear the global memory context."
  (interactive)
  (setq ai--global-memo-context nil)
  (message "AI buffer context cleared"))

(defun ai-common--get-global-memory ()
  "Return the current global memory context."
  ai--global-memo-context)

(defun ai-common--add-buffer-bound-prompts (input)
  "Add INPUT as buffer-specific instructions.
INPUT can be entered by the user or taken from the active region."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter buffer bound prompt: "))))

  (with-current-buffer (current-buffer)
    (let ((struct (ai-common--make-typed-struct input 'buffer-bound-prompt)))
      (message "AI buffer bound context added")
      (setq-local ai-common--buffer-bound-prompts (append ai-common--buffer-bound-prompts `(,struct))))))

(defun ai-common--clear-buffer-bound-prompts ()
  "Clear all buffer-specific instructions."
  (interactive)
  (with-current-buffer (current-buffer)
    (message "AI buffer context cleared")
    (setq-local ai-common--buffer-bound-prompts nil)))

(defun ai-common--get-buffer-bound-prompts ()
  "Return the current buffer-specific instructions."
  ai-common--buffer-bound-prompts)

(defun ai-common--add-global-system-prompts (input)
  "Add INPUT to the global system prompts.
INPUT can be entered by the user or taken from the active region."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter global system prompt: "))))

  (with-current-buffer (current-buffer)
    (let ((struct (ai-common--make-typed-struct input 'global-system-prompt)))
      (setq ai-common--global-system-prompts
            (append ai-common--global-system-prompts `(,struct))))))

(defun ai-common--clear-global-system-prompts ()
  "Clear all global system instructions."
  (interactive)
  (with-current-buffer (current-buffer)
    (setq ai-common--global-system-prompts nil)))

(defun ai-common--get-global-system-prompts ()
  "Return the current global system instructions."
  ai-common--global-system-prompts)

(defun ai-common--clear-context-pool ()
  "Clear the temporary context pool."
  (interactive)
  (setq ai-common--context-pool nil)
  (message "Clear context pool"))

(defun ai-common--get-context-pool ()
  "Return the current context pool."
  ai-common--context-pool)

(defun ai-common--make-file-context ()
  "Create a plist containing the full content of the current buffer as a file context.
This includes metadata such as file name, buffer name, and timestamps."
  (let* ((beg  (point-min))
         (end  (point-max))
         (file (or (buffer-file-name) (buffer-name)))
         (buf  (buffer-name))
         (ts   (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id   (ai-common--generate-id 'file-context)))
    `(:type       file-context
                  :content    ,(buffer-substring-no-properties beg end)
                  :file       ,file
                  :buffer     ,buf
                  :start-pos  ,beg
                  :end-pos    ,end
                  :mode       ,(symbol-name major-mode)
                  :timestamp  ,ts
                  :id         ,id)))

(defun ai-common--make-snippet-from-region (&optional tag-type)
  "Create a plist structure from the current region.
If TAG-TYPE is provided, it is used as the :type value instead of 'snippet."
  (when (use-region-p)
    (let* ((type       (or tag-type 'snippet))
           (start      (region-beginning))
           (end        (region-end))
           (file       (or (buffer-file-name) (buffer-name)))
           (buf        (buffer-name))
           (line-beg   (line-number-at-pos start))
           (line-end   (line-number-at-pos end))
           (col-beg    (car (posn-col-row (posn-at-point start))))
           (col-end    (car (posn-col-row (posn-at-point end))))
           (mode       (symbol-name major-mode))
           (ts         (format-time-string "%Y-%m-%dT%H:%M:%S"))
           (id         (ai-common--generate-id type))
           (content    (buffer-substring-no-properties start end)))
      `(:type              ,type
                           :content           ,content
                           :file              ,file
                           :buffer            ,buf
                           :start-pos         ,start
                           :end-pos           ,end
                           :start-line        ,line-beg
                           :end-line          ,line-end
                           :start-column      ,col-beg
                           :end-column        ,col-end
                           :mode              ,mode
                           :timestamp         ,ts
                           :id                ,id))))

(defun ai-common--generate-id (type)
  "Generate a unique ID for a struct of TYPE.
The format will be 'type-hash' where hash is an MD5 substring of the current time."
  (format "%s-%s"
          (symbol-name type)
          (substring (md5 (number-to-string (float-time))) 0 8)))

(defun ai-common--make-typed-struct (content &optional type source &rest additional-props)
  "Create a plist structure from CONTENT with optional TYPE, SOURCE and ADDITIONAL-PROPS.
CONTENT can be text, a typed struct, or a list of typed structs.
TYPE is a symbol used to differentiate message types (defaults to 'default').
SOURCE indicates where the message originated (can be nil).
ADDITIONAL-PROPS are key-value pairs to be included in the resulting plist.
Returns a plist with :type, :content, :timestamp, :source, :id fields and any additional properties."
  (let* ((type-val (or type 'default))
         (base-plist `(:type ,type-val
                       :content ,content
                       :timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%S")
                       :id ,(ai-common--generate-id type-val))))
    ;; Add source only if provided
    (when source
      (setq base-plist (plist-put base-plist :source source)))

    ;; Add any additional properties
    (when additional-props
      (setq base-plist (append base-plist additional-props)))

    base-plist))


(defun ai-common--get-user-input ()
  "Create a plist structure based on user input via minibuffer."
  (let ((text (read-string "Enter instruction or context for AI: ")))
    (ai-common--make-typed-struct text 'user-input 'user-input)))


(defun ai-common--make-additional-context (content &optional source)
  "Create a plist structure with type 'additional-context' from CONTENT.
Optional SOURCE parameter specifies the source of the context."
  (ai-common--make-typed-struct content 'additional-context source))

(defun ai-common--make-preceding-context (size)
  "Return a plist describing the preceding-context of SIZE chars before point."
  (let* ((end    (point))
         (beg    (max (point-min) (- end size)))
         (file   (or (buffer-file-name) (buffer-name)))
         (buf    (buffer-name))
         (start-line   (line-number-at-pos beg))
         (end-line     (line-number-at-pos end))
         (start-col    (car (posn-col-row (posn-at-point beg))))
         (end-col      (car (posn-col-row (posn-at-point end))))
         (mode         (symbol-name major-mode))
         (ts           (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id           (ai-common--generate-id 'preceding-context))
         (content      (buffer-substring-no-properties beg end)))
    `(:type              preceding-context
                         :content           ,content
                         :file              ,file
                         :buffer            ,buf
                         :start-pos         ,beg
                         :end-pos           ,end
                         :start-line        ,start-line
                         :end-line          ,end-line
                         :start-column      ,start-col
                         :end-column        ,end-col
                         :mode              ,mode
                         :timestamp         ,ts
                         :id                ,id)))

(defun ai-common--make-following-context (size)
  "Return a plist describing the following-context of SIZE chars after point."
  (let* ((beg    (point))
         (end    (min (point-max) (+ beg size)))
         (file   (or (buffer-file-name) (buffer-name)))
         (buf    (buffer-name))
         (start-line   (line-number-at-pos beg))
         (end-line     (line-number-at-pos end))
         (start-col    (car (posn-col-row (posn-at-point beg))))
         (end-col      (car (posn-col-row (posn-at-point end))))
         (mode         (symbol-name major-mode))
         (ts           (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id           (ai-common--generate-id 'following-context))
         (content      (buffer-substring-no-properties beg end)))
    `(:type              following-context
      :content           ,content
      :file              ,file
      :buffer            ,buf
      :start-pos         ,beg
      :end-pos           ,end
      :start-line        ,start-line
      :end-line          ,end-line
      :start-column      ,start-col
      :end-column        ,end-col
      :mode              ,mode
      :timestamp         ,ts
      :id                ,id)))

(defun ai-common--add-to-context-pool (item)
  "Add ITEM (plist-structure) to `ai-common--context-pool`."
  (push item ai-common--context-pool))

(defun ai-common--capture-region-snippet ()
  "Create a snippet from the region and add it to the context pool."
  (interactive)
  (let ((snippet (ai-common--make-snippet-from-region)))
    (when snippet
        (ai-common--add-to-context-pool snippet))))

(defun ai-common--capture-user-input ()
  "Capture user input and add it to the context pool."
  (interactive)
  (ai-common--add-to-context-pool (ai-common--get-user-input)))

(defun ai-common--capture-file-context ()
  "Adds the entire file content to the context pool."
  (interactive)
  (let ((file-context (ai-common--make-file-context)))
    (ai-common--add-to-context-pool file-context)
    (message "File content added to context pool.")))

(defun ai-common--stringify (val)
  "Convert VAL to a simple string."
  (cond
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((numberp val) (number-to-string val))
   (t (format "%s" val))))

(defun ai-common--make-xml-element-from-plist-or-symbol (item)
  "Convert ITEM into an XML-like S-expression.
If ITEM is a symbol, return a self-closing tag without attributes or content.
If ITEM is a plist, return an XML element with attributes and content."
  (if (symbolp item)
      ;; self-closing tag: (cursor ())
      (list item '())
    ;; plist → (tag-name attrs content)
    (let* ((tag-name (plist-get item :type))
           (content  (plist-get item :content))
           (attrs    (cl-loop for (k v) on item by #'cddr
                              unless (memq k '(:type :content))
                              collect (cons
                                       (substring (symbol-name k) 1)
                                       (ai-common--stringify v)))))
      (list tag-name attrs content))))

(defun ai-common--render-tag-to-string (tag)
  "Serialize TAG into an XML-like string.

– If TAG is a symbol or keyword (e.g. `cursor` or `:cursor`), emits `<cursor/>`.
– If TAG is a list of the form (name attrs content...), then:
    • if attrs and content are both empty ⇒ `<name/>`
    • otherwise ⇒ `<name attr=\"…\">content</name>`."
  (cond
   ;; 1) Symbol - self-closing
   ((symbolp tag)
    (let* ((raw  (symbol-name tag))
           (name (if (and (> (length raw) 0)
                          (eq (aref raw 0) ?:))
                     (substring raw 1)
                   raw)))
      (format "<%s/>" name)))

   ;; 2) List (name attrs content...) - either self-close or full tag
   ((and (listp tag) (symbolp (car tag)))
    (let* ((raw-name (symbol-name (car tag)))
           (name     (if (and (> (length raw-name) 0)
                               (eq (aref raw-name 0) ?:))
                         (substring raw-name 1)
                       raw-name))
           (attrs    (or (cadr tag) '()))
           (content  (cddr tag)))
      (if (and (null attrs) (null content))
          ;; No attributes and no content
          (format "<%s/>" name)
        ;; Attributes or content exist
        (let ((attrs-str
               (mapconcat
                (lambda (a) (format " %s=\"%s\"" (car a) (cdr a)))
                attrs
                ""))
              (inner-str
               (mapconcat
                (lambda (c)
                  (if (and (listp c) (symbolp (car c)))
                      (ai-common--render-tag-to-string c)
                    (format "%s" c)))
                content
                "")))
          (format "<%s%s>%s</%s>"
                  name attrs-str inner-str name)))))

   ;; 3) Anything else - error
   (t
    (error "Cannot render tag: %S" tag))))

(defun ai-common--render-context-pool-as-string (context-pool)
  "Group `ai-common--context-pool` by :source and return an entire XML-string."
  (let ((groups nil))
    ;; Gather groups in alist format (("region" . (item1 item2 …)) …)
    (dolist (item (reverse context-pool))
      (let* ((src   (ai-common--stringify (plist-get item :source)))
             (entry (assoc src groups)))
        (if entry
            (setcdr entry (append (cdr entry) (list item)))
          (push (cons src (list item)) groups))))
    (setq groups (nreverse groups))
    ;; Build an XML block for each group
    (let ((xml-blocks nil))
      (dolist (grp groups)
        (let ((src   (car grp))
              (elems (cdr grp)))
          (let ((inner
                 (mapconcat
                  (lambda (itm)
                    (let ((el (ai-common--make-xml-element-from-plist-or-symbol itm)))
                      (concat "  " (ai-common--render-tag-to-string el))))
                  elems
                  "\n")))
            (push
             (concat "<additional-context source=\""
                     src "\">\n"
                     inner
                     "\n</additional-context>")
             xml-blocks))))
      ;; Combine all blocks into a final string
      (mapconcat #'identity (nreverse xml-blocks) "\n\n"))))

(defun ai-common--render-struct-to-string (struct)
  "Render a typed structure STRUCT to an XML-like string.
STRUCT can be a plist, a symbol or a list of other structures.
For plists, tag name is determined by :type property.
Handles nested structures recursively."
  (cond
   ;; Simple symbol case (like :cursor)
   ((symbolp struct)
    (let* ((tag-name (if (keywordp struct)
                         (substring (symbol-name struct) 1)
                       (symbol-name struct))))
      (format "<%s/>" tag-name)))

   ;; Plist case
   ((and (listp struct) (keywordp (car struct)))
    (let* ((type (plist-get struct :type))
           (tag-name (if type
                         (if (symbolp type)
                             (symbol-name type)
                           (format "%s" type))
                       "unknown"))
           (content (plist-get struct :content))
           (inner-content
            (cond
             ;; Content is string - use directly
             ((stringp content) content)
             ;; Content is a list of structures - render recursively
             ((and (listp content) (not (null content)) (keywordp (car content)))
              (ai-common--render-struct-to-string content))
             ;; Content is a list of other things - render each separately
             ((and (listp content) (not (null content)))
              (mapconcat #'ai-common--render-struct-to-string
                         content ""))
             ;; Content is another nested structure
             ((and content (not (stringp content)))
              (ai-common--render-struct-to-string content))
             ;; No content
             (t "")))
           (attrs (cl-loop for (k v) on struct by #'cddr
                           unless (memq k '(:type :content))
                           collect (cons (substring (symbol-name k) 1)
                                         (ai-common--stringify v)))))

      ;; Output the tag with attributes and content
      (if (and (string-empty-p inner-content) (null attrs))
          ;; Self-closing tag if empty
          (format "<%s/>" tag-name)
        ;; Otherwise full tag with attributes and content
        (format "<%s%s>%s</%s>"
                tag-name
                (mapconcat (lambda (attr)
                            (format " %s=\"%s\"" (car attr) (cdr attr)))
                          attrs "")
                inner-content
                tag-name))))

   ;; List of structures
   ((listp struct)
    (mapconcat #'ai-common--render-struct-to-string struct ""))

   ;; Fallback for other types
   (t (format "%s" struct))))

(defun ai-common--render-context-entry (tag context-entry)
  "Convert CONTEXT-ENTRY into an XML string, wrapping it in TAG.
TAG is the name of the tag (a string or symbol), CONTEXT-ENTRY is a plist-structure
(such as returned by `ai-common--make-snippet-from-region` or
`ai-common--get-user-input`), containing all necessary fields."
  (when (and tag context-entry)
    (let* ((tag-symbol (if (symbolp tag) tag (intern tag)))
           ;; Make a copy of the plist to avoid modifying the original
           (entry-copy (copy-sequence context-entry))
           ;; Replace "type" with the required tag
           (_           (plist-put entry-copy :type tag-symbol))
           ;; Form the inner S-expression (TAG ((attr . val)…) CONTENT)
           (xml-element (ai-common--make-xml-element-from-plist-or-symbol entry-copy))
           ;; Serialize to string
           (xml-string  (ai-common--render-tag-to-string xml-element)))
      xml-string)))

(cl-defun ai-common--assemble-completion-context
    (&key
     (preceding-context-size  ai-utils--default-preceding-context-size)
     (following-context-size  ai-utils--default-following-context-size))
  "Return a list of context elements for <completion>.

Modes of operation:
1. If the region is active, return the selected region as `preceding-context` and `:cursor`.
2. If not active, return `preceding-context` and `following-context` with specified sizes.
3. If `preceding-context-size` or `following-context-size` are nil, use the beginning/end of file respectively."
  (cond
   ;; Mode 1: Selected region
   ((use-region-p)
    (list
     (ai-common--make-snippet-from-region 'preceding-context)
     :cursor))

   ;; Mode 2: Contexts of specified size
   ((and preceding-context-size following-context-size)
    (let* ((pre  (ai-common--make-preceding-context preceding-context-size))
           (post (ai-common--make-following-context following-context-size))
           (post-content (plist-get post :content)))
      (append
       (list pre :cursor)
       (when (and post-content
                  (> (length post-content) 0))
         (list post)))))

   ;; Modes 3 & 4: Contexts from start/to end of file
   (t
    (let* ((pre  (ai-common--make-preceding-context (- (point) (point-min))))
           (post (ai-common--make-following-context (- (point-max) (point))))
           (post-content (plist-get post :content)))
      (append
       (list pre :cursor)
       (when (and post-content
                  (> (length post-content) 0))
         (list post)))))))

(defun ai-common--assemble-edit-context ()
  "Return a list of context elements for an edit operation.

• If REGION is active → a single element with tag `<selection>…</selection>`.
• If REGION is not active → a single element with tag `<file-context>…</file-context>`."
  (if (use-region-p)
      ;; Highlighted fragment as <selection>
      (list
       (ai-common--make-snippet-from-region 'selection))
    ;; Otherwise, the whole buffer as <file-context>
    (list
     (ai-common--make-file-context))))

(defun ai-common--render-container-from-elements
    (container-tag elements &optional attrs)
  "Render <CONTAINER-TAG …> … </CONTAINER-TAG>.
ELEMENTS is a list of plists or symbols (:cursor).
ATTRS is an alist of the form '((\"attr\" . \"val\") …) for the container itself."
  (let* ((attrs-str
          (mapconcat (lambda (a)
                       (format " %s=\"%s\"" (car a) (cdr a)))
                     attrs
                     ""))
         (body
          (mapconcat
           (lambda (el)
             (ai-common--render-tag-to-string
              (ai-common--make-xml-element-from-plist-or-symbol el)))
           elements
           "")))
    (format "<%s%s>%s</%s>"
            container-tag attrs-str body container-tag)))

(defun ai-common--get-text-content-from-struct (typed-struct)
  "Extract the text content from a TYPED-STRUCT plist."
  (plist-get typed-struct :content))

(provide 'ai-common)

;;; ai-common.el ends here
