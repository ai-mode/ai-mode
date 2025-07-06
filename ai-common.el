;;; ai-common.el --- Common AI logic -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Alex (https://github.com/lispython)
;;
;; This file is part of ai-mode.
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
;; This file contains common logic for AI functionalities, focusing on
;; typed struct creation and manipulation, and rendering utilities.
;;
;; The main features include:
;; - Typed struct creation and manipulation
;; - XML-like rendering of data structures
;; - File context and snippet creation utilities
;;
;;; Code:

(require 'ai-utils)

(defcustom ai-common--render-ignore-fields '(:id :timestamp)
  "List of plist keys (fields) to ignore when rendering a typed struct to a string.
Fields specified here will not be included as attributes in the XML-like output."
  :type '(repeat (keyword :tag "Field to ignore"))
  :group 'ai-common)

(defcustom ai-common--render-content-only-types '(agent-instructions action-context)
  "List of struct types that should render only their content without XML tags.
Structs with these types will render as plain text content instead of
being wrapped in XML-like tags."
  :type '(repeat (symbol :tag "Struct type"))
  :group 'ai-common)

(defun ai-common--make-file-context-from-buffer (&optional buffer &rest additional-props)
  "Create a file-context structure from BUFFER.
If BUFFER is not provided, uses the current buffer.
This includes metadata such as file name, buffer name, and timestamps.
ADDITIONAL-PROPS are key-value pairs to be included in the resulting structure."
  (with-current-buffer (or buffer (current-buffer))
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (file-path (buffer-file-name))
          (file-mod-time (when (buffer-file-name)
                           (format-time-string "%Y-%m-%dT%H:%M:%S"
                                               (file-attribute-modification-time
                                                (file-attributes (buffer-file-name)))))))
      (apply #'ai-common--make-typed-struct
             content 'file-context 'current-buffer-content
             :file (or file-path (buffer-name))
             :buffer (buffer-name)
             :start-pos (point-min)
             :end-pos (point-max)
             :file-size (length content)
             (if file-mod-time
                 (append (list :file-modified file-mod-time) additional-props)
               additional-props)))))


(defun ai-common--make-file-context-from-file (file-path &optional type source &rest additional-props)
  "Create a file-context structure from FILE-PATH.
The file must exist and be readable.
TYPE specifies the structure type (defaults to 'file-context').
SOURCE indicates where the context originated (defaults to 'external-file-content').
ADDITIONAL-PROPS are key-value pairs to be included in the resulting structure."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))
  (unless (file-readable-p file-path)
    (error "File is not readable: %s" file-path))

  (let* ((content (with-temp-buffer
                    (insert-file-contents file-path)
                    (buffer-substring-no-properties (point-min) (point-max))))
         (struct-type (or type 'file-context))
         (struct-source (or source 'external-file-content))
         (file-attrs (file-attributes file-path))
         (file-mod-time (format-time-string "%Y-%m-%dT%H:%M:%S"
                                            (file-attribute-modification-time file-attrs))))
    (apply #'ai-common--make-typed-struct
           content struct-type struct-source
           :file (expand-file-name file-path)
           :buffer (file-name-nondirectory file-path)
           :start-pos 1
           :end-pos (1+ (length content))
           :file-size (length content)
           :file-modified file-mod-time
           additional-props)))

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
           (content    (buffer-substring-no-properties start end))
           (file-mod-time (when (buffer-file-name)
                            (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                (file-attribute-modification-time
                                                 (file-attributes (buffer-file-name)))))))
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
        :source            region-selection
        :id                ,id
        ,@(when file-mod-time (list :file-modified file-mod-time))))))

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
         (col-beg    (car (posn-col-row (posn-at-point beg))))
         (col-end    (car (posn-col-row (posn-at-point end))))
         (mode         (symbol-name major-mode))
         (ts           (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id           (ai-common--generate-id 'preceding-context))
         (content      (buffer-substring-no-properties beg end))
         (file-mod-time (when (buffer-file-name)
                          (format-time-string "%Y-%m-%dT%H:%M:%S"
                                              (file-attribute-modification-time
                                               (file-attributes (buffer-file-name)))))))
    `(:type              preceding-context
      :content           ,content
      :file              ,file
      :buffer            ,buf
      :start-pos         ,beg
      :end-pos           ,end
      :start-line        ,start-line
      :end-line          ,end-line
      :start-column      ,col-beg
      :end-column        ,col-end
      :mode              ,mode
      :timestamp         ,ts
      :source            preceding-context
      :id                ,id
      ,@(when file-mod-time (list :file-modified file-mod-time)))))

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
         (content      (buffer-substring-no-properties beg end))
         (file-mod-time (when (buffer-file-name)
                          (format-time-string "%Y-%m-%dT%H:%M:%S"
                                              (file-attribute-modification-time
                                               (file-attributes (buffer-file-name)))))))
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
      :source            following-context
      :id                ,id
      ,@(when file-mod-time (list :file-modified file-mod-time)))))


(defun ai-common--make-action-object (action-type elements &optional source &rest additional-props)
  "Create a typed action-object structure for AI operations.
ACTION-TYPE is a symbol or string specifying the action type (e.g., 'complete', 'modify').
If ACTION-TYPE is nil or empty, defaults to 'action-object'.
ELEMENTS is a list of context elements to include in the action object.
SOURCE indicates where the action object originated (optional).
ADDITIONAL-PROPS are key-value pairs to be included in the resulting structure.

Returns a plist with :type set to ACTION-TYPE-action-object format."
  (let* ((action-type-str (cond
                           ((null action-type) nil)
                           ((and (stringp action-type) (string-empty-p action-type)) nil)
                           ((symbolp action-type) (symbol-name action-type))
                           (t action-type)))
         (object-type (if action-type-str
                          (intern (format "%s-action-object" action-type-str))
                        'action-object))
         (source-val (or source 'contextual-action)))
    (apply #'ai-common--make-typed-struct
           elements
           object-type
           source-val
           :render-ignore-fields '(:source)
           additional-props)))


(defun ai-common--make-file-summary-struct (content &optional file-path project-root &rest additional-props)
  "Create a typed struct for storing an AI summary of a file.
CONTENT is the summary text (string).
FILE-PATH is the path to the summarized file (optional).
PROJECT-ROOT is the project root directory (optional).
ADDITIONAL-PROPS are key-value pairs to be included in the resulting struct.
The struct type will be 'file-summary and source 'ai-summary.
This structure is intended for use in file indexing and summarization."
  (let* ((base-props `(:type file-summary :source ai-summary))
         (file-mod-time (when (and file-path (file-exists-p file-path))
                          (format-time-string "%Y-%m-%dT%H:%M:%S"
                                              (file-attribute-modification-time
                                               (file-attributes file-path)))))
         (file-specific-props (when file-path
                                `(:file ,(expand-file-name file-path)
                                  :buffer ,(file-name-nondirectory file-path)
                                  ,@(when project-root
                                      (list :relative-path (file-relative-name file-path project-root)))
                                  ,@(when file-mod-time (list :file-modified file-mod-time))))))
    (apply #'ai-common--make-typed-struct
           content
           'file-summary
           'ai-summary
           (append file-specific-props additional-props))))

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
           (struct-ignore-fields (plist-get item :render-ignore-fields))
           (all-ignore-fields (append ai-common--render-ignore-fields
                                      struct-ignore-fields
                                      '(:type :content :rendered :render-ignore-fields)))
           (attrs    (cl-loop for (k v) on item by #'cddr
                              unless (memq k all-ignore-fields)
                              collect (cons
                                       (substring (symbol-name k) 1)
                                       (ai-common--stringify v)))))
      (list tag-name attrs content))))

(defun ai-common--is-grouped-struct-p (struct)
  "Check if STRUCT is a grouped structure created by grouping function.
Returns t if the structure has the :grouped flag set to t, nil otherwise."
  (and (listp struct) (keywordp (car struct)) (eq (plist-get struct :grouped) t)))

(defun ai-common--render-struct-to-string (struct)
  "Render a typed structure STRUCT to an XML-like string.
STRUCT can be a plist, a symbol or a list of other structures.
For plists, tag name is determined by :type property.
Handles nested structures recursively.
For grouped structures (those with :grouped property set to t),
only renders the child elements without the wrapper container."
  (cond
   ;; Handle nil and t specifically as per user request to return empty string
   ((or (eq struct nil) (eq struct t)) "")

   ;; If the structure has a :rendered property and it's non-nil, return its content directly
   ((and (listp struct) (plist-get struct :rendered))
    (plist-get struct :content))

   ;; Handle grouped structures - render only child elements without container
   ((ai-common--is-grouped-struct-p struct)
    (let ((children (plist-get struct :content)))
      (cond
       ((and (listp children) (not (null children)))
        (mapconcat #'ai-common--render-struct-to-string children ""))
       (t ""))))

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
           (content (plist-get struct :content)))

      ;; Check if this type should render content only
      (if (memq type ai-common--render-content-only-types)
          ;; Render only content without XML tags
          (cond
           ((stringp content) content)
           ((and (listp content) (not (null content)) (keywordp (car content)))
            (ai-common--render-struct-to-string content))
           ((and (listp content) (not (null content)))
            (mapconcat #'ai-common--render-struct-to-string content ""))
           ((and content (not (stringp content)))
            (ai-common--render-struct-to-string content))
           (t ""))

        ;; Normal XML rendering
        (let* ((inner-content
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
               (struct-ignore-fields (plist-get struct :render-ignore-fields))
               (all-ignore-fields (append ai-common--render-ignore-fields
                                          struct-ignore-fields
                                          '(:type :content :rendered :render-ignore-fields)))
               (attrs (cl-loop for (k v) on struct by #'cddr
                               unless (memq k all-ignore-fields)
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
                    tag-name))))))

   ;; List of structures
   ((listp struct)
    (mapconcat #'ai-common--render-struct-to-string struct ""))

   ;; Fallback for other types
   (t (format "%s" struct))))

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
             (ai-common--render-struct-to-string el))
           elements
           "")))
    (format "<%s%s>%s</%s>"
            container-tag attrs-str body container-tag)))

(defun ai-common--get-text-content-from-struct (typed-struct)
  "Extract the text content from a TYPED-STRUCT plist, stripping any text properties.
If the content is not a string, it is returned as is."
  (let ((content (plist-get typed-struct :content)))
    (if (stringp content)
        (substring-no-properties content)
      content)))


(defun ai-common--update-typed-struct (typed-struct &rest updates)
  "Create a new typed struct based on TYPED-STRUCT with UPDATES applied.
UPDATES should be key-value pairs that will override or add properties.
Returns a new plist with the updated values."
  (let ((new-struct (copy-sequence typed-struct)))
    (while updates
      (let ((key (car updates))
            (value (cadr updates)))
        (setq new-struct (plist-put new-struct key value))
        (setq updates (cddr updates))))
    new-struct))


(provide 'ai-common)

;;; ai-common.el ends here
