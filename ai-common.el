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
;; - JSON serialization and deserialization for plist structures
;;
;;; Code:

(require 'ai-utils)
(require 'json)

(defcustom ai-common--render-ignore-fields '(:id :timestamp)
  "List of plist keys (fields) to ignore when rendering a typed struct to a string.
Fields specified here will not be included as attributes in the XML-like output."
  :type '(repeat (keyword :tag "Field to ignore"))
  :group 'ai-common)

(defcustom ai-common--render-content-only-types '(agent-instructions action-context assistant-response)
  "List of struct types that should render only their content without XML tags.
Structs with these types will render as plain text content instead of
being wrapped in XML-like tags."
  :type '(repeat (symbol :tag "Struct type"))
  :group 'ai-common)

(defun ai-common--plist-to-json (plist)
  "Convert PLIST to JSON string with plist keys as JSON object keys.
Keyword keys are converted to strings by removing the colon prefix.
Nested plists are recursively converted to JSON objects.
Returns a JSON string representation of the plist."
  (let ((json-object (make-hash-table :test 'equal)))
    ;; Convert plist to hash table
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (when (keywordp key)
          (let* ((key-str (substring (symbol-name key) 1))
                 (json-value (cond
                              ;; Recursively convert nested plists
                              ((and (listp value) (keywordp (car value)))
                               (ai-common--plist-to-json value))
                              ;; Convert lists of plists
                              ((and (listp value) (listp (car value)) (keywordp (caar value)))
                               (mapcar #'ai-common--plist-to-json value))
                              ;; Convert symbols to strings
                              ((symbolp value) (symbol-name value))
                              ;; Keep other values as is
                              (t value))))
            (puthash key-str json-value json-object)))
        (setq plist (cddr plist))))
    ;; Convert hash table to JSON string
    (json-encode json-object)))

(defun ai-common--json-to-plist (json-string)
  "Convert JSON string to plist with JSON object keys as keyword keys.
String keys are converted to keywords by adding colon prefix.
Nested JSON objects are recursively converted to plists.
Returns a plist representation of the JSON object."
  (let ((json-object (json-read-from-string json-string)))
    (ai-common--json-object-to-plist json-object)))

(defun ai-common--json-object-to-plist (json-object)
  "Convert JSON-OBJECT (hash table or alist) to plist.
Helper function for `ai-common--json-to-plist'."
  (cond
   ;; Handle hash tables
   ((hash-table-p json-object)
    (let ((plist '()))
      (maphash (lambda (key value)
                 (let ((keyword-key (intern (concat ":" key)))
                       (plist-value (cond
                                     ;; Recursively convert nested objects
                                     ((hash-table-p value)
                                      (ai-common--json-object-to-plist value))
                                     ;; Convert arrays of objects
                                     ((and (vectorp value)
                                           (> (length value) 0)
                                           (hash-table-p (aref value 0)))
                                      (mapcar #'ai-common--json-object-to-plist
                                              (append value nil)))
                                     ;; Keep other values as is
                                     (t value))))
                   (setq plist (append plist (list keyword-key plist-value)))))
               json-object)
      plist))

   ;; Handle alists
   ((and (listp json-object) (consp (car json-object)))
    (let ((plist '()))
      (dolist (pair json-object)
        (let ((keyword-key (intern (concat ":" (symbol-name (car pair)))))
              (plist-value (cond
                            ;; Recursively convert nested objects
                            ((and (listp (cdr pair)) (consp (cadr pair)))
                             (ai-common--json-object-to-plist (cdr pair)))
                            ;; Convert lists of objects
                            ((and (listp (cdr pair))
                                  (listp (cadr pair))
                                  (consp (caadr pair)))
                             (mapcar #'ai-common--json-object-to-plist (cdr pair)))
                            ;; Keep other values as is
                            (t (cdr pair)))))
          (setq plist (append plist (list keyword-key plist-value)))))
      plist))

   ;; Return as is for non-objects
   (t json-object)))

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


(defun ai-common--generate-request-id ()
  "Generate a unique request ID based on timestamp and hash."
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (hash (substring (secure-hash 'md5 (format "%s_%s" timestamp (random))) 0 8)))
    (format "%s_req_%s" timestamp hash)))

(defun ai-common--is-typed-struct-p (value)
  "Check if VALUE is a plist representing a typed struct."
  (and (listp value)
       (keywordp (car value))
       (plist-get value :type)
       (plist-member value :content))) ; Use plist-member for robustness

(defun ai-common--get-variable-type (variable)
  "Determine the type of VARIABLE and return it as a symbol.
Returns one of: 'string, 'number, 'symbol, 'keyword, 'list, 'plist, 'alist, 'vector, 'hash-table, 'function, 'buffer, 'window, 'process, 'marker, 'overlay, 'boolean, 'null, 'unknown."
  (cond
   ((null variable) 'null)
   ((eq variable t) 'boolean)
   ((stringp variable) 'string)
   ((numberp variable) 'number)
   ((keywordp variable) 'keyword)
   ((symbolp variable) 'symbol)
   ((and (listp variable) (keywordp (car variable))) 'plist)
   ((and (listp variable) (consp (car variable))) 'alist)
   ((listp variable) 'list)
   ((vectorp variable) 'vector)
   ((hash-table-p variable) 'hash-table)
   ((functionp variable) 'function)
   ((bufferp variable) 'buffer)
   ((windowp variable) 'window)
   ((processp variable) 'process)
   ((markerp variable) 'marker)
   ((overlayp variable) 'overlay)
   (t 'unknown)))

(defun ai-common--get-detailed-variable-type (variable)
  "Get detailed type information for VARIABLE.
Returns a plist with :type and additional type-specific properties."
  (let ((basic-type (ai-common--get-variable-type variable)))
    (case basic-type
      ('plist
       `(:type plist :length ,(length variable)))
      ('alist
       `(:type alist :length ,(length variable)))
      ('list
       (cond
        ((null variable) `(:type null))
        (t `(:type list :length ,(length variable)))))
      ('string
       `(:type string :length ,(length variable)))
      ('vector
       `(:type vector :length ,(length variable)))
      ('hash-table
       `(:type hash-table :size ,(hash-table-size variable) :count ,(hash-table-count variable)))
      ('symbol
       `(:type symbol :bound ,(boundp variable) :function ,(fboundp variable)))
      (t `(:type ,basic-type)))))


(defun ai-common--convert-typed-struct-to-alist (typed-struct)
  "Convert a typed struct to an alist, handling nested structures appropriately.
This function provides specialized conversion for known typed struct types
and falls back to generic conversion for unknown types."
  (let ((struct-type (plist-get typed-struct :type))
        (content (plist-get typed-struct :content)))
    (cond
     ;; Handle file-context structures
     ((eq struct-type 'file-context)
      (ai-common--convert-file-context-to-alist typed-struct))

     ;; Handle snippet structures
     ((memq struct-type '(snippet selection preceding-context following-context))
      (ai-common--convert-snippet-to-alist typed-struct))

     ;; Handle action-object structures
     ((string-suffix-p "-action-object" (symbol-name struct-type))
      (ai-common--convert-action-object-to-alist typed-struct))

     ;; Handle memory structures
     ((eq struct-type 'memory)
      (ai-common--convert-memory-to-alist typed-struct))

     ;; Handle project-context structures
     ((eq struct-type 'project-context)
      (ai-common--convert-project-context-to-alist typed-struct))

     ;; Handle file-summary structures
     ((eq struct-type 'file-summary)
      (ai-common--convert-file-summary-to-alist typed-struct))

     ;; Handle agent-instructions structures
     ((eq struct-type 'agent-instructions)
      (ai-common--convert-agent-instructions-to-alist typed-struct))

     ;; Handle user-input structures
     ((eq struct-type 'user-input)
      (ai-common--convert-user-input-to-alist typed-struct))

     ;; Handle additional-context structures
     ((eq struct-type 'additional-context)
      (ai-common--convert-additional-context-to-alist typed-struct))

     ;; Handle assistant-response structures
     ((eq struct-type 'assistant-response)
      (ai-common--convert-assistant-response-to-alist typed-struct))

     ;; Handle error structures
     ((eq struct-type 'error)
      (ai-common--convert-error-to-alist typed-struct))

     ;; Generic fallback for unknown types
     (t
      (ai-common--convert-generic-typed-struct-to-alist typed-struct)))))

(defun ai-common--convert-file-context-to-alist (file-context)
  "Convert a file-context typed struct to an alist."
  (let ((content (plist-get file-context :content))
        (file (plist-get file-context :file))
        (buffer (plist-get file-context :buffer))
        (file-size (plist-get file-context :file-size))
        (file-modified (plist-get file-context :file-modified))
        (start-pos (plist-get file-context :start-pos))
        (end-pos (plist-get file-context :end-pos)))
    `((type . file-context)
      (content . ,content)
      (file . ,file)
      (buffer . ,buffer)
      ,@(when file-size `((file-size . ,file-size)))
      ,@(when file-modified `((file-modified . ,file-modified)))
      ,@(when start-pos `((start-pos . ,start-pos)))
      ,@(when end-pos `((end-pos . ,end-pos))))))

(defun ai-common--convert-snippet-to-alist (snippet)
  "Convert a snippet-type typed struct to an alist."
  (let ((type (plist-get snippet :type))
        (content (plist-get snippet :content))
        (file (plist-get snippet :file))
        (buffer (plist-get snippet :buffer))
        (start-pos (plist-get snippet :start-pos))
        (end-pos (plist-get snippet :end-pos))
        (start-line (plist-get snippet :start-line))
        (end-line (plist-get snippet :end-line))
        (start-column (plist-get snippet :start-column))
        (end-column (plist-get snippet :end-column))
        (mode (plist-get snippet :mode))
        (file-modified (plist-get snippet :file-modified)))
    `((type . ,type)
      (content . ,content)
      (file . ,file)
      (buffer . ,buffer)
      (start-pos . ,start-pos)
      (end-pos . ,end-pos)
      (start-line . ,start-line)
      (end-line . ,end-line)
      (start-column . ,start-column)
      (end-column . ,end-column)
      (mode . ,mode)
      ,@(when file-modified `((file-modified . ,file-modified))))))

(defun ai-common--convert-action-object-to-alist (action-object)
  "Convert an action-object typed struct to an alist."
  (let ((type (plist-get action-object :type))
        (content (plist-get action-object :content)))
    `((type . ,type)
      (content . ,(if (listp content)
                      (mapcar #'ai-common--convert-context-element-to-alist content)
                    content)))))

(defun ai-common--convert-memory-to-alist (memory)
  "Convert a memory typed struct to an alist."
  (let ((content (plist-get memory :content)))
    `((type . memory)
      (content . ,(if (listp content)
                      (mapcar #'ai-common--convert-typed-struct-to-alist content)
                    content)))))

(defun ai-common--convert-project-context-to-alist (project-context)
  "Convert a project-context typed struct to an alist."
  (let ((content (plist-get project-context :content))
        (root (plist-get project-context :root)))
    `((type . project-context)
      (content . ,(if (listp content)
                      (mapcar #'ai-common--convert-typed-struct-to-alist content)
                    content))
      ,@(when root `((root . ,root))))))

(defun ai-common--convert-file-summary-to-alist (file-summary)
  "Convert a file-summary typed struct to an alist."
  (let ((content (plist-get file-summary :content))
        (file (plist-get file-summary :file))
        (buffer (plist-get file-summary :buffer))
        (relative-path (plist-get file-summary :relative-path))
        (file-size (plist-get file-summary :file-size))
        (file-modified (plist-get file-summary :file-modified)))
    `((type . file-summary)
      (content . ,content)
      ,@(when file `((file . ,file)))
      ,@(when buffer `((buffer . ,buffer)))
      ,@(when relative-path `((relative-path . ,relative-path)))
      ,@(when file-size `((file-size . ,file-size)))
      ,@(when file-modified `((file-modified . ,file-modified))))))

(defun ai-common--convert-agent-instructions-to-alist (agent-instructions)
  "Convert an agent-instructions typed struct to an alist."
  (let ((content (plist-get agent-instructions :content))
        (group (plist-get agent-instructions :group)))
    `((type . agent-instructions)
      (content . ,content)
      ,@(when group `((group . ,group))))))

(defun ai-common--convert-user-input-to-alist (user-input)
  "Convert a user-input typed struct to an alist."
  (let ((content (plist-get user-input :content)))
    `((type . user-input)
      (content . ,content))))

(defun ai-common--convert-additional-context-to-alist (additional-context)
  "Convert an additional-context typed struct to an alist."
  (let ((content (plist-get additional-context :content)))
    `((type . additional-context)
      (content . ,(if (listp content)
                      (mapcar #'ai-common--convert-typed-struct-to-alist content)
                    content)))))

(defun ai-common--convert-assistant-response-to-alist (assistant-response)
  "Convert an assistant-response typed struct to an alist."
  (let ((content (plist-get assistant-response :content)))
    `((type . assistant-response)
      (content . ,content))))

(defun ai-common--convert-error-to-alist (error-struct)
  "Convert an error typed struct to an alist."
  (let ((content (plist-get error-struct :content))
        (error-type (plist-get error-struct :error-type))
        (code (plist-get error-struct :code)))
    `((type . error)
      (content . ,content)
      ,@(when error-type `((error-type . ,error-type)))
      ,@(when code `((code . ,code))))))

(defun ai-common--convert-generic-typed-struct-to-alist (typed-struct)
  "Convert a generic typed struct to an alist, preserving all properties."
  (let ((result '())
        (remaining typed-struct))
    (while remaining
      (let ((key (car remaining))
            (value (cadr remaining)))
        (when (keywordp key)
          (let ((alist-key (intern (substring (symbol-name key) 1)))
                (alist-value (cond
                              ;; Handle nested typed structs
                              ((ai-common--is-typed-struct-p value)
                               (ai-common--convert-typed-struct-to-alist value))
                              ;; Handle lists of typed structs
                              ((and (listp value) (ai-common--is-typed-struct-p (car value)))
                               (mapcar #'ai-common--convert-typed-struct-to-alist value))
                              ;; Handle other lists
                              ((listp value)
                               (ai-common--plist-to-alist value))
                              ;; Handle atomic values
                              (t value))))
            (push (cons alist-key alist-value) result)))
        (setq remaining (cddr remaining))))
    (nreverse result)))

(defun ai-common--convert-context-element-to-alist (element)
  "Convert a context element to an alist.
ELEMENT can be a typed struct, a symbol, or other data structure."
  (cond
   ;; Handle symbols (like :cursor)
   ((symbolp element)
    `((type . ,(if (keywordp element)
                   (intern (substring (symbol-name element) 1))
                 element))))

   ;; Handle typed structs
   ((ai-common--is-typed-struct-p element)
    (ai-common--convert-typed-struct-to-alist element))

   ;; Handle other data structures
   (t (ai-common--plist-to-alist element))))


(defun ai-common--plist-to-alist (plist)
  "Convert PLIST to an alist.
Keyword keys are converted to symbols by removing the colon prefix.
Nested plists are recursively converted to alists.
Typed structs encountered as values are converted using specialized conversion functions.
Returns an alist representation of the plist."
  (cond
   ;; Handle nil
   ((null plist) nil)

   ;; Handle alist (already in alist format)
   ((and (listp plist) (consp (car plist)) (not (keywordp (car plist))))
    ;; This is already an alist, recursively process its values
    (mapcar (lambda (pair)
              (cons (car pair) (ai-common--plist-to-alist (cdr pair))))
            plist))

   ;; Handle typed structs
   ((ai-common--is-typed-struct-p plist)
    (ai-common--convert-typed-struct-to-alist plist))

   ;; Handle plist (keyword property list)
   ((and (listp plist) (keywordp (car plist)))
    (let ((result '())
          (remaining plist))
      (while remaining
        (let ((key (car remaining))
              (value (cadr remaining)))
          (when (keywordp key)
            (let ((alist-key (intern (substring (symbol-name key) 1)))
                  (alist-value (cond
                                ;; Handle typed structs
                                ((ai-common--is-typed-struct-p value)
                                 (ai-common--convert-typed-struct-to-alist value))
                                ;; Handle lists of typed structs
                                ((and (listp value) (not (null value)) (ai-common--is-typed-struct-p (car value)))
                                 (mapcar #'ai-common--convert-typed-struct-to-alist value))
                                ;; Handle other values recursively
                                (t (ai-common--plist-to-alist value)))))
              (push (cons alist-key alist-value) result)))
          (setq remaining (cddr remaining))))
      (nreverse result)))

   ;; Handle list of items
   ((listp plist)
    (mapcar #'ai-common--plist-to-alist plist))

   ;; Handle atomic values
   (t plist)))


(provide 'ai-common)

;;; ai-common.el ends here
