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
(require 'projectile nil 'noerror) ; Ensure projectile is loaded for project root detection.

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

(defcustom ai-common-ignore-file-name ".ai-ignore"
  "Name of the AI ignore file, typically in the project root."
  :type 'string
  :group 'ai-common)

(defcustom ai-common-global-ignore-patterns
  '(".git/"
    ".svn/"
    ".hg/"
    ".bzr/"
    "_darcs/"
    "CVS/"
    "node_modules/"
    ".npm/"
    ".yarn/"
    "bower_components/"
    "__pycache__/"
    "*.pyc"
    "*.pyo"
    "*.pyd"
    ".venv/"
    "venv/"
    "env/"
    ".env/"
    "target/"
    "build/"
    "dist/"
    ".gradle/"
    ".idea/"
    ".vscode/"
    "*.log"
    "*.tmp"
    "*.temp"
    "*.cache"
    ".DS_Store"
    "Thumbs.db"
    "*.swp"
    "*.swo"
    "*~"
    ".#*"
    "#*#")
  "Global hardcoded ignore patterns that are always applied.
These patterns are used to prevent sensitive or irrelevant files
from being accidentally included in AI context in any project.
Includes common version control directories, build artifacts,
temporary files, and IDE-specific files."
  :type '(repeat string)
  :group 'ai-common)

(defcustom ai-common-global-ignore-files
  (list (expand-file-name "~/.global-gitignore")
        (expand-file-name "~/.global-ai-ignore"))
  "List of global ignore files to be processed.
These files contain patterns that apply to all projects."
  :type '(repeat file)
  :group 'ai-common)

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

(defun ai-common--add-to-global-memory (input)
  "Add INPUT to `ai--global-memo-context'.
INPUT is a string representing the context or information to be remembered globally."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter instruction: "))))
  (let ((struct (ai-common--make-typed-struct input 'global-memory-item 'user-input)))
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
    (let ((struct (ai-common--make-typed-struct input 'buffer-bound-prompt 'user-input)))
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
    (let ((struct (ai-common--make-typed-struct input 'global-system-prompt 'user-input)))
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

(defun ai-common--make-file-context-from-buffer (&optional buffer &rest additional-props)
  "Create a file-context structure from BUFFER.
If BUFFER is not provided, uses the current buffer.
This includes metadata such as file name, buffer name, and timestamps.
ADDITIONAL-PROPS are key-value pairs to be included in the resulting structure."
  (with-current-buffer (or buffer (current-buffer))
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (apply #'ai-common--make-typed-struct
             content 'file-context 'current-buffer-content
             :file (or (buffer-file-name) (buffer-name))
             :buffer (buffer-name)
             :start-pos (point-min)
             :end-pos (point-max)
             :file-size (length content)
             additional-props))))


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
         (struct-source (or source 'external-file-content)))
    (apply #'ai-common--make-typed-struct
           content struct-type struct-source
           :file (expand-file-name file-path)
           :buffer (file-name-nondirectory file-path)
           :start-pos 1
           :end-pos (1+ (length content))
           :file-size (length content)
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
                           :source            region-selection
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
    (ai-common--make-typed-struct text 'user-input 'user-input
                                  :render-ignore-fields '(:source))))

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
         (content      (buffer-substring-no-properties beg end)))
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
      :source            following-context
      :id                ,id)))


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


(defun ai-common--make-file-summary-struct (content &optional file-path &rest additional-props)
  "Create a typed struct for storing an AI summary of a file.
CONTENT is the summary text (string).
FILE-PATH is the path to the summarized file (optional).
ADDITIONAL-PROPS are key-value pairs to be included in the resulting struct.
The struct type will be 'file-summary and source 'ai-summary.
This structure is intended for use in file indexing and summarization."
  (let* ((base-props `(:type file-summary :source ai-summary))
         (file-specific-props (when file-path
                                `(:file ,(expand-file-name file-path)
                                  :buffer ,(file-name-nondirectory file-path)
                                  :relative-path ,(file-relative-name file-path (ai-common--get-project-root))))))
    (apply #'ai-common--make-typed-struct
           content
           'file-summary
           'ai-summary
           (append file-specific-props additional-props))))

(defun ai-common--add-to-context-pool (item)
  "Add ITEM (plist-structure) to `ai-common--context-pool`."
  (push item ai-common--context-pool))

(defun ai-common--format-context-pool-item-for-selection (item)
  "Format a context pool ITEM for display in completing-read.
Returns a string representation of the item for user selection."
  (let* ((type (plist-get item :type))
         (source (plist-get item :source))
         (id (plist-get item :id))
         (content (plist-get item :content))
         (file (plist-get item :file))
         (preview (if (stringp content)
                      (substring content 0 (min 50 (length content)))
                    ""))
         (preview (replace-regexp-in-string "\n" " " preview)))
    (format "%s | %s | %s%s%s"
            (or type "unknown")
            (or source "unknown")
            (if file (format "%s | " (file-name-nondirectory file)) "")
            preview
            (if (> (length content) 50) "..." ""))))

(defun ai-common--remove-from-context-pool ()
  "Remove selected item from context pool via minibuffer selection."
  (interactive)
  (if (null ai-common--context-pool)
      (message "Context pool is empty")
    (let* ((items-with-display (mapcar (lambda (item)
                                         (cons (ai-common--format-context-pool-item-for-selection item) item))
                                       ai-common--context-pool))
           (selected-display (completing-read "Remove from context pool: "
                                              (mapcar #'car items-with-display)
                                              nil t))
           (selected-item (cdr (assoc selected-display items-with-display))))
      (when selected-item
        (setq ai-common--context-pool
              (cl-remove selected-item ai-common--context-pool :test #'equal))
        (message "Removed item from context pool: %s"
                 (ai-common--format-context-pool-item-for-selection selected-item))))))

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
  (let ((file-context (ai-common--make-file-context-from-buffer)))
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


(defun ai-common--render-struct-to-string (struct)
  "Render a typed structure STRUCT to an XML-like string.
STRUCT can be a plist, a symbol or a list of other structures.
For plists, tag name is determined by :type property.
Handles nested structures recursively."
  (cond
   ;; Handle nil and t specifically as per user request to return empty string
   ((or (eq struct nil) (eq struct t)) "")

   ;; If the structure has a :rendered property and it's non-nil, return its content directly
   ((and (listp struct) (plist-get struct :rendered))
    (plist-get struct :content))

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
     (ai-common--make-file-context-from-buffer))))

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

;; Project file filtering methods

(defun ai-common--get-project-root ()
  "Return the project root directory using Projectile, or nil if not in a project.
Requires Projectile to be loaded and active."
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(defun ai-common--read-ignore-patterns-from-file (file-path)
  "Read patterns from FILE-PATH.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file.
Lines starting with '#' are comments and are ignored. Empty lines are also ignored."
  (when (file-exists-p file-path)
    (let ((patterns nil))
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring (point) (line-end-position)))
                 (trimmed-line (string-trim line)))
            (unless (or (string-empty-p trimmed-line)
                        (string-prefix-p "#" trimmed-line))
              (let* ((is-negated (string-prefix-p "!" trimmed-line))
                     (pattern (if is-negated (substring trimmed-line 1) trimmed-line)))
                (push (cons pattern is-negated) patterns)))
          (forward-line 1)))
      (nreverse patterns)))))

(defun ai-common--read-ai-ignore-patterns (project-root)
  "Read patterns from PROJECT-ROOT/.ai-ignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file.
Lines starting with '#' are comments and are ignored. Empty lines are also ignored."
  (let ((ignore-file (expand-file-name ai-common-ignore-file-name project-root)))
    (ai-common--read-ignore-patterns-from-file ignore-file)))

(defun ai-common--get-global-hardcoded-patterns ()
  "Get global hardcoded ignore patterns.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
These patterns are never negated and come from `ai-common-global-ignore-patterns`."
  (mapcar (lambda (pattern) (cons pattern nil)) ai-common-global-ignore-patterns))

(defun ai-common--get-global-ignore-file-patterns ()
  "Get patterns from global ignore files.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Reads patterns from files specified in `ai-common-global-ignore-files`."
  (let ((all-patterns nil))
    (dolist (global-file ai-common-global-ignore-files)
      (when (file-exists-p global-file)
        (setq all-patterns (append all-patterns
                                   (ai-common--read-ignore-patterns-from-file global-file)))))
    all-patterns))

(defun ai-common--get-project-gitignore-patterns (project-root)
  "Get patterns from PROJECT-ROOT/.gitignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file."
  (let ((gitignore-file (expand-file-name ".gitignore" project-root)))
    (when (file-exists-p gitignore-file)
      (ai-common--read-ignore-patterns-from-file gitignore-file))))

(defun ai-common--get-project-ai-ignore-patterns (project-root)
  "Get patterns from PROJECT-ROOT/.ai-ignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file."
  (ai-common--read-ai-ignore-patterns project-root))

(defun ai-common--get-all-ignore-patterns (project-root)
  "Get all ignore patterns from various sources.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Sources include:
1. Global hardcoded patterns from `ai-common-global-ignore-patterns`
2. Global ignore files from `ai-common-global-ignore-files`
3. Project .gitignore file
4. Project .ai-ignore file

Patterns are processed in this order, with later patterns potentially
overriding earlier ones via negation."
  (let ((all-patterns nil))

    ;; 1. Add global hardcoded patterns
    (setq all-patterns (append all-patterns (ai-common--get-global-hardcoded-patterns)))

    ;; 2. Add patterns from global ignore files
    (setq all-patterns (append all-patterns (ai-common--get-global-ignore-file-patterns)))

    ;; 3. Add patterns from project .gitignore
    (when-let ((gitignore-patterns (ai-common--get-project-gitignore-patterns project-root)))
      (setq all-patterns (append all-patterns gitignore-patterns)))

    ;; 4. Add patterns from project .ai-ignore (highest priority)
    (when-let ((ai-ignore-patterns (ai-common--get-project-ai-ignore-patterns project-root)))
      (setq all-patterns (append all-patterns ai-ignore-patterns)))

    all-patterns))

(defun ai-common--pattern-to-regexp (pattern)
  "Convert a gitignore-style PATTERN to an Emacs Lisp regexp string.
Handles '*' (wildcard), '**' (recursive wildcard), '/' (directory separators), and anchoring rules."
  (let* ((has-slash (string-match-p "/" pattern))
         (leading-slash (string-prefix-p "/" pattern))
         (trailing-slash (string-suffix-p "/" pattern))
         (clean-pattern (if leading-slash (substring pattern 1) pattern))
         (clean-pattern (if trailing-slash (substring clean-pattern 0 -1) clean-pattern)))

    ;; Handle special cases first
    (cond
     ;; Simple filename pattern without slashes (e.g., ".DS_Store", "*.txt")
     ((not has-slash)
      ;; Replace wildcards with placeholders before escaping
      (let ((with-placeholders (replace-regexp-in-string "\\*\\*" "DOUBLE_STAR" clean-pattern t))
            (escaped nil))
        (setq with-placeholders (replace-regexp-in-string "\\*" "SINGLE_STAR" with-placeholders t))
        (setq escaped (regexp-quote with-placeholders))
        ;; Restore wildcards as regexp patterns
        (setq escaped (replace-regexp-in-string "DOUBLE_STAR" ".*" escaped t))
        (setq escaped (replace-regexp-in-string "SINGLE_STAR" "[^/]*" escaped t))
        (concat "\\(?:^\\|/\\)" escaped "$")))

     ;; Directory wildcard pattern (e.g., "dir/*", "dir/**")
     ((and has-slash (string-match-p "\\*" pattern))
      ;; Replace wildcards with placeholders before escaping
      (let ((with-placeholders (replace-regexp-in-string "\\*\\*" "DOUBLE_STAR" clean-pattern t))
            (escaped nil))
        (setq with-placeholders (replace-regexp-in-string "\\*" "SINGLE_STAR" with-placeholders t))
        (setq escaped (regexp-quote with-placeholders))
        ;; Restore wildcards as regexp patterns
        (setq escaped (replace-regexp-in-string "DOUBLE_STAR" ".*" escaped t))
        (setq escaped (replace-regexp-in-string "SINGLE_STAR" "[^/]*" escaped t))
        (if leading-slash
            (concat "^" escaped (if trailing-slash "\\(?:/.*\\)?$" "$"))
          (concat "\\(?:^\\|/\\)" escaped (if trailing-slash "\\(?:/.*\\)?$" "$")))))

     ;; Exact path pattern with slashes but no wildcards
     (t
      (let ((escaped (regexp-quote clean-pattern)))
        (if leading-slash
            (concat "^" escaped (if trailing-slash "\\(?:/.*\\)?$" "$"))
          (concat "\\(?:^\\|/\\)" escaped (if trailing-slash "\\(?:/.*\\)?$" "$"))))))))


(defun ai-common--get-all-project-paths (project-root)
  "Recursively list all files and directories in PROJECT-ROOT, returning relative paths.
  Directories will have a trailing slash."
  (let ((all-paths nil))
    (unless (file-directory-p project-root)
      (error "Project root '%s' is not a directory." project-root))
    (letrec ((traverse (lambda (dir relative-dir-path)
                         (dolist (entry (directory-files dir t))
                           (let* ((full-path (expand-file-name entry dir))
                                  (relative-entry-name (file-name-nondirectory entry)))
                             ;; Ignore '.' and '..' directories
                             (unless (member relative-entry-name '("." ".."))
                               (let ((current-relative-path
                                      (if (string-empty-p relative-dir-path)
                                          relative-entry-name
                                        (concat relative-dir-path "/" relative-entry-name))))
                                 (cond
                                  ((file-regular-p full-path)
                                   (push current-relative-path all-paths))
                                  ((file-directory-p full-path)
                                   (push (concat current-relative-path "/") all-paths) ;; Add trailing slash for directories
                                   (funcall traverse full-path current-relative-path))))))))))
      (funcall traverse project-root ""))
    (nreverse all-paths)))

(defun ai-common--filter-paths-by-patterns (paths patterns)
  "Filter PATHS based on PATTERNS from ignore files.
PATHS should be relative paths from project root.
PATTERNS is a list of (PATTERN . IS-NEGATED) pairs.
Returns filtered list of paths."
  (let ((filtered-paths nil)
        (compiled-patterns nil))

    ;; Compile all patterns once
    (dolist (pattern-cons patterns)
      (let* ((pattern-str (car pattern-cons))
             (is-negated (cdr pattern-cons))
             (regexp (ai-common--pattern-to-regexp pattern-str)))
        (push (list regexp pattern-str is-negated) compiled-patterns)))
    (setq compiled-patterns (nreverse compiled-patterns))

    ;; Filter each path
    (dolist (path paths)
      (let ((should-include t)
            (matched-rule "default (no ignore rule matched)"))

        ;; Apply each pattern in order
        (dolist (compiled-pattern compiled-patterns)
          (let* ((regexp (nth 0 compiled-pattern))
                 (pattern-str (nth 1 compiled-pattern))
                 (is-negated (nth 2 compiled-pattern)))
            (when (string-match-p regexp path)
              (setq should-include is-negated)
              (setq matched-rule (format "rule '%s' (negated: %s)" pattern-str is-negated)))))

        (when should-include
          (push path filtered-paths))))

    (nreverse filtered-paths)))

(defun ai-common--get-filtered-project-files (&optional relative-paths)
  "Get a list of all relevant files in the current project, filtered by all ignore patterns.
If RELATIVE-PATHS is non-nil, returns relative paths from project root.
Otherwise, returns absolute file paths.
Requires Projectile to be loaded."
  (interactive)
  (let ((project-root (ai-common--get-project-root)))
    (if project-root
        (let* ((all-relative-paths (ai-common--get-all-project-paths project-root))
               (ignore-patterns (ai-common--get-all-ignore-patterns project-root))
               (filtered-relative-paths (ai-common--filter-paths-by-patterns
                                         all-relative-paths ignore-patterns))
               (file-paths nil))
          ;; Filter to only include regular files (paths without trailing slashes)
          (dolist (rel-path filtered-relative-paths)
            (unless (string-suffix-p "/" rel-path) ;; Exclude directories themselves
              (if relative-paths
                  (push rel-path file-paths)
                (push (expand-file-name rel-path project-root) file-paths))))
          (nreverse file-paths))
      (message "Not in a Projectile project. Cannot get project files."))))



(defun ai-common--test-get-filtered-project-files ()
  "Test function for `ai-common--get-filtered-project-files`.
  Displays the list of filtered project files in a new buffer."
  (interactive)
  (let ((files (ai-common--get-filtered-project-files)))
    (if files
        (with-temp-buffer
          (insert (mapconcat 'identity files "\n"))
          (display-buffer (current-buffer)))
      (message "No files found or not in a Projectile project."))))

(defun ai-common--list-filtered-project-files-to-console ()
  "List all relevant files in the current project, filtered by all ignore patterns, to the *Messages* buffer."
  (interactive)
  (let ((files (ai-common--get-filtered-project-files)))
    (if files
        (message "Filtered project files:\n%s" (mapconcat 'identity files "\n"))
      (message "No files found or not in a Projectile project."))
    nil))


(defun ai-common--get-filtered-project-files-as-structs ()
  "Get a list of typed structs containing filtered project files.
Each struct contains file path, content, and metadata.
Returns a list of typed structs with :type 'project-file."
  (interactive)
  (let* ((project-root (ai-common--get-project-root))
        (file-structs nil))
    (if project-root
        (let ((filtered-files (ai-common--get-filtered-project-files)))
          (dolist (file-path filtered-files)
            (when (file-readable-p file-path)
              (condition-case-unless-debug err
                  (let* ((relative-path (file-relative-name file-path project-root))
                         (file-struct (ai-common--make-file-context-from-file
                                       file-path 'file-content 'project-scan
                                       :relative-path relative-path
                                       :project-root project-root)))
                    (push file-struct file-structs))
                (error
                 (message "Warning: Could not read file %s: %s" file-path (error-message-string err))))))
          (nreverse file-structs))
      (progn
        (message "Not in a Projectile project. Cannot get project files.")
        nil))))

(defun ai-common--add-project-files-to-context-pool ()
  "Add all filtered project files as typed structs to the context pool."
  (interactive)
  (let ((file-structs (ai-common--get-filtered-project-files-as-structs)))
    (dolist (struct file-structs)
      (ai-common--add-to-context-pool struct))
    (message "Added %d project files to context pool" (length file-structs))))


(provide 'ai-common)

;;; ai-common.el ends here
