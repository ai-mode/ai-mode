;;; ai-context-management.el --- Context management for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; This file is part of ai-mode.

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
;; This module centralizes the collection, structuring, and storage of context
;; for AI requests, including buffer-specific context, region snippets, and
;; assembly of full execution contexts.

;;; Code:

(require 'cl-lib)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-project)
(require 'ai-prompt-management)
(require 'ai-command-management)
(require 'ai-user-input)

(defcustom ai-context-management--project-context-mode 'disabled
  "Project context inclusion mode for AI execution context.
Controls how project-wide context is included in AI requests:
- `disabled': No project context is included
- `full-project': Include all filtered project files as context
- `project-ai-summary': Include project files summary from cached index"
  :type '(choice (const :tag "Disabled" disabled)
                 (const :tag "Full Project Files" full-project)
                 (const :tag "Project AI Summary from Index" project-ai-summary))
  :group 'ai)

(defcustom ai-context-management--current-buffer-additional-context t
  "Enable additional context for the current buffer."
  :type 'boolean
  :group 'ai)

(defcustom ai-context-management--extended-instructions-enabled t
  "Enable extended instructions."
  :type 'boolean
  :group 'ai)

(defcustom ai-context-management--user-input-method 'ai-user-input-ctrl-enter
  "Function to use for collecting user input.
Should be a function symbol that returns a string or nil."
  :type '(choice (const :tag "Simple minibuffer" ai-user-input-minibuffer-with-preview)
                 (const :tag "Minibuffer with preview" ai-user-input-minibuffer-with-preview)
                 (const :tag "Ctrl-Enter to send" ai-user-input-ctrl-enter)
                 (const :tag "Multiline buffer" ai-user-input-multiline-buffer)
                 (function :tag "Custom function"))
  :group 'ai)

(defcustom ai-context-management--current-precending-context-size 200
  "Number of lines for context."
  :type 'integer
  :group 'ai-completions)

(defcustom ai-context-management--current-forwarding-context-size 200
  "Following context size."
  :type 'integer
  :group 'ai-completions)

;; Moved from ai-utils
(defcustom ai-context-management--default-preceding-context-size 20
  "Number of lines to include in the preceding context."
  :type 'integer
  :group 'ai-context-management)

(defcustom ai-context-management--default-following-context-size 20
  "Number of lines to include in the following context."
  :type 'integer
  :group 'ai-context-management)

;; Context providers registry
(defvar ai-context-management--providers nil
  "List of context provider functions with priorities.
Each element is a cons cell (PRIORITY . PROVIDER-FUNCTION).
PRIORITY is a number (lower numbers have higher priority).
PROVIDER-FUNCTION should accept the following parameters:
- request-id: Unique request identifier
- buffer: Current buffer
- config: Command configuration
- model: Model being used
- context-data: Additional context data

Functions should return a list of typed structs or nil if no context to provide.")

(defun ai-context-management--register-provider (provider-function priority)
  "Register a context PROVIDER-FUNCTION with given PRIORITY.
PRIORITY is a number - lower numbers have higher priority.
The function will be called during context assembly to contribute context elements."
  (unless (functionp provider-function)
    (error "Provider must be a function: %s" provider-function))
  (unless (numberp priority)
    (error "Priority must be a number: %s" priority))
  ;; Remove existing registration of the same provider
  (setq ai-context-management--providers
        (cl-remove provider-function ai-context-management--providers :key #'cdr :test #'eq))
  ;; Add new registration
  (push (cons priority provider-function) ai-context-management--providers)
  ;; Sort by priority (lower numbers first)
  (setq ai-context-management--providers
        (sort ai-context-management--providers (lambda (a b) (< (car a) (car b))))))

(defun ai-context-management--unregister-provider (provider-function)
  "Unregister a context PROVIDER-FUNCTION."
  (setq ai-context-management--providers
        (cl-remove provider-function ai-context-management--providers :key #'cdr :test #'eq)))

(defun ai-context-management--call-providers (request-id buffer config model context-data)
  "Call all registered context providers with the given parameters.
Returns a flattened list of all context elements provided by registered providers."
  (let ((results '()))
    (dolist (provider-entry ai-context-management--providers)
      (let ((provider (cdr provider-entry)))
        (condition-case-unless-debug err
            (let ((provider-result (funcall provider request-id buffer config model context-data)))
              (when provider-result
                ;; Normalize result into a list
                (let ((normalized-result
                       (cond
                        ;; If it's already a list of non-empty elements
                        ((and (listp provider-result)
                              (not (keywordp (car provider-result)))
                              (> (length provider-result) 0))
                         provider-result)
                        ;; If it's a single element
                        ((not (null provider-result))
                         (list provider-result))
                        ;; Otherwise empty list
                        (t '()))))
                  ;; Add all non-empty elements
                  (dolist (item normalized-result)
                    (when (and item (not (null item)))
                      (push item results))))))
          (error
           (ai-utils--log "Error calling context provider %s: %s" provider err)))))
    ;; Return results in correct order
    (reverse results)))

;; Language detection (moved from ai-utils)
(defvar ai-context-management--language-alist
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

;; Context and memory variables moved from ai-common.el

(defvar ai-context-management--global-memo-context nil
  "Global memory context for storing important information that persists across all buffers.
This variable holds data that should be available to AI in any interaction session.")

(defvar ai-context-management--context-pool nil
  "Temporary context pool for the current interaction session.
Unlike global memory, this holds context that is only relevant for the current
AI operation and will be cleared after completion.")

(defvar-local ai-context-management--buffer-bound-prompts nil
  "Buffer-local instructions that apply only to the current buffer.
These prompts are specifically tied to a particular buffer and do not affect
other buffers or global AI behavior.")

(defcustom ai-context-management--global-system-prompts nil
  "Global system instructions for AI.
These define the fundamental behavior and capabilities of the AI assistant
across all interactions and buffers."
  :type 'string
  :group 'ai)

;; Project files summary index
(defvar ai-context-management--project-files-summary-index (make-hash-table :test 'equal)
  "Cached index of project files summary structures.
Maps project root paths to lists of file summary structs.")

;; Context-related functions moved from ai-utils

(defun ai-context-management--get-buffer-type-or-language (buffer)
  "Get the programming language for BUFFER from `ai-context-management--language-alist'."
  (with-current-buffer buffer
    (let ((mode major-mode))
      (while (not (alist-get mode ai-context-management--language-alist))
	(setq mode (get mode 'derived-mode-parent)))
      (alist-get mode ai-context-management--language-alist))))

(defun ai-context-management--get-buffer-path (buffer)
  "Return the file path associated with the BUFFER, or nil if none."
  (with-current-buffer buffer
    (if (projectile-project-p)
        (let ((project-root (projectile-project-root))
              (file-name (buffer-file-name)))
          (and project-root file-name (file-relative-name file-name project-root)))
      (buffer-file-name))))

(defun ai-context-management--get-model-context (model)
  "Get the context information for the given MODEL."
  (let ((name (map-elt model :name ""))
        (provider (map-elt model :provider "")))
    `(:model ,name
      :provider ,provider)))

(defun ai-context-management--get-buffer-context (buffer)
  "Retrieve buffer-related context information for BUFFER."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name)))
      `(:buffer-name ,(buffer-name buffer)
        :buffer-language ,(ai-context-management--get-buffer-type-or-language buffer)
        :buffer ,buffer
        :file-path ,(ai-context-management--get-buffer-path buffer)
        :project-root ,(and (projectile-project-p) (projectile-project-root))
        :file-name ,file-name))))

(cl-defun ai-context-management--get-completion-params (&key (preceding-context-size ai-context-management--default-preceding-context-size)
                                                             (following-context-size ai-context-management--default-following-context-size))
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

(defun ai-context-management--get-memory-file-path (location)
  "Get memory file path for LOCATION (either 'global or 'local)."
  (cond
   ((eq location 'global)
    (expand-file-name ".ai/memory.md" "~"))
   ((eq location 'local)
    (when-let ((project-root (ai-project--get-project-root)))
      (expand-file-name ".ai/memory.md" project-root)))
   ((eq location 'local-team)
    (when-let ((project-root (ai-project--get-project-root)))
      (expand-file-name ".ai/memory.local.md" project-root)))))

(defun ai-context-management--read-memory-file (location)
  "Read memory file content from LOCATION."
  (when-let ((file-path (ai-context-management--get-memory-file-path location)))
    (ai-prompt-management--read-file-content file-path)))

(defun ai-context-management--get-memory-context ()
  "Get combined memory context from all available memory files."
  (let ((memory-contents nil))

    ;; Read global memory
    (when-let ((global-memory (ai-context-management--read-memory-file 'global)))
      (push (ai-common--make-typed-struct global-memory 'memory-content 'global-memory) memory-contents))

    ;; Read local project memory
    (when-let ((local-memory (ai-context-management--read-memory-file 'local)))
      (push (ai-common--make-typed-struct local-memory 'memory-content 'local-memory) memory-contents))

    ;; Read local team memory
    (when-let ((local-team-memory (ai-context-management--read-memory-file 'local-team)))
      (push (ai-common--make-typed-struct local-team-memory 'memory-content 'local-team-memory) memory-contents))

    (when memory-contents
      (ai-common--make-typed-struct memory-contents 'memory 'memory-files))))

;; Memory and context management functions moved from ai-common.el

(defun ai-context-management--add-to-global-memory (input)
  "Add INPUT to `ai-context-management--global-memo-context'.
INPUT is a string representing the context or information to be remembered globally."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter instruction: "))))
  (let ((struct (ai-common--make-typed-struct input 'global-memory-item 'user-input)))
    (setq ai-context-management--global-memo-context (append ai-context-management--global-memo-context `(,struct)))
    (message "AI memory context added")))

(defun ai-context-management--clear-global-memory ()
  "Clear the global memory context."
  (interactive)
  (setq ai-context-management--global-memo-context nil)
  (message "AI buffer context cleared"))

(defun ai-context-management--get-global-memory ()
  "Return the current global memory context."
  ai-context-management--global-memo-context)

(defun ai-context-management--add-buffer-bound-prompts (input)
  "Add INPUT as buffer-specific instructions.
INPUT can be entered by the user or taken from the active region."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter buffer bound prompt: "))))

  (with-current-buffer (current-buffer)
    (let ((struct (ai-common--make-typed-struct input 'buffer-bound-prompt 'user-input)))
      (message "AI buffer bound context added")
      (setq-local ai-context-management--buffer-bound-prompts (append ai-context-management--buffer-bound-prompts `(,struct))))))

(defun ai-context-management--clear-buffer-bound-prompts ()
  "Clear all buffer-specific instructions."
  (with-current-buffer (current-buffer)
    (message "AI buffer context cleared")
    (setq-local ai-context-management--buffer-bound-prompts nil)))

(defun ai-context-management--get-buffer-bound-prompts ()
  "Return the current buffer-specific instructions."
  ai-context-management--buffer-bound-prompts)

(defun ai-context-management--add-global-system-prompts (input)
  "Add INPUT to the global system prompts.
INPUT can be entered by the user or taken from the active region."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter global system prompt: "))))

  (with-current-buffer (current-buffer)
    (let ((struct (ai-common--make-typed-struct input 'global-system-prompt 'user-input)))
      (setq ai-context-management--global-system-prompts
            (append ai-context-management--global-system-prompts `(,struct))))))

(defun ai-context-management--clear-global-system-prompts ()
  "Clear all global system instructions."
  (with-current-buffer (current-buffer)
    (setq ai-context-management--global-system-prompts nil)))

(defun ai-context-management--get-global-system-prompts ()
  "Return the current global system instructions."
  ai-context-management--global-system-prompts)

(defun ai-context-management--clear-context-pool ()
  "Clear the temporary context pool."
  (setq ai-context-management--context-pool nil)
  (message "Clear context pool"))

(defun ai-context-management--get-context-pool ()
  "Return the current context pool."
  ai-context-management--context-pool)

(defun ai-context-management--add-to-context-pool (item)
  "Add ITEM (plist-structure) to `ai-context-management--context-pool`."
  (push item ai-context-management--context-pool))

(defun ai-context-management--format-context-pool-item-for-selection (item)
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

(defun ai-context-management--remove-from-context-pool ()
  "Remove selected item from context pool via minibuffer selection."
  (if (null ai-context-management--context-pool)
      (message "Context pool is empty")
    (let* ((items-with-display (mapcar (lambda (item)
                                         (cons (ai-context-management--format-context-pool-item-for-selection item) item))
                                       ai-context-management--context-pool))
           (selected-display (completing-read "Remove from context pool: "
                                              (mapcar #'car items-with-display)
                                              nil t))
           (selected-item (cdr (assoc selected-display items-with-display))))
      (when selected-item
        (setq ai-context-management--context-pool
              (cl-remove selected-item ai-context-management--context-pool :test #'equal))
        (message "Removed item from context pool: %s"
                 (ai-context-management--format-context-pool-item-for-selection selected-item))))))

(defun ai-context-management--capture-region-snippet ()
  "Create a snippet from the region and add it to the context pool."
  (let ((snippet (ai-common--make-snippet-from-region)))
    (when snippet
      (ai-context-management--add-to-context-pool snippet))))

(defun ai-context-management--capture-user-input ()
  "Capture user input and add it to the context pool."
  (ai-context-management--add-to-context-pool (ai-context-management--get-user-input-struct)))

(defun ai-context-management--capture-file-context ()
  "Adds the entire file content to the context pool."
  (let ((file-context (ai-common--make-file-context-from-buffer)))
    (ai-context-management--add-to-context-pool file-context)
    (message "File content added to context pool.")))

(defun ai-context-management--get-user-input-struct ()
  "Create a plist structure based on user input via minibuffer."
  (let ((text (read-string "Enter instruction or context for AI: ")))
    (ai-common--make-typed-struct text 'user-input 'user-input
                                  :render-ignore-fields '(:source))))

(cl-defun ai-context-management--assemble-completion-context (&key preceding-context-size following-context-size)
  "Return a list of context elements for <completion>.
PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE specify context sizes.

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

(defun ai-context-management--assemble-edit-context ()
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

(cl-defun ai-context-management--assemble-edit-context-extended (&key preceding-context-size following-context-size)
  "Return a list of context elements for edit operations with flexible sizing.
PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE specify context sizes.

Modes of operation:
1. If the region is active, return `preceding-context`, the selected region as `selection`, and `following-context`.
2. If not active, return the whole file as `file-context`."
  (if (use-region-p)
      ;; Mode 1: Selected region with context
      (let* ((region-start (region-beginning))
             (region-end (region-end))
             (pre-start (if preceding-context-size
                            (point-min)
                          (max (point-min) (- region-start preceding-context-size))))
             (post-end (if following-context-size
                           (point-max)
                         (min (point-max) (+ region-end following-context-size))))
             (pre-content (when (< pre-start region-start)
                            (buffer-substring-no-properties pre-start region-start)))
             (post-content (when (< region-end post-end)
                             (buffer-substring-no-properties region-end post-end)))
             (selection (ai-common--make-snippet-from-region 'selection))
             (file-mod-time (when (buffer-file-name)
                              (format-time-string "%Y-%m-%dT%H:%M:%S"
                                                  (file-attribute-modification-time
                                                   (file-attributes (buffer-file-name))))))
             (results (list)))

        ;; Add preceding context if it exists
        (when (and pre-content (> (length pre-content) 0))
          (let ((pre-struct (ai-common--make-typed-struct
                             pre-content 'preceding-context 'edit-context
                             :file (or (buffer-file-name) (buffer-name))
                             :buffer (buffer-name)
                             :start-pos pre-start
                             :end-pos (- region-start 1)
                             :start-line (line-number-at-pos pre-start)
                             :end-line (line-number-at-pos (- region-start 1))
                             :start-column (car (posn-col-row (posn-at-point pre-start)))
                             :end-column (car (posn-col-row (posn-at-point (- region-start 1))))
                             :mode (symbol-name major-mode)
                             (if file-mod-time :file-modified file-mod-time))))
            (push pre-struct results)))

        ;; Add selection
        (push selection results)

        ;; Add following context if it exists
        (when (and post-content (> (length post-content) 0))
          (let ((post-struct (ai-common--make-typed-struct
                              post-content 'following-context 'edit-context
                              :file (or (buffer-file-name) (buffer-name))
                              :buffer (buffer-name)
                              :start-pos (+ region-end 1)
                              :end-pos post-end
                              :start-line (line-number-at-pos (+ region-end 1))
                              :end-line (line-number-at-pos post-end)
                              :start-column (car (posn-col-row (posn-at-point (+ region-end 1))))
                              :end-column (car (posn-col-row (posn-at-point post-end)))
                              :mode (symbol-name major-mode)
                              (if file-mod-time :file-modified file-mod-time))))
            (push post-struct results)))

        (nreverse results))

    ;; Mode 2: No region - return whole file
    (list (ai-common--make-file-context-from-buffer))))

;; Rest of the existing functions remain unchanged...

(defun ai-context-management--get-selected-region (config full-context)
  "Return the currently selected region.
CONFIG contains configuration details and FULL-CONTEXT includes information for rendering."
  (when (region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ai-context-management--render-messages-templates (items context)
  "Render message templates from ITEMS using CONTEXT."
  (mapcar (lambda (content) (ai-prompt-management--render-template content context)) items))

(defun ai-context-management--get-action-type-for-config (config)
  "Determine the action type from CONFIG based on result-action, defaulting to \"modify\"."
  (let* ((result-action (map-elt config :result-action))
         (action-type (ai-context-management--get-container-type-by-result-action result-action)))
    (or action-type "modify")))

(defun ai-context-management--get-container-type-by-result-action (result-action)
  "Determine the container type based on RESULT-ACTION.
Returns the container name or nil if no specific container is needed."
  (cond
   ((eq result-action 'complete)        "complete")
   ((eq result-action 'show)            "explain")
   ((eq result-action 'eval)            "eval")
   ((eq result-action 'replace)         "modify")
   ((eq result-action 'insert-at-point) "complete")
   (t                                   nil)))

(cl-defun ai-context-management--get-contextual-action-object (config &key preceding-context-size following-context-size)
  "Generate contextual action object based on CONFIG and optional context sizes PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE."
  (let* ((action (map-elt config :action))
         (result-action (map-elt config :result-action))
         (container-type (ai-context-management--get-container-type-by-result-action result-action))
         (needs-buffer-context (map-elt config :needs-buffer-context))
         ;; If needs-buffer-context is true, use nil for full context, otherwise use provided sizes
         (actual-preceding-size (if needs-buffer-context nil preceding-context-size))
         (actual-following-size (if needs-buffer-context nil following-context-size)))

    (if (equal container-type "complete")
        (ai-common--make-action-object
         container-type
         (ai-context-management--assemble-completion-context
          :preceding-context-size preceding-context-size
          :following-context-size following-context-size)
         'contextual-action)
      (ai-common--make-action-object
       container-type
       (ai-context-management--assemble-edit-context-extended
        :preceding-context-size actual-preceding-size
        :following-context-size actual-following-size)
       'contextual-action))))

(defun ai-context-management--get-current-buffer-context ()
  "Get the additional context for the current buffer if enabled."
  (when ai-context-management--current-buffer-additional-context
    (ai-common--make-file-context-from-buffer)))

(defun ai-context-management--should-include-current-buffer-content-context-p (config full-context)
  "Determine if current-buffer-content-context should be included.
CONFIG is the command configuration, and FULL-CONTEXT contains the complete context information.
Returns t if context should be included, nil otherwise."
  (let* ((result-action (map-elt config :result-action))
         (needs-buffer-context (map-elt config :needs-buffer-context))
         (container-type (ai-context-management--get-container-type-by-result-action result-action))
         (has-region (use-region-p)))

    (and ai-context-management--current-buffer-additional-context
         (or (and needs-buffer-context
                  (not (string= container-type "complete")))))))

(defun ai-context-management--get-result-action-prompt (result-action context)
  "Get the prompt for RESULT-ACTION rendered with CONTEXT."
  (let ((prompt-name (if (and (eq result-action 'replace)
                             (bound-and-true-p ai-execution--replace-action-use-patch))
                         "result_action_apply_patch"
                       (format "result_action_%s" (symbol-name result-action)))))
    (ai-prompt-management--render-system-prompt prompt-name context)))

(defun ai-context-management--get-user-input ()
  "Get user input using the configured function."
  (if (functionp ai-context-management--user-input-method)
      (funcall ai-context-management--user-input-method)
    (error "ai-context-management--user-input-method is not a valid function: %s" ai-context-management--user-input-method)))

(defun ai-context-management--process-external-context-item (item)
  "Process a single external context item ITEM for inclusion in execution context.
Handles both plain contexts and typed structs, including nested structures."
  (cond
   ;; Already a typed struct - return as is
   ((and (listp item) (keywordp (car item)))
    item)
   ;; Plain string or other format
   (t
    (ai-common--make-typed-struct
     (format "%s" item)
     'additional-context
     'external-context))))

(defun ai-context-management--get-full-project-context ()
  "Get project context by collecting all filtered project files.
Returns a typed struct containing the project files context, or nil if no project is detected."
  (when-let* ((project-root (ai-project--get-project-root))
              (files-list (ai-project--get-filtered-project-files t)) ; Request relative paths
              (project-files (ai-project--get-filtered-project-files-as-structs)))
    (when project-files
      (let* ((files-list-content (mapconcat (lambda (file-path)
                                              (format "- %s" file-path))
                                            files-list "\n"))
             (files-list-struct (ai-common--make-typed-struct
                                files-list-content
                                'files-list
                                'project-scan
                                :root project-root
                                :count (length files-list)))
             (files-struct (ai-common--make-typed-struct project-files 'files 'project-files))
             (project-struct (ai-common--make-typed-struct
                             (list files-list-struct files-struct)
                             'project-context
                             'project-indexer
                             :root project-root)))
        project-struct))))

(defun ai-context-management--get-project-ai-summary-context ()
  "Get project context using project AI summary mode with cached index.
Returns a typed struct containing the project files summary from cached index,
or nil if no project is detected or index is empty."
  (when-let* ((project-root (ai-project--get-project-root))
              (files-list (ai-project--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai-context-management--project-files-summary-index)))
    (let* ((files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count (length files-list)))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root)))
      project-struct)))

(defun ai-context-management--get-enhanced-project-ai-summary-context ()
  "Get enhanced project context using project AI summary mode with dependency awareness."
  (when-let* ((project-root (ai-project--get-project-root))
              (files-list (ai-project--get-filtered-project-files t))
              (summaries-for-current-project (gethash project-root ai-context-management--project-files-summary-index)))
    (let* ((files-count (length files-list))
           (indexed-count (length summaries-for-current-project))
           (files-list-content (mapconcat (lambda (file-path)
                                            (format "- %s" file-path))
                                          files-list "\n"))
           (files-list-struct (ai-common--make-typed-struct
                              files-list-content
                              'files-list
                              'project-scan
                              :root project-root
                              :count files-count
                              :indexed-count indexed-count))
           (files-struct (ai-common--make-typed-struct
                         (or summaries-for-current-project '())
                         'files
                         'project-summary-index
                         :has-context (> indexed-count 0)))
           (project-struct (ai-common--make-typed-struct
                           (list files-list-struct files-struct)
                           'project-ai-summary
                           'project-ai-indexer
                           :root project-root
                           :indexing-mode "enhanced")))
      project-struct)))

(defun ai-context-management--get-project-context ()
  "Get project context based on `ai-context-management--project-context-mode` setting.
Returns a typed struct containing the appropriate project context, or nil if disabled."
  (cond
   ((eq ai-context-management--project-context-mode 'full-project)
    (ai-context-management--get-full-project-context))
   ((eq ai-context-management--project-context-mode 'project-ai-summary)
    (ai-context-management--get-project-ai-summary-context))
   (t nil)))

(defun ai-context-management--switch-project-context-mode ()
  "Interactively switch the project context mode.
Allows user to select between different project context inclusion modes."
  (let* ((current-mode ai-context-management--project-context-mode)
         (modes '(("disabled" . disabled)
                  ("full-project" . full-project)
                  ("project-ai-summary" . project-ai-summary)))
         (mode-descriptions '((disabled . "No project context")
                             (full-project . "Include all filtered project files")
                             (project-ai-summary . "Include project files summary from cached index")))
         (prompt (format "Current mode: %s. Select new project context mode: "
                        (cdr (assoc current-mode mode-descriptions))))
         (selected-name (completing-read prompt (mapcar #'car modes)))
         (selected-mode (cdr (assoc selected-name modes))))

    (setq ai-context-management--project-context-mode selected-mode)
    (customize-save-variable 'ai-context-management--project-context-mode selected-mode)
    (message "Project context mode changed to: %s (%s)"
             selected-name
             (cdr (assoc selected-mode mode-descriptions)))))


(cl-defun ai-context-management--get-execution-context (buffer config &key
                                                               (preceding-context-size ai-context-management--current-precending-context-size)
                                                               (following-context-size ai-context-management--current-forwarding-context-size)
                                                               model
                                                               (external-contexts nil))
  "Get full execution context for BUFFER.
CONFIG specifies configuration, and options for context sizes are PRECEDING-CONTEXT-SIZE and FOLLOWING-CONTEXT-SIZE.
EXTERNAL-CONTEXTS is an optional list of additional context structs to include.
Each context should be a plist with :type, :content, and other metadata."
  (with-current-buffer buffer
    (let* ((needs-buffer-context (map-elt config :needs-buffer-context))
           (actual-preceding-size (if needs-buffer-context nil preceding-context-size))
           (actual-following-size (if needs-buffer-context nil following-context-size))
           (completion-context (ai-context-management--get-completion-params
                                :preceding-context-size actual-preceding-size
                                :following-context-size actual-following-size))
           (buffer-context (ai-context-management--get-buffer-context (current-buffer)))
           (model-context (ai-context-management--get-model-context model))
           (full-context (append completion-context buffer-context model-context))
           (request-id (ai-common--generate-request-id))

           ;; Create context data as alist
           (context-data `((completion-context . ,completion-context)
                           (buffer-context . ,buffer-context)
                           (full-context . ,full-context)
                           (needs-buffer-context . ,needs-buffer-context)
                           (actual-preceding-size . ,actual-preceding-size)
                           (actual-following-size . ,actual-following-size)
                           (external-contexts . ,external-contexts)))

           ;; Get contexts from providers
           (provider-contexts (when ai-context-management--extended-instructions-enabled
                                (ai-context-management--call-providers request-id buffer config model context-data)))

           (messages (ai-context-management--filter-non-empty-content provider-contexts))

           (_ (ai-telemetry-write-context-to-prompt-buffer messages))

           (result `(:messages ,messages :model-context ,model :request-id ,request-id :command-config ,config)))
      result)))

(cl-defun ai-context-management--get-executions-context-for-command (command &key (model nil) (default-result-action nil) (external-contexts nil))
  "Get execution context for COMMAND with optional MODEL, DEFAULT-RESULT-ACTION, and EXTERNAL-CONTEXTS.
EXTERNAL-CONTEXTS is a list of additional context structs to include in the execution context."
  (let* ((config (ai-command-management--get-command-config-by-type command default-result-action))
         (execution-context (ai-context-management--get-execution-context (current-buffer) config :model model :external-contexts external-contexts)))
    execution-context))

(defun ai-context-management--empty-message-p (message)
  "Return non-nil if MESSAGE has an empty or missing :content field.
Handles cases where :content might not be a string."
  (let ((content (plist-get message :content)))
    (or (null content)
        (and (stringp content) (string-empty-p content)))))

(defun ai-context-management--filter-non-empty-content (messages)
  "Filter out messages with empty content from MESSAGES."
  (cl-remove-if #'ai-context-management--empty-message-p messages))

;; ============================================================================
;; Context Providers
;; ============================================================================

(defun ai-context-management--provider-basic-instructions (request-id buffer config model context-data)
  "Provider for basic agent instructions."
  (let ((full-context (alist-get 'full-context context-data)))
    (when-let ((content (ai-prompt-management--render-system-prompt "basic" full-context)))
      (ai-common--make-typed-struct content 'agent-instructions 'basic-prompt :group 'basic))))

(defun ai-context-management--provider-file-metadata (request-id buffer config model context-data)
  "Provider for file metadata context."
  (let ((full-context (alist-get 'full-context context-data)))
    (when-let ((content (ai-prompt-management--render-system-prompt "file_metadata" full-context)))
      (ai-common--make-typed-struct content 'file-metadata 'file-metadata))))

(defun ai-context-management--provider-command-instructions (request-id buffer config model context-data)
  "Provider for command-specific instructions."
  (let ((full-context (alist-get 'full-context context-data))
        (command (map-elt config :command)))
    (when-let ((content (ai-command-management--get-rendered-command-instructions command full-context)))
      (ai-common--make-typed-struct content 'agent-instructions 'command-specific-instructions :group 'command))))

(defun ai-context-management--provider-command-examples (request-id buffer config model context-data)
  "Provider for command examples."
  (let ((full-context (alist-get 'full-context context-data))
        (command (map-elt config :command)))
    (when-let ((content (ai-command-management--get-rendered-command-examples command full-context)))
      (ai-common--make-typed-struct content 'agent-instructions 'command-examples :group 'command))))

(defun ai-context-management--provider-action-type-object-instructions (request-id buffer config model context-data)
  "Provider for action type object instructions."
  (let ((full-context (alist-get 'full-context context-data)))
    (when-let ((content (ai-command-management--get-action-type-object-instructions (ai-context-management--get-action-type-for-config config) full-context)))
      (ai-common--make-typed-struct content 'agent-instructions 'action-object-rules :group 'command))))

(defun ai-context-management--provider-result-action-instructions (request-id buffer config model context-data)
  "Provider for result action instructions."
  (let ((result-action (map-elt config :result-action))
        (full-context (alist-get 'full-context context-data)))
    (when result-action
      (when-let ((content (ai-context-management--get-result-action-prompt result-action full-context)))
        (ai-common--make-typed-struct content 'agent-instructions 'result-action-format :group 'command)))))

(defun ai-context-management--provider-config-instructions (request-id buffer config model context-data)
  "Provider for config-specific instructions."
  (when-let ((instructions (map-elt config :instructions)))
    (ai-common--make-typed-struct instructions 'agent-instructions 'config-instructions)))

(defun ai-context-management--provider-context-pool (request-id buffer config model context-data)
  "Provider for context pool items."
  (when-let ((context-pool (ai-context-management--get-context-pool)))
    (when context-pool
      (ai-common--make-typed-struct context-pool 'additional-context 'context-pool))))

(defun ai-context-management--provider-external-contexts (request-id buffer config model context-data)
  "Provider for external contexts."
  (let ((external-contexts (alist-get 'external-contexts context-data)))
    (when external-contexts
      (let ((processed-contexts (mapcar #'ai-context-management--process-external-context-item external-contexts)))
        (ai-common--make-typed-struct processed-contexts 'additional-context 'external-context)))))

(defun ai-context-management--provider-project-context (request-id buffer config model context-data)
  "Provider for project context."
  (ai-context-management--get-project-context))

(defun ai-context-management--provider-memory-context (request-id buffer config model context-data)
  "Provider for memory context."
  (ai-context-management--get-memory-context))

(defun ai-context-management--provider-current-buffer-content (request-id buffer config model context-data)
  "Provider for current buffer content context."
  (let ((full-context (alist-get 'full-context context-data)))
    (when (ai-context-management--should-include-current-buffer-content-context-p config full-context)
      (when-let ((context (ai-context-management--get-current-buffer-context)))
        (ai-common--make-typed-struct context 'additional-context 'current-buffer-content)))))

(defun ai-context-management--provider-user-input (request-id buffer config model context-data)
  "Provider for user input."
  (when (map-elt config :user-input)
    (let ((input-text (ai-context-management--get-user-input))
          (full-context (alist-get 'full-context context-data)))
      ;; If user input is cancelled, stop execution
      (unless input-text
        (user-error "User input cancelled."))
      (ai-common--make-typed-struct
       (ai-prompt-management--render-template input-text full-context)
       'user-input
       'user-input
       :render-ignore-fields '(:source)))))

(defun ai-context-management--provider-command-struct (request-id buffer config model context-data)
  "Provider for command struct."
  (when-let ((command-text (map-elt config :command)))
    (let ((full-context (alist-get 'full-context context-data)))
      (ai-common--make-typed-struct
       (ai-prompt-management--render-template command-text full-context)
       'user-input
       'config-command))))

(defun ai-context-management--provider-action-context (request-id buffer config model context-data)
  "Provider for action context."
  (let ((actual-preceding-size (alist-get 'actual-preceding-size context-data))
        (actual-following-size (alist-get 'actual-following-size context-data)))
    (ai-common--make-typed-struct
     (ai-context-management--get-contextual-action-object
      config
      :preceding-context-size actual-preceding-size
      :following-context-size actual-following-size)
     'action-context
     'contextual-action)))

(defun ai-context-management--provider-global-system-prompts (request-id buffer config model context-data)
  "Provider for global system prompts."
  (when-let ((global-system-prompts (ai-context-management--get-global-system-prompts)))
    (when global-system-prompts
      (ai-common--make-typed-struct global-system-prompts 'agent-instructions 'global-system-prompts))))

(defun ai-context-management--provider-global-memory (request-id buffer config model context-data)
  "Provider for global memory context."
  (when-let ((global-memory (ai-context-management--get-global-memory)))
    (when global-memory
      (ai-common--make-typed-struct global-memory 'additional-context 'global-memory))))

(defun ai-context-management--provider-buffer-bound-prompts (request-id buffer config model context-data)
  "Provider for buffer-bound prompts."
  (when-let ((buffer-bound-prompts (ai-context-management--get-buffer-bound-prompts)))
    (when buffer-bound-prompts
      (ai-common--make-typed-struct buffer-bound-prompts 'agent-instructions 'buffer-bound-prompts))))

;; Register all default providers with priorities
(ai-context-management--register-provider #'ai-context-management--provider-basic-instructions 100)
(ai-context-management--register-provider #'ai-context-management--provider-file-metadata 150)
(ai-context-management--register-provider #'ai-context-management--provider-global-system-prompts 200)
(ai-context-management--register-provider #'ai-context-management--provider-command-instructions 250)
(ai-context-management--register-provider #'ai-context-management--provider-config-instructions 300)
(ai-context-management--register-provider #'ai-context-management--provider-action-type-object-instructions 350)
(ai-context-management--register-provider #'ai-context-management--provider-result-action-instructions 400)
(ai-context-management--register-provider #'ai-context-management--provider-command-examples 450)
(ai-context-management--register-provider #'ai-context-management--provider-memory-context 500)
(ai-context-management--register-provider #'ai-context-management--provider-global-memory 550)
(ai-context-management--register-provider #'ai-context-management--provider-buffer-bound-prompts 600)
(ai-context-management--register-provider #'ai-context-management--provider-project-context 650)
(ai-context-management--register-provider #'ai-context-management--provider-current-buffer-content 700)
(ai-context-management--register-provider #'ai-context-management--provider-context-pool 750)
(ai-context-management--register-provider #'ai-context-management--provider-external-contexts 800)
(ai-context-management--register-provider #'ai-context-management--provider-user-input 850)
(ai-context-management--register-provider #'ai-context-management--provider-command-struct 900)
(ai-context-management--register-provider #'ai-context-management--provider-action-context 950)


(provide 'ai-context-management)

;;; ai-context-management.el ends here
