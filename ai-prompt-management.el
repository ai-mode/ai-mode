;;; ai-prompt-management.el --- Prompt and instruction management for AI mode -*- lexical-binding: t -*-

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
;; This module manages the creation, assembly, rendering, and caching of prompts
;; and instructions for AI models, including:
;; - Loading and caching instruction and system prompt files from various locations.
;; - Rendering templates with contextual data.
;; - Handling file system interactions related to instructions (directories, mtimes).

;;; Code:

(require 'cl-lib)
(require 'ai-common) ; For ai-common--get-project-root
(require 'ai-telemetry) ; For ai-telemetry--verbose-message

(defcustom ai-prompt-management--instruction-file-extension ".md"
  "File extension used for instruction and system prompt files."
  :type 'string
  :group 'ai)

(defcustom ai-prompt-management--instruction-watching-enabled t
  "Enable file watching for instruction files to auto-reload cache."
  :type 'boolean
  :group 'ai)

(defvar ai-prompt-management--ai-subdirectory ".ai/"
  "Subdirectory where AI-related files are stored.")

(defcustom ai-prompt-management--legacy-instruction-file-extension ".ins"
  "File extension used for legacy instruction files."
  :type 'string
  :group 'ai)

;; Instruction cache variables
(defvar ai-prompt-management--default-instructions-cache (make-hash-table :test 'equal)
  "Cache for default instruction files from ai-mode package.")

(defvar ai-prompt-management--global-instructions-cache (make-hash-table :test 'equal)
  "Cache for global instruction files from ~/.ai/.")

(defvar-local ai-prompt-management--local-instructions-cache (make-hash-table :test 'equal)
  "Buffer-local cache for project instruction files.")

;; System prompt cache variables
(defvar ai-prompt-management--default-system-prompts-cache (make-hash-table :test 'equal)
  "Cache for default system prompt files from ai-mode package.")

(defvar ai-prompt-management--global-system-prompts-cache (make-hash-table :test 'equal)
  "Cache for global system prompt files from ~/.ai/.")

(defvar-local ai-prompt-management--local-system-prompts-cache (make-hash-table :test 'equal)
  "Buffer-local cache for project system prompt files.")

(defvar ai-prompt-management--instruction-file-watchers (make-hash-table :test 'equal)
  "Hash table storing file watchers for instruction directories.")

(defvar ai-prompt-management--instruction-directory-mtimes (make-hash-table :test 'equal)
  "Hash table storing modification times for instruction directories.")

(defun ai-prompt-management--render-template (template keyword-list)
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

(defun ai-prompt-management--get-query-type-instructions-file (command-name)
  "Return the path to the instructions file for COMMAND-NAME.
The file is located in the .ai/ subdirectory with
the extension from `ai-prompt-management--legacy-instruction-file-extension'."
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name (concat ai-prompt-management--ai-subdirectory
                              (replace-regexp-in-string " " "_" command-name)
                              ai-prompt-management--legacy-instruction-file-extension)
                      current-dir)))

(defun ai-prompt-management--get-instructions-from-file (path)
  "Return the string contents from a file at PATH if readable.
Output is logged if verbose logging is enabled."
  (if (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string))))

(defun ai-prompt-management--file-instructions-for-command (command-name base-path)
  "Return the path to the instruction file for COMMAND-NAME at BASE-PATH.
The file is located in the .ai/ subdirectory and is suffixed
with `ai-prompt-management--legacy-instruction-file-extension'."
  (let ((file-path (expand-file-name (concat ai-prompt-management--ai-subdirectory
                                             (replace-regexp-in-string " " "_" command-name)
                                             ai-prompt-management--legacy-instruction-file-extension)
                                     base-path)))
    (ai-prompt-management--get-instructions-from-file file-path)))

(defun ai-prompt-management--get-default-instructions-directory ()
  "Get the default instructions directory from the ai-mode package."
  (file-name-as-directory
   (expand-file-name ".ai/commands"
                     (file-name-directory (locate-library "ai-mode")))))

(defun ai-prompt-management--get-global-instructions-directory ()
  "Get the global instructions directory from user's home."
  (file-name-as-directory
   (expand-file-name ".ai/commands" "~")))

(defun ai-prompt-management--get-local-instructions-directory ()
  "Get the local instructions directory from current project root."
  (when-let ((project-root (ai-common--get-project-root)))
    (file-name-as-directory
     (expand-file-name ".ai/commands" project-root))))

(defun ai-prompt-management--get-default-system-prompts-directory ()
  "Get the default system prompts directory from the ai-mode package."
  (file-name-as-directory
   (expand-file-name ".ai/system"
                     (file-name-directory (locate-library "ai-mode")))))

(defun ai-prompt-management--get-global-system-prompts-directory ()
  "Get the global system prompts directory from user's home."
  (file-name-as-directory
   (expand-file-name ".ai/system" "~")))

(defun ai-prompt-management--get-local-system-prompts-directory ()
  "Get the local system prompts directory from current project root."
  (when-let ((project-root (ai-common--get-project-root)))
    (file-name-as-directory
     (expand-file-name ".ai/system" project-root))))

(defun ai-prompt-management--normalize-name-for-lookup (name)
  "Normalize name by converting spaces to underscores for consistent cache lookup.
This ensures names like 'foo bar' and 'foo_bar' map to the same internal key."
  (when name
    (replace-regexp-in-string "[[:space:]]" "_" name)))

(defun ai-prompt-management--get-file-path-for-name (name directory &optional is-example-file)
  "Get the full path to an instruction/prompt file for NAME in DIRECTORY.
NAME should be the logical name (with `__` for modifiers if applicable for commands).
If IS-EXAMPLE-FILE is t, it assumes the '.examples.md' suffix.
The file name will replace spaces with underscores."
  (when (and name directory)
    (let* ((filename (ai-prompt-management--normalize-name-for-lookup name))
           (final-filename (if is-example-file
                               (format "%s.examples%s" filename ai-prompt-management--instruction-file-extension)
                             (concat filename ai-prompt-management--instruction-file-extension))))
      (expand-file-name final-filename directory))))

(defun ai-prompt-management--read-file-content (file-path)
  "Read content from FILE-PATH. Returns nil if file doesn't exist or isn't readable."
  (when (and file-path (file-readable-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(defun ai-prompt-management--get-directory-modification-time (directory)
  "Get the modification time of DIRECTORY. Returns nil if directory doesn't exist."
  (when (file-directory-p directory)
    (file-attribute-modification-time (file-attributes directory))))

(defun ai-prompt-management--directory-needs-cache-update-p (directory cache-key)
  "Check if DIRECTORY needs cache update based on modification time."
  (when-let ((current-mtime (ai-prompt-management--get-directory-modification-time directory)))
    (let ((cached-mtime (gethash cache-key ai-prompt-management--instruction-directory-mtimes)))
      (not (and cached-mtime (time-equal-p current-mtime cached-mtime))))))

(defun ai-prompt-management--scan-directory-for-names (directory &optional is-system-prompt-dir)
  "Scan DIRECTORY for instruction/prompt files and return list of names (logical, not filenames).
If IS-SYSTEM-PROMPT-DIR, it also normalizes the names for lookup consistency."
  (when (file-directory-p directory)
    (let ((files (directory-files directory nil (concat "\\." (regexp-quote (substring ai-prompt-management--instruction-file-extension 1)) "$")))
          (names nil))
      (dolist (file files)
        (let ((base-name (file-name-sans-extension file)))
          ;; Exclude .examples.md files from primary command/prompt list
          (unless (string-match-p "\\.examples$" base-name)
            (let ((name (replace-regexp-in-string "_" " " base-name))) ; Convert underscores back to spaces for logical name
              (when is-system-prompt-dir
                (setq name (ai-prompt-management--normalize-name-for-lookup name)))
              (push name names)))))
      names)))

(defun ai-prompt-management--update-cache (directory cache-table cache-key &optional is-system-prompt-dir)
  "Update CACHE-TABLE with content from DIRECTORY. CACHE-KEY is used for modification time tracking.
If IS-SYSTEM-PROMPT-DIR, uses system prompt normalization."
  (when (file-directory-p directory)
    (clrhash cache-table)
    (let ((names (ai-prompt-management--scan-directory-for-names directory is-system-prompt-dir)))
      (dolist (name names)
        (when-let ((content (ai-prompt-management--read-file-content
                            (ai-prompt-management--get-file-path-for-name name directory))))
          (puthash (if is-system-prompt-dir (ai-prompt-management--normalize-name-for-lookup name) name)
                   content cache-table))))
    ;; Update modification time
    (puthash cache-key (ai-prompt-management--get-directory-modification-time directory) ai-prompt-management--instruction-directory-mtimes)
    (ai-telemetry--verbose-message "Updated cache for %s (%d entries)" cache-key (hash-table-count cache-table))))

(defun ai-prompt-management--ensure-cache-updated (directory cache-table cache-key &optional is-system-prompt-dir)
  "Ensure CACHE-TABLE is up to date for DIRECTORY identified by CACHE-KEY."
  (when (ai-prompt-management--directory-needs-cache-update-p directory cache-key)
    (ai-prompt-management--update-cache directory cache-table cache-key is-system-prompt-dir)))

;; Public API for raw content retrieval

(defun ai-prompt-management--get-instruction-content (name)
  "Get raw instruction content for NAME from priority: Local > Global > Default.
NAME should be the full command name (e.g., 'user__explain_code__show')."
  (let ((local-dir (ai-prompt-management--get-local-instructions-directory))
        (global-dir (ai-prompt-management--get-global-instructions-directory))
        (default-dir (ai-prompt-management--get-default-instructions-directory)))

    (when local-dir
      (ai-prompt-management--ensure-cache-updated local-dir ai-prompt-management--local-instructions-cache "local"))
    (when global-dir
      (ai-prompt-management--ensure-cache-updated global-dir ai-prompt-management--global-instructions-cache "global"))
    (ai-prompt-management--ensure-cache-updated default-dir ai-prompt-management--default-instructions-cache "default")

    (or (when local-dir (gethash name ai-prompt-management--local-instructions-cache))
        (when global-dir (gethash name ai-prompt-management--global-instructions-cache))
        (gethash name ai-prompt-management--default-instructions-cache))))

(defun ai-prompt-management--get-system-prompt-content (name)
  "Get raw system prompt content for NAME from priority: Local > Global > Default.
Normalizes NAME for lookup."
  (let ((normalized-name (ai-prompt-management--normalize-name-for-lookup name))
        (local-dir (ai-prompt-management--get-local-system-prompts-directory))
        (global-dir (ai-prompt-management--get-global-system-prompts-directory))
        (default-dir (ai-prompt-management--get-default-system-prompts-directory)))

    (when local-dir
      (ai-prompt-management--ensure-cache-updated local-dir ai-prompt-management--local-system-prompts-cache "local-system" t))
    (when global-dir
      (ai-prompt-management--ensure-cache-updated global-dir ai-prompt-management--global-system-prompts-cache "global-system" t))
    (ai-prompt-management--ensure-cache-updated default-dir ai-prompt-management--default-system-prompts-cache "default-system" t)

    (or (when local-dir (gethash normalized-name ai-prompt-management--local-system-prompts-cache))
        (when global-dir (gethash normalized-name ai-prompt-management--global-system-prompts-cache))
        (gethash normalized-name ai-prompt-management--default-system-prompts-cache))))

(defun ai-prompt-management--get-examples-content (name)
  "Get raw examples content for NAME from priority: Local > Global > Default.
NAME should be the base command name (e.g., 'explain_code')."
  (let ((local-dir (ai-prompt-management--get-local-instructions-directory))
        (global-dir (ai-prompt-management--get-global-instructions-directory))
        (default-dir (ai-prompt-management--get-default-instructions-directory)))
    (or (when local-dir (ai-prompt-management--read-file-content (ai-prompt-management--get-file-path-for-name name local-dir t)))
        (when global-dir (ai-prompt-management--read-file-content (ai-prompt-management--get-file-path-for-name name global-dir t)))
        (ai-prompt-management--read-file-content (ai-prompt-management--get-file-path-for-name name default-dir t)))))

;; Public API for rendering templates

(defun ai-prompt-management--render-instruction (name context)
  "Get and render instruction for NAME with CONTEXT.
If no instruction content is found for NAME, returns nil."
  (when-let ((content (ai-prompt-management--get-instruction-content name)))
    (ai-prompt-management--render-template content context)))

(defun ai-prompt-management--render-system-prompt (name context)
  "Get and render system prompt for NAME with CONTEXT.
If no system prompt content is found for NAME, returns nil."
  (when-let ((content (ai-prompt-management--get-system-prompt-content name)))
    (ai-prompt-management--render-template content context)))

(defun ai-prompt-management--render-examples (name context)
  "Get and render examples for NAME with CONTEXT.
If no examples content is found for NAME, returns nil."
  (when-let ((examples-content (ai-prompt-management--get-examples-content name)))
    (ai-prompt-management--render-template examples-content context)))

(defun ai-prompt-management--render-action-type-object-instructions (action-type context)
  "Get and render action type object instructions for ACTION-TYPE with CONTEXT.
This is a specialized system prompt."
  (ai-prompt-management--render-system-prompt (format "%s_action_type_object" action-type) context))

;; Public API for cache updates

(defun ai-prompt-management--update-caches ()
  "Update all instruction and system prompt caches for all locations."
  (message "Updating AI mode prompt caches...")

  ;; Update default commands cache
  (let ((directory (ai-prompt-management--get-default-instructions-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--default-instructions-cache "default"))

  ;; Update global commands cache
  (let ((directory (ai-prompt-management--get-global-instructions-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--global-instructions-cache "global"))

  ;; Update local commands cache
  (when-let ((directory (ai-prompt-management--get-local-instructions-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--local-instructions-cache "local"))

  ;; Update default system prompts cache
  (let ((directory (ai-prompt-management--get-default-system-prompts-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--default-system-prompts-cache "default-system" t))

  ;; Update global system prompts cache
  (let ((directory (ai-prompt-management--get-global-system-prompts-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--global-system-prompts-cache "global-system" t))

  ;; Update local system prompts cache
  (when-let ((directory (ai-prompt-management--get-local-system-prompts-directory)))
    (ai-prompt-management--ensure-cache-updated directory ai-prompt-management--local-system-prompts-cache "local-system" t))

  (message "AI mode prompt caches updated!")
  (ai-telemetry--verbose-message "Prompt caches updated successfully."))

(provide 'ai-prompt-management)
