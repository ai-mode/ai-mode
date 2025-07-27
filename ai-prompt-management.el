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
;; - Hook system for cache update notifications.
;;
;; Architecture Overview:
;;
;; Caching Architecture:
;; The module implements a three-tier caching system with separate caches for
;; instructions and system prompts:
;; - Default cache: Contains built-in instructions from the ai-mode package
;; - Global cache: User-wide instructions stored in ~/.ai/
;; - Local cache: Project-specific instructions in <project-root>/.ai/
;;
;; Each cache is implemented as a hash table with modification time tracking
;; for automatic invalidation and reloading when files change.
;;
;; Data Structure:
;; - Cache tables: Hash tables mapping normalized names to file contents
;; - Directory mtimes: Hash table tracking directory modification times
;; - File watchers: Hash table for optional file system watching
;;
;; Cache keys are normalized (spaces converted to underscores) for consistent
;; lookup across different naming conventions.
;;
;; Update Algorithm:
;; 1. Check directory modification time against cached mtime
;; 2. If changed, clear cache and rescan directory
;; 3. Load all matching files (.md extension, excluding .examples.md)
;; 4. Store content with normalized keys
;; 5. Update cached directory mtime
;; 6. Execute registered cache update hooks
;;
;; Search Priorities:
;; Content lookup follows priority order:
;; 1. Local project cache (highest priority)
;; 2. Global user cache
;; 3. Default package cache (lowest priority)
;;
;; This allows local customization to override global settings,
;; which in turn override package defaults.
;;
;; Name Normalization:
;; - Spaces in names are converted to underscores for file system compatibility
;; - Command names like "explain code" become "explain_code.md" on disk
;; - Lookup keys are normalized to ensure consistent matching
;; - Modifier patterns (double underscores) are preserved as-is
;;
;; Template Rendering:
;; Templates support {{:variable}} syntax where variables are replaced
;; with values from context keyword lists during rendering.
;;
;; Hook System:
;; The module provides a hook system for cache update notifications:
;; - ai-prompt-management-add-cache-update-hook: Register callbacks
;; - ai-prompt-management-remove-cache-update-hook: Unregister callbacks
;; - Hooks are called after each cache update with location and statistics

;;; Code:

(require 'cl-lib)
(require 'ai-project) ; For ai-project--get-project-root
(require 'ai-logging) ; For ai-logging--verbose-message

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

;; Hook system variables
(defvar ai-prompt-management--cache-update-hooks nil
  "List of functions to call when cache is updated.
Each function should accept three parameters:
- LOCATION (:default/:global/:local)
- CACHE-TYPE (:instruction/:system-prompt)
- STATS (plist with :entry-count and :directory keys)")

(defun ai-prompt-management-add-cache-update-hook (hook-function)
  "Add HOOK-FUNCTION to cache update hooks.
HOOK-FUNCTION should accept parameters (location cache-type stats)."
  (unless (functionp hook-function)
    (error "HOOK-FUNCTION must be a function: %s" hook-function))
  (unless (member hook-function ai-prompt-management--cache-update-hooks)
    (push hook-function ai-prompt-management--cache-update-hooks)))

(defun ai-prompt-management-remove-cache-update-hook (hook-function)
  "Remove HOOK-FUNCTION from cache update hooks."
  (setq ai-prompt-management--cache-update-hooks
        (delq hook-function ai-prompt-management--cache-update-hooks)))

(defun ai-prompt-management--call-cache-update-hooks (location cache-type stats)
  "Call all registered cache update hooks with LOCATION, CACHE-TYPE, and STATS."
  (dolist (hook ai-prompt-management--cache-update-hooks)
    (condition-case-unless-debug err
        (funcall hook location cache-type stats)
      (error
       (ai-logging--verbose-message "Error in cache update hook %s: %s" hook err)))))

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
  (when-let ((project-root (ai-project--get-project-root)))
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
  (when-let ((project-root (ai-project--get-project-root)))
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
            (let ((name base-name)) ; Keep filenames as-is without underscore-to-space conversion
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
    (ai-logging--verbose-message "Updated cache for %s (%d entries)" cache-key (hash-table-count cache-table))

    ;; Call hooks
    (let* ((entry-count (hash-table-count cache-table))
           (location (cond
                     ((string-match-p "^default" cache-key) :default)
                     ((string-match-p "^global" cache-key) :global)
                     ((string-match-p "^local" cache-key) :local)
                     (t :unknown)))
           (cache-type (if is-system-prompt-dir :system-prompt :instruction))
           (stats `(:entry-count ,entry-count :directory ,directory :cache-key ,cache-key)))
      (ai-prompt-management--call-cache-update-hooks location cache-type stats))))

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

(defun ai-prompt-management--find-instruction-by-base-name (base-name)
  "Find instruction file that starts with BASE-NAME.
Searches through all caches for files matching base-name pattern."
  (let ((local-dir (ai-prompt-management--get-local-instructions-directory))
        (global-dir (ai-prompt-management--get-global-instructions-directory))
        (default-dir (ai-prompt-management--get-default-instructions-directory)))

    ;; Helper function to find matching file in cache
    (cl-flet ((find-in-cache (cache prefix)
                (let (result)
                  (maphash (lambda (key value)
                            (when (string-prefix-p prefix key)
                              (setq result value)))
                          cache)
                  result)))

      ;; Search in priority order
      (or (when local-dir
            (ai-prompt-management--ensure-cache-updated
             local-dir ai-prompt-management--local-instructions-cache "local")
            (find-in-cache ai-prompt-management--local-instructions-cache base-name))
          (when global-dir
            (ai-prompt-management--ensure-cache-updated
             global-dir ai-prompt-management--global-instructions-cache "global")
            (find-in-cache ai-prompt-management--global-instructions-cache base-name))
          (progn
            (ai-prompt-management--ensure-cache-updated
             default-dir ai-prompt-management--default-instructions-cache "default")
            (find-in-cache ai-prompt-management--default-instructions-cache base-name))))))

(defun ai-prompt-management--get-instruction-content-with-fallback (name)
  "Get raw instruction content for NAME with fallback to base name search.
First tries exact match, then searches for files starting with base name."
  (or
   ;; Try exact match first
   (ai-prompt-management--get-instruction-content name)
   ;; Try to find files with base name as prefix
   (let ((base-name (ai-prompt-management--normalize-name-for-lookup name)))
     (ai-prompt-management--find-instruction-by-base-name base-name))))

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

;; Public API for specific cache access

(defun ai-prompt-management--get-instruction-from-default-cache (name)
  "Get instruction content from default cache for NAME.
Returns nil if not found."
  (ai-prompt-management--ensure-cache-updated
   (ai-prompt-management--get-default-instructions-directory)
   ai-prompt-management--default-instructions-cache
   "default")
  (gethash name ai-prompt-management--default-instructions-cache))

(defun ai-prompt-management--get-instruction-from-global-cache (name)
  "Get instruction content from global cache for NAME.
Returns nil if not found."
  (when-let ((global-dir (ai-prompt-management--get-global-instructions-directory)))
    (ai-prompt-management--ensure-cache-updated
     global-dir
     ai-prompt-management--global-instructions-cache
     "global")
    (gethash name ai-prompt-management--global-instructions-cache)))

(defun ai-prompt-management--get-instruction-from-local-cache (name)
  "Get instruction content from local cache for NAME.
Returns nil if not found."
  (when-let ((local-dir (ai-prompt-management--get-local-instructions-directory)))
    (ai-prompt-management--ensure-cache-updated
     local-dir
     ai-prompt-management--local-instructions-cache
     "local")
    (gethash name ai-prompt-management--local-instructions-cache)))

(defun ai-prompt-management--get-system-prompt-from-default-cache (name)
  "Get system prompt content from default cache for NAME.
Returns nil if not found. Normalizes NAME for lookup."
  (let ((normalized-name (ai-prompt-management--normalize-name-for-lookup name)))
    (ai-prompt-management--ensure-cache-updated
     (ai-prompt-management--get-default-system-prompts-directory)
     ai-prompt-management--default-system-prompts-cache
     "default-system"
     t)
    (gethash normalized-name ai-prompt-management--default-system-prompts-cache)))

(defun ai-prompt-management--get-system-prompt-from-global-cache (name)
  "Get system prompt content from global cache for NAME.
Returns nil if not found. Normalizes NAME for lookup."
  (let ((normalized-name (ai-prompt-management--normalize-name-for-lookup name)))
    (when-let ((global-dir (ai-prompt-management--get-global-system-prompts-directory)))
      (ai-prompt-management--ensure-cache-updated
       global-dir
       ai-prompt-management--global-system-prompts-cache
       "global-system"
       t)
      (gethash normalized-name ai-prompt-management--global-system-prompts-cache))))

(defun ai-prompt-management--get-system-prompt-from-local-cache (name)
  "Get system prompt content from local cache for NAME.
Returns nil if not found. Normalizes NAME for lookup."
  (let ((normalized-name (ai-prompt-management--normalize-name-for-lookup name)))
    (when-let ((local-dir (ai-prompt-management--get-local-system-prompts-directory)))
      (ai-prompt-management--ensure-cache-updated
       local-dir
       ai-prompt-management--local-system-prompts-cache
       "local-system"
       t)
      (gethash normalized-name ai-prompt-management--local-system-prompts-cache))))

(defun ai-prompt-management--get-all-instruction-names-from-cache (cache-type)
  "Get all instruction names from specified CACHE-TYPE.
CACHE-TYPE should be one of: 'default, 'global, 'local.
Returns a list of instruction names."
  (let ((cache-table (cond
                      ((eq cache-type 'default) ai-prompt-management--default-instructions-cache)
                      ((eq cache-type 'global) ai-prompt-management--global-instructions-cache)
                      ((eq cache-type 'local) ai-prompt-management--local-instructions-cache)
                      (t (error "Invalid cache type: %s" cache-type))))
        (directory (cond
                    ((eq cache-type 'default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq cache-type 'global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq cache-type 'local) (ai-prompt-management--get-local-instructions-directory))
                    (t (error "Invalid cache type: %s" cache-type))))
        (cache-key (cond
                    ((eq cache-type 'default) "default")
                    ((eq cache-type 'global) "global")
                    ((eq cache-type 'local) "local")
                    (t (error "Invalid cache type: %s" cache-type)))))
    (when directory
      (ai-prompt-management--ensure-cache-updated directory cache-table cache-key)
      (let ((names nil))
        (maphash (lambda (name _content) (push name names)) cache-table)
        names))))

(defun ai-prompt-management--get-all-system-prompt-names-from-cache (cache-type)
  "Get all system prompt names from specified CACHE-TYPE.
CACHE-TYPE should be one of: 'default, 'global, 'local.
Returns a list of system prompt names."
  (let ((cache-table (cond
                      ((eq cache-type 'default) ai-prompt-management--default-system-prompts-cache)
                      ((eq cache-type 'global) ai-prompt-management--global-system-prompts-cache)
                      ((eq cache-type 'local) ai-prompt-management--local-system-prompts-cache)
                      (t (error "Invalid cache type: %s" cache-type))))
        (directory (cond
                    ((eq cache-type 'default) (ai-prompt-management--get-default-system-prompts-directory))
                    ((eq cache-type 'global) (ai-prompt-management--get-global-system-prompts-directory))
                    ((eq cache-type 'local) (ai-prompt-management--get-local-system-prompts-directory))
                    (t (error "Invalid cache type: %s" cache-type))))
        (cache-key (cond
                    ((eq cache-type 'default) "default-system")
                    ((eq cache-type 'global) "global-system")
                    ((eq cache-type 'local) "local-system")
                    (t (error "Invalid cache type: %s" cache-type)))))
    (when directory
      (ai-prompt-management--ensure-cache-updated directory cache-table cache-key t)
      (let ((names nil))
        (maphash (lambda (name _content) (push name names)) cache-table)
        names))))

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
  (ai-logging--verbose-message "Prompt caches updated successfully."))

;; =============================================================================
;; Public API for command providers and new unified system
;; =============================================================================

(defun ai-prompt-management-get-instruction-file-path (name location)
  "Get file path for instruction NAME at LOCATION (:default/:global/:local).
Returns the full file path or nil if location is not available."
  (let ((directory (cond
                    ((eq location :default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq location :global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq location :local) (ai-prompt-management--get-local-instructions-directory))
                    (t nil))))
    (when directory
      (ai-prompt-management--get-file-path-for-name name directory))))

(defun ai-prompt-management-get-instruction-file-exists-p (name location)
  "Check if instruction file for NAME exists at LOCATION (:default/:global/:local).
Returns t if file exists and is readable, nil otherwise."
  (when-let ((file-path (ai-prompt-management-get-instruction-file-path name location)))
    (file-readable-p file-path)))

(defun ai-prompt-management-get-examples-file-path (name location)
  "Get file path for examples NAME at LOCATION (:default/:global/:local).
Returns the full file path or nil if location is not available."
  (let ((directory (cond
                    ((eq location :default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq location :global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq location :local) (ai-prompt-management--get-local-instructions-directory))
                    (t nil))))
    (when directory
      (ai-prompt-management--get-file-path-for-name name directory t))))

(defun ai-prompt-management-get-examples-file-exists-p (name location)
  "Check if examples file for NAME exists at LOCATION (:default/:global/:local).
Returns t if file exists and is readable, nil otherwise."
  (when-let ((file-path (ai-prompt-management-get-examples-file-path name location)))
    (file-readable-p file-path)))

(defun ai-prompt-management-get-all-instruction-files (location)
  "Get all instruction files from LOCATION as list of (name . file-path) pairs.
LOCATION should be one of :default, :global, :local.
Returns empty list if location is not available."
  (let ((directory (cond
                    ((eq location :default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq location :global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq location :local) (ai-prompt-management--get-local-instructions-directory))
                    (t nil)))
        (cache-type (cond
                     ((eq location :default) 'default)
                     ((eq location :global) 'global)
                     ((eq location :local) 'local)
                     (t nil))))
    (when (and directory cache-type)
      (let ((names (ai-prompt-management--get-all-instruction-names-from-cache cache-type))
            (result nil))
        (dolist (name names)
          (when-let ((file-path (ai-prompt-management--get-file-path-for-name name directory)))
            (push (cons name file-path) result)))
        result))))

(defun ai-prompt-management-invalidate-cache (location)
  "Invalidate cache for specific LOCATION (:default/:global/:local).
Forces cache rebuild on next access."
  (let ((cache-key (cond
                    ((eq location :default) "default")
                    ((eq location :global) "global")
                    ((eq location :local) "local")
                    (t (error "Invalid location: %s" location))))
        (instruction-cache (cond
                           ((eq location :default) ai-prompt-management--default-instructions-cache)
                           ((eq location :global) ai-prompt-management--global-instructions-cache)
                           ((eq location :local) ai-prompt-management--local-instructions-cache)
                           (t (error "Invalid location: %s" location))))
        (system-cache (cond
                       ((eq location :default) ai-prompt-management--default-system-prompts-cache)
                       ((eq location :global) ai-prompt-management--global-system-prompts-cache)
                       ((eq location :local) ai-prompt-management--local-system-prompts-cache)
                       (t (error "Invalid location: %s" location))))
        (system-cache-key (cond
                           ((eq location :default) "default-system")
                           ((eq location :global) "global-system")
                           ((eq location :local) "local-system")
                           (t (error "Invalid location: %s" location)))))
    ;; Clear caches
    (clrhash instruction-cache)
    (clrhash system-cache)
    ;; Remove modification time entries to force rebuild
    (remhash cache-key ai-prompt-management--instruction-directory-mtimes)
    (remhash system-cache-key ai-prompt-management--instruction-directory-mtimes)
    (ai-logging--verbose-message "Invalidated cache for location: %s" location)

    ;; Call hooks for invalidation
    (ai-prompt-management--call-cache-update-hooks location :instruction `(:entry-count 0 :action invalidate))
    (ai-prompt-management--call-cache-update-hooks location :system-prompt `(:entry-count 0 :action invalidate))))

(defun ai-prompt-management-is-cache-valid-p (location)
  "Check if cache for LOCATION (:default/:global/:local) is valid.
Returns t if cache is up-to-date, nil if it needs rebuilding."
  (let ((directory (cond
                    ((eq location :default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq location :global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq location :local) (ai-prompt-management--get-local-instructions-directory))
                    (t nil)))
        (cache-key (cond
                    ((eq location :default) "default")
                    ((eq location :global) "global")
                    ((eq location :local) "local")
                    (t nil))))
    (and directory cache-key
         (not (ai-prompt-management--directory-needs-cache-update-p directory cache-key)))))

(defun ai-prompt-management-get-cache-info (location)
  "Get information about cache state for LOCATION (:default/:global/:local).
Returns plist with cache statistics and status."
  (let ((instruction-cache (cond
                           ((eq location :default) ai-prompt-management--default-instructions-cache)
                           ((eq location :global) ai-prompt-management--global-instructions-cache)
                           ((eq location :local) ai-prompt-management--local-instructions-cache)
                           (t nil)))
        (system-cache (cond
                       ((eq location :default) ai-prompt-management--default-system-prompts-cache)
                       ((eq location :global) ai-prompt-management--global-system-prompts-cache)
                       ((eq location :local) ai-prompt-management--local-system-prompts-cache)
                       (t nil)))
        (directory (cond
                    ((eq location :default) (ai-prompt-management--get-default-instructions-directory))
                    ((eq location :global) (ai-prompt-management--get-global-instructions-directory))
                    ((eq location :local) (ai-prompt-management--get-local-instructions-directory))
                    (t nil)))
        (cache-key (cond
                    ((eq location :default) "default")
                    ((eq location :global) "global")
                    ((eq location :local) "local")
                    (t nil))))
    (when (and instruction-cache system-cache cache-key)
      `(:location ,location
        :directory ,directory
        :directory-exists ,(and directory (file-directory-p directory))
        :instruction-count ,(hash-table-count instruction-cache)
        :system-prompt-count ,(hash-table-count system-cache)
        :cache-valid ,(ai-prompt-management-is-cache-valid-p location)
        :last-update ,(gethash cache-key ai-prompt-management--instruction-directory-mtimes)))))

(defun ai-prompt-management-get-file-metadata (file-path)
  "Get metadata for instruction file at FILE-PATH.
Returns plist with file size, modification time, and other attributes."
  (when (and file-path (file-exists-p file-path))
    (let ((attributes (file-attributes file-path)))
      `(:file-path ,file-path
        :exists t
        :readable ,(file-readable-p file-path)
        :writable ,(file-writable-p file-path)
        :size ,(file-attribute-size attributes)
        :modification-time ,(file-attribute-modification-time attributes)
        :access-time ,(file-attribute-access-time attributes)
        :file-type ,(file-attribute-type attributes)
        :permissions ,(file-attribute-modes attributes)))))

(defun ai-prompt-management-force-update-all-caches ()
  "Force update of all caches regardless of modification times.
This is useful for debugging or when file system changes are not detected properly."
  (interactive)
  (message "Force updating all AI mode prompt caches...")

  ;; Clear all modification time entries to force updates
  (clrhash ai-prompt-management--instruction-directory-mtimes)

  ;; Update all caches
  (ai-prompt-management--update-caches)

  (message "All AI mode prompt caches force updated!"))

(defun ai-prompt-management-list-cache-update-hooks ()
  "List all registered cache update hooks.
Returns list of hook functions for debugging purposes."
  ai-prompt-management--cache-update-hooks)

(defun ai-prompt-management-show-cache-contents ()
  "Display contents of all prompt caches in *Messages* buffer."
  (interactive)
  (message "=== AI Prompt Management Cache Contents ===")

  ;; Helper function to show cache contents
  (cl-flet ((show-cache-contents (cache-name cache-table directory)
              (message "\n--- %s Cache ---" cache-name)
              (if directory
                  (message "Directory: %s" directory)
                (message "Directory: Not available"))
              (message "Entries: %d" (hash-table-count cache-table))
              (if (> (hash-table-count cache-table) 0)
                  (let ((names nil))
                    (maphash (lambda (name _content) (push name names)) cache-table)
                    (setq names (sort names #'string<))
                    (dolist (name names)
                      (message "  - %s" name)))
                (message "  (empty)"))))

    ;; Show instruction caches
    (message "\n========== INSTRUCTION CACHES ==========")

    ;; Show default instruction cache
    (show-cache-contents "Default Instructions"
                        ai-prompt-management--default-instructions-cache
                        (ai-prompt-management--get-default-instructions-directory))

    ;; Show global instruction cache
    (show-cache-contents "Global Instructions"
                        ai-prompt-management--global-instructions-cache
                        (ai-prompt-management--get-global-instructions-directory))

    ;; Show local instruction cache
    (show-cache-contents "Local Instructions"
                        ai-prompt-management--local-instructions-cache
                        (ai-prompt-management--get-local-instructions-directory))

    ;; Show system prompt caches
    (message "\n========== SYSTEM PROMPT CACHES ==========")

    ;; Show default system prompt cache
    (show-cache-contents "Default System Prompts"
                        ai-prompt-management--default-system-prompts-cache
                        (ai-prompt-management--get-default-system-prompts-directory))

    ;; Show global system prompt cache
    (show-cache-contents "Global System Prompts"
                        ai-prompt-management--global-system-prompts-cache
                        (ai-prompt-management--get-global-system-prompts-directory))

    ;; Show local system prompt cache
    (show-cache-contents "Local System Prompts"
                        ai-prompt-management--local-system-prompts-cache
                        (ai-prompt-management--get-local-system-prompts-directory))

    ;; Show summary
    (let ((total-instructions (+ (hash-table-count ai-prompt-management--default-instructions-cache)
                                (hash-table-count ai-prompt-management--global-instructions-cache)
                                (hash-table-count ai-prompt-management--local-instructions-cache)))
          (total-system-prompts (+ (hash-table-count ai-prompt-management--default-system-prompts-cache)
                                  (hash-table-count ai-prompt-management--global-system-prompts-cache)
                                  (hash-table-count ai-prompt-management--local-system-prompts-cache))))
      (message "\n========== SUMMARY ==========")
      (message "Total Instructions: %d" total-instructions)
      (message "Total System Prompts: %d" total-system-prompts)
      (message "Total Cached Items: %d" (+ total-instructions total-system-prompts))
      (message "========================================="))))


(provide 'ai-prompt-management)

;;; ai-prompt-management.el ends here
