;;; ai-project.el --- AI project management utilities -*- lexical-binding: t -*-
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
;; This file contains project management utilities for AI mode.
;; It provides functions for working with project files, filtering,
;; and creating project-related data structures.
;;
;; The main features include:
;; - Project root detection using Projectile
;; - Project file filtering and listing
;; - Creating typed structs for project files
;; - AI ignore file handling and pattern matching
;; - Integration with .gitignore and .ai-ignore files
;;
;;; Code:

(require 'ai-utils)
(require 'ai-common)
(require 'projectile nil 'noerror) ; Ensure projectile is loaded for project root detection.

(defcustom ai-project-ignore-file-name ".ai-ignore"
  "Name of the AI ignore file, typically in the project root."
  :type 'string
  :group 'ai-common)

(defcustom ai-project-global-ignore-patterns
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

(defcustom ai-project-global-ignore-files
  (list (expand-file-name "~/.global-gitignore")
        (expand-file-name "~/.global-ai-ignore"))
  "List of global ignore files to be processed.
These files contain patterns that apply to all projects."
  :type '(repeat file)
  :group 'ai-common)

(defun ai-project--get-project-root ()
  "Return the project root directory using Projectile, or nil if not in a project.
Requires Projectile to be loaded and active."
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(defun ai-project--read-ignore-patterns-from-file (file-path)
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
                (push (cons pattern is-negated) patterns))))
          (forward-line 1)))
      (nreverse patterns))))

(defun ai-project--read-ai-ignore-patterns (project-root)
  "Read patterns from PROJECT-ROOT/.ai-ignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file.
Lines starting with '#' are comments and are ignored. Empty lines are also ignored."
  (let ((ignore-file (expand-file-name ai-project-ignore-file-name project-root)))
    (ai-project--read-ignore-patterns-from-file ignore-file)))

(defun ai-project--get-global-hardcoded-patterns ()
  "Get global hardcoded ignore patterns.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
These patterns are never negated and come from `ai-project-global-ignore-patterns`."
  (mapcar (lambda (pattern) (cons pattern nil)) ai-project-global-ignore-patterns))

(defun ai-project--get-global-ignore-file-patterns ()
  "Get patterns from global ignore files.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Reads patterns from files specified in `ai-project-global-ignore-files`."
  (let ((all-patterns nil))
    (dolist (global-file ai-project-global-ignore-files)
      (when (file-exists-p global-file)
        (setq all-patterns (append all-patterns
                                   (ai-project--read-ignore-patterns-from-file global-file)))))
    all-patterns))

(defun ai-project--get-project-gitignore-patterns (project-root)
  "Get patterns from PROJECT-ROOT/.gitignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file."
  (let ((gitignore-file (expand-file-name ".gitignore" project-root)))
    (when (file-exists-p gitignore-file)
      (ai-project--read-ignore-patterns-from-file gitignore-file))))

(defun ai-project--get-project-ai-ignore-patterns (project-root)
  "Get patterns from PROJECT-ROOT/.ai-ignore.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Each pattern string is kept as read from the file."
  (ai-project--read-ai-ignore-patterns project-root))

(defun ai-project--get-all-ignore-patterns (project-root)
  "Get all ignore patterns from various sources.
Returns a list of cons cells: (PATTERN-STRING . IS-NEGATED-P).
Sources include:
1. Global hardcoded patterns from `ai-project-global-ignore-patterns`
2. Global ignore files from `ai-project-global-ignore-files`
3. Project .gitignore file
4. Project .ai-ignore file

Patterns are processed in this order, with later patterns potentially
overriding earlier ones via negation."
  (let ((all-patterns nil))

    ;; 1. Add global hardcoded patterns
    (setq all-patterns (append all-patterns (ai-project--get-global-hardcoded-patterns)))

    ;; 2. Add patterns from global ignore files
    (setq all-patterns (append all-patterns (ai-project--get-global-ignore-file-patterns)))

    ;; 3. Add patterns from project .gitignore
    (when-let ((gitignore-patterns (ai-project--get-project-gitignore-patterns project-root)))
      (setq all-patterns (append all-patterns gitignore-patterns)))

    ;; 4. Add patterns from project .ai-ignore (highest priority)
    (when-let ((ai-ignore-patterns (ai-project--get-project-ai-ignore-patterns project-root)))
      (setq all-patterns (append all-patterns ai-ignore-patterns)))

    all-patterns))

(defun ai-project--pattern-to-regexp (pattern)
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

(defun ai-project--get-all-project-paths (project-root)
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

(defun ai-project--filter-paths-by-patterns (paths patterns)
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
             (regexp (ai-project--pattern-to-regexp pattern-str)))
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

(defun ai-project--get-filtered-project-files (&optional relative-paths)
  "Get a list of all relevant files in the current project, filtered by all ignore patterns.
If RELATIVE-PATHS is non-nil, returns relative paths from project root.
Otherwise, returns absolute file paths.
Requires Projectile to be loaded."
  (interactive)
  (let ((project-root (ai-project--get-project-root)))
    (if project-root
        (let* ((all-relative-paths (ai-project--get-all-project-paths project-root))
               (ignore-patterns (ai-project--get-all-ignore-patterns project-root))
               (filtered-relative-paths (ai-project--filter-paths-by-patterns
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

(defun ai-project--test-get-filtered-project-files ()
  "Test function for `ai-project--get-filtered-project-files`.
  Displays the list of filtered project files in a new buffer."
  (interactive)
  (let ((files (ai-project--get-filtered-project-files)))
    (if files
        (with-temp-buffer
          (insert (mapconcat 'identity files "\n"))
          (display-buffer (current-buffer)))
      (message "No files found or not in a Projectile project."))))

(defun ai-project--list-filtered-project-files-to-console ()
  "List all relevant files in the current project, filtered by all ignore patterns, to the *Messages* buffer."
  (interactive)
  (let ((files (ai-project--get-filtered-project-files)))
    (if files
        (message "Filtered project files:\n%s" (mapconcat 'identity files "\n"))
      (message "No files found or not in a Projectile project."))
    nil))

(defun ai-project--get-filtered-project-files-as-structs ()
  "Get a list of typed structs containing filtered project files.
Each struct contains file path, content, and metadata.
Returns a list of typed structs with :type 'project-file."
  (interactive)
  (let* ((project-root (ai-project--get-project-root))
        (file-structs nil))
    (if project-root
        (let ((filtered-files (ai-project--get-filtered-project-files)))
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

(provide 'ai-project)

;;; ai-project.el ends heren
