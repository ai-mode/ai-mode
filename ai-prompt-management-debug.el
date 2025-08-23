;;; ai-prompt-management-debug.el --- Debug utilities for AI prompt management -*- lexical-binding: t -*-

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
;; This module provides debug utilities for the AI prompt management system,
;; including cache inspection, statistics, and interactive debugging tools.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'ai-prompt-management)
(require 'ai-project)
(require 'ai-logging)

;; Face definitions for consistent styling
(defface ai-prompt-debug-main-header
  '((t :inherit magit-section-heading :foreground "brown" :weight bold))
  "Face for main section headers."
  :group 'ai-debug)

(defface ai-prompt-debug-category-header
  '((t :inherit magit-section-heading :foreground "brown"))
  "Face for category headers."
  :group 'ai-debug)

(defface ai-prompt-debug-file-header
  '((t :inherit magit-section-secondary-heading))
  "Face for file item headers."
  :group 'ai-debug)

(defface ai-prompt-debug-metadata
  '((t :inherit magit-dimmed))
  "Face for metadata information."
  :group 'ai-debug)

(defface ai-prompt-debug-empty
  '((t :inherit magit-dimmed :slant italic))
  "Face for empty or unavailable information."
  :group 'ai-debug)

(defvar ai-prompt-management-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "r") 'ai-prompt-management-debug-refresh)
    (define-key map (kbd "g") 'ai-prompt-management-debug-refresh)
    (define-key map (kbd "u") 'ai-prompt-management-debug-force-update-cache)
    (define-key map (kbd "c") 'ai-prompt-management-debug-clear-cache)
    map)
  "Keymap for AI prompt management debug mode.")

(define-derived-mode ai-prompt-management-debug-mode magit-section-mode "AI-Debug"
  "Major mode for debugging AI prompt management caches.

Key bindings:
\\{ai-prompt-management-debug-mode-map}"
  :group 'ai
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun ai-prompt-management-debug-refresh ()
  "Refresh the AI prompt management debug buffer."
  (interactive)
  (when (eq major-mode 'ai-prompt-management-debug-mode)
    (ai-prompt-management-debug-show-cache-status)))

(defun ai-prompt-management-debug-force-update-cache ()
  "Force update all caches and refresh debug buffer."
  (interactive)
  (when (eq major-mode 'ai-prompt-management-debug-mode)
    (ai-prompt-management-force-update-all-caches)
    (ai-prompt-management-debug-refresh)
    (message "All caches force updated")))

(defun ai-prompt-management-debug-clear-cache ()
  "Clear selected cache and refresh debug buffer."
  (interactive)
  (when (eq major-mode 'ai-prompt-management-debug-mode)
    (let ((locations '(("Default" . :default)
                      ("Global" . :global)
                      ("Local" . :local))))
      (when-let* ((selected (completing-read "Clear cache for location: "
                                           (mapcar #'car locations)))
                  (location (cdr (assoc selected locations))))
        (ai-prompt-management-invalidate-cache location)
        (ai-prompt-management-debug-refresh)
        (message "Cache cleared for %s" selected)))))

(defun ai-prompt-management-debug--insert-file-section (name file-path index content section-type)
  "Insert a section for a single file with NAME, FILE-PATH at INDEX with CONTENT and SECTION-TYPE."
  (let* ((metadata (ai-prompt-management-get-file-metadata file-path))
         (file-size (when metadata (plist-get metadata :size)))
         (modified-time (when metadata (plist-get metadata :modification-time)))
         (metadata-parts '()))

    ;; Build metadata for header - only non-path information
    (when file-size
      (push (format "size: %d bytes" file-size) metadata-parts))
    (when modified-time
      (push (format "modified: %s" (format-time-string "%Y-%m-%d %H:%M:%S" modified-time)) metadata-parts))

    (magit-insert-section (section-type (format "%d-%s" index name) t) ; Hidden by default
      (magit-insert-heading
        ;; Main part: full file path (bold)
        (propertize file-path 'face 'ai-prompt-debug-file-header)
        ;; Metadata part: additional attributes (dimmed)
        (when metadata-parts
          (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                 'face 'ai-prompt-debug-metadata))))

      ;; Insert file content
      (when content
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n")))
      (insert "\n"))))

(defun ai-prompt-management-debug--insert-location-section (location title)
  "Insert debug section for LOCATION with TITLE."
  (let* ((cache-info (ai-prompt-management-get-cache-info location))
         (instruction-count (plist-get cache-info :instruction-count))
         (system-prompt-count (plist-get cache-info :system-prompt-count))
         (directory (plist-get cache-info :directory))
         (directory-exists (plist-get cache-info :directory-exists))
         (cache-valid (plist-get cache-info :cache-valid))
         (last-update (plist-get cache-info :last-update))
         (total-count (+ instruction-count system-prompt-count))
         (metadata-parts '()))

    ;; Build metadata for header
    (when directory
      (push (format "path: %s" directory) metadata-parts))
    (push (format "exists: %s" (if directory-exists "yes" "no")) metadata-parts)
    (push (format "valid: %s" (if cache-valid "yes" "no")) metadata-parts)
    (when last-update
      (push (format "updated: %s" (format-time-string "%Y-%m-%d %H:%M:%S" last-update)) metadata-parts))

    (magit-insert-section (ai-location location t) ; Hidden by default
      (magit-insert-heading
        ;; Main part: cache name and count (bold)
        (propertize (format "%s Cache (%d items)" (upcase title) total-count)
                   'face 'ai-prompt-debug-category-header)
        ;; Metadata part: path and status information (dimmed)
        (when metadata-parts
          (concat " " (propertize (format "(%s)" (mapconcat #'identity metadata-parts " | "))
                                 'face 'ai-prompt-debug-metadata))))

      ;; Instructions section
      (when (> instruction-count 0)
        (magit-insert-section (ai-instructions)
          (magit-insert-heading
            (propertize (format "Instruction Files (%d items)" instruction-count)
                       'face 'ai-prompt-debug-file-header))
          (let ((instruction-files (ai-prompt-management-get-all-instruction-files location))
                (index 0))
            (dolist (file-info instruction-files)
              (let* ((name (car file-info))
                     (file-path (cdr file-info))
                     (content (ai-prompt-management--read-file-content file-path)))
                (ai-prompt-management-debug--insert-file-section name file-path index content 'ai-instruction-file)
                (setq index (1+ index)))))))

      ;; System prompts section
      (when (> system-prompt-count 0)
        (magit-insert-section (ai-system-prompts)
          (magit-insert-heading
            (propertize (format "System Prompt Files (%d items)" system-prompt-count)
                       'face 'ai-prompt-debug-file-header))
          (let ((system-prompt-names (ai-prompt-management--get-all-system-prompt-names-from-cache
                                    (cond
                                     ((eq location :default) 'default)
                                     ((eq location :global) 'global)
                                     ((eq location :local) 'local))))
                (directory-func (cond
                                ((eq location :default) 'ai-prompt-management--get-default-system-prompts-directory)
                                ((eq location :global) 'ai-prompt-management--get-global-system-prompts-directory)
                                ((eq location :local) 'ai-prompt-management--get-local-system-prompts-directory)))
                (index 0))
            (when directory-func
              (let ((system-dir (funcall directory-func)))
                (dolist (name (sort system-prompt-names #'string<))
                  (let* ((file-path (ai-prompt-management--get-file-path-for-name name system-dir))
                         (content (ai-prompt-management--read-file-content file-path)))
                    (ai-prompt-management-debug--insert-file-section name file-path index content 'ai-system-prompt-file)
                    (setq index (1+ index)))))))))

      (insert "\n"))))

(defun ai-prompt-management-debug--insert-summary-section ()
  "Insert summary section with overall statistics."
  (let* ((default-info (ai-prompt-management-get-cache-info :default))
         (global-info (ai-prompt-management-get-cache-info :global))
         (local-info (ai-prompt-management-get-cache-info :local))
         (total-instructions (+ (plist-get default-info :instruction-count)
                               (plist-get global-info :instruction-count)
                               (plist-get local-info :instruction-count)))
         (total-system-prompts (+ (plist-get default-info :system-prompt-count)
                                 (plist-get global-info :system-prompt-count)
                                 (plist-get local-info :system-prompt-count)))
         (total-items (+ total-instructions total-system-prompts))
         (project-root (ai-project--get-project-root)))

    (magit-insert-section (ai-summary)
      (magit-insert-heading
        (propertize (format "AI Prompt Cache Summary (%d total items)" total-items)
                   'face 'ai-prompt-debug-category-header))

      (magit-insert-section (ai-project-info)
        (magit-insert-heading
          (propertize "Project Information" 'face 'ai-prompt-debug-file-header))
        (insert (format "    Current Project: %s\n"
                        (if project-root
                            (file-name-nondirectory (directory-file-name project-root))
                          (propertize "No project detected" 'face 'ai-prompt-debug-empty))))
        (when project-root
          (insert (format "    Project Root: %s\n" project-root)))
        (insert "\n"))

      (magit-insert-section (ai-statistics)
        (magit-insert-heading
          (propertize "Cache Statistics" 'face 'ai-prompt-debug-file-header))
        (insert (format "    Total Instructions: %d\n" total-instructions))
        (insert (format "    Total System Prompts: %d\n" total-system-prompts))
        (insert (format "    Total Cached Items: %d\n" total-items))
        (insert "\n"))

      (insert "\n"))))

(defun ai-prompt-management-debug-show-cache-status ()
  "Show AI prompt management cache status in a dedicated buffer."
  (interactive)
  (let* ((buffer-name "*AI Prompt Cache Debug*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ai-prompt-management-debug-mode)

        ;; Set header-line-format with help information
        (setq header-line-format "AI Prompt Cache Debug - TAB to expand/collapse, r/g to refresh, u to force update, c to clear cache, q to quit")

        ;; Insert root section
        (magit-insert-section (ai-prompt-debug-root)
          (magit-insert-heading
            (propertize "AI Prompt Management Cache Debug" 'face 'ai-prompt-debug-main-header))

          ;; Insert summary section
          (ai-prompt-management-debug--insert-summary-section)

          ;; Insert location sections
          (ai-prompt-management-debug--insert-location-section :default "Default")
          (ai-prompt-management-debug--insert-location-section :global "Global")
          (ai-prompt-management-debug--insert-location-section :local "Local"))

        ;; Collapse all sections by default
        (goto-char (point-min))
        (condition-case-unless-debug nil
            (let ((root-section (magit-current-section)))
              (when root-section
                (dolist (child (oref root-section children))
                  (magit-section-hide child))))
          (error nil))

        (goto-char (point-min))))

    ;; Show buffer
    (pop-to-buffer buffer)))

(provide 'ai-prompt-management-debug)

;;; ai-prompt-management-debug.el ends here
