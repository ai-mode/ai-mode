;;; ai-mode-line.el --- AI mode line utilities and indicators -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, modeline

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
;; This module provides mode line utilities and indicators for AI mode,
;; including progress indicators, status displays, and integration with
;; doom-modeline.

;;; Code:

(require 'cl-lib)
(require 'ai-execution)
(require 'ai-context-management)
(require 'ai-model-management)

(defcustom ai-mode-line-progress-indicator-enabled t
  "Enable progress indicator for AI requests."
  :type 'boolean
  :group 'ai-mode)

(defcustom ai-mode-line-progress-indicator-style 'spinner
  "Style of progress indicator to use for AI requests."
  :type '(choice (const :tag "Spinner animation" spinner)
                 (const :tag "Progress dots" dots)
                 (const :tag "Message only" message))
  :group 'ai-mode)

(defvar ai-mode-line-progress-spinner-chars '("○" "◔" "◑" "◕" "●" "○" "◔" "◑" "◕" "●")
  "Characters used for spinner animation in progress indicator.")

;; Animation-only variables (buffer-local)
(defvar-local ai-mode-line-progress-timer nil
  "Timer for progress indicator animation.")

(defvar-local ai-mode-line-progress-counter 0
  "Counter for progress indicator animation.")

;; Progress indicator functions (buffer-local operations)
(defun ai-mode-line-format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let* ((elapsed (- (float-time) (float-time start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun ai-mode-line-progress-start (&optional message buffer)
  "Start progress indicator animation with optional MESSAGE in specified BUFFER or current buffer."
  (when ai-mode-line-progress-indicator-enabled
    (with-current-buffer (or buffer (current-buffer))
      (setq ai-mode-line-progress-counter 0)

      (cond
       ((eq ai-mode-line-progress-indicator-style 'spinner)
        (ai-mode-line-progress-start-spinner))
       ((eq ai-mode-line-progress-indicator-style 'dots)
        (ai-mode-line-progress-start-dots))
       ((eq ai-mode-line-progress-indicator-style 'message)
        (force-mode-line-update))))))

(defun ai-mode-line-progress-stop (&optional buffer)
  "Stop progress indicator animation in specified BUFFER or current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when ai-mode-line-progress-timer
      (cancel-timer ai-mode-line-progress-timer)
      (setq ai-mode-line-progress-timer nil))
    (force-mode-line-update)))

(defun ai-mode-line-progress-start-spinner ()
  "Start spinner-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai-mode-line-progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (let ((progress-info (ai-execution--get-progress-info)))
                                  (when (plist-get progress-info :active)
                                    (setq ai-mode-line-progress-counter (1+ ai-mode-line-progress-counter))
                                    (force-mode-line-update))))))))))

(defun ai-mode-line-progress-start-dots ()
  "Start dots-style progress indicator."
  (let ((current-buffer (current-buffer)))
    (setq ai-mode-line-progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (when (buffer-live-p current-buffer)
                              (with-current-buffer current-buffer
                                (let ((progress-info (ai-execution--get-progress-info)))
                                  (when (plist-get progress-info :active)
                                    (setq ai-mode-line-progress-counter (1+ ai-mode-line-progress-counter))
                                    (force-mode-line-update))))))))))

(defun ai-mode-line-progress-wrap-callback (original-callback &optional buffer)
  "Wrap ORIGINAL-CALLBACK to stop progress indicator when called in specified BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (lambda (&rest args)
      (ai-mode-line-progress-stop target-buffer)
      (when original-callback
        (apply original-callback args)))))

(defun ai-mode-line-get-project-context-indicator ()
  "Return a single character indicator for the current project context mode."
  (cond
   ((eq ai-context-management--project-context-mode 'full-project) "P")
   ((eq ai-context-management--project-context-mode 'project-ai-summary) "S")
   ((eq ai-context-management--project-context-mode 'disabled) "D")
   (t "")))

(defun ai-mode-line-info ()
  "Return a formatted string describing the current AI mode state for the mode line."
  (let* ((model (ai-model-management-get-current))
         (project-indicator (ai-mode-line-get-project-context-indicator))
         (cache-indicator (if (bound-and-true-p ai-execution--prompt-caching-enabled) "C" ""))
         (patch-indicator (if (bound-and-true-p ai-execution--replace-action-use-patch) "P" ""))
         (execution-progress-data (ai-execution--get-progress-info))
         (progress-indicator (cond
                              ((and (plist-get execution-progress-data :active)
                                    (eq ai-mode-line-progress-indicator-style 'spinner))
                               (let ((spinner-chars ai-mode-line-progress-spinner-chars)
                                     (elapsed-time (when (plist-get execution-progress-data :start-time)
                                                     (ai-mode-line-format-elapsed-time (plist-get execution-progress-data :start-time)))))
                                 (format "%s%s"
                                         (nth (% ai-mode-line-progress-counter (length spinner-chars)) spinner-chars)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              ((and (plist-get execution-progress-data :active)
                                    (eq ai-mode-line-progress-indicator-style 'dots))
                               (let ((elapsed-time (when (plist-get execution-progress-data :start-time)
                                                     (ai-mode-line-format-elapsed-time (plist-get execution-progress-data :start-time)))))
                                 (format "%s%s"
                                         (make-string (% ai-mode-line-progress-counter 4) ?.)
                                         (if elapsed-time (format ":%s" elapsed-time) ""))))
                              ((plist-get execution-progress-data :active) "⚡")
                              (t "")))
         (context-info (if (plist-get execution-progress-data :active)
                           (format "%s" progress-indicator)
                         (format "%s%s%s|%d/%d"
                                 project-indicator
                                 cache-indicator
                                 patch-indicator
                                 ai-context-management--current-precending-context-size
                                 ai-context-management--current-forwarding-context-size)))
         (ai-mode-line-section
          (format " AI[%s|%s]"
                  (map-elt model :name)
                  context-info)))
    ai-mode-line-section))

(defun ai-mode-line-update-mode-line-info ()
  "Force update of the mode line to reflect current AI mode state."
  (force-mode-line-update))

(defun ai-mode-line-initialize ()
  "Initialize AI mode line by registering callbacks with ai-execution."
  (ai-execution-register-progress-start-callback #'ai-mode-line-progress-start)
  (ai-execution-register-progress-stop-callback #'ai-mode-line-progress-stop))

;; Create aliases for backward compatibility with existing code
(defalias 'ai--progress-start #'ai-mode-line-progress-start)
(defalias 'ai--progress-stop #'ai-mode-line-progress-stop)
(defalias 'ai--progress-wrap-callback #'ai-mode-line-progress-wrap-callback)

(when (require 'doom-modeline nil 'noerror)

  (doom-modeline-def-segment ai-mode-line-info
    "Display AI mode line information."
    (ai-mode-line-info))

  (add-hook 'ai-model-management-change-hook 'doom-modeline-refresh-bars)
  (add-to-list 'mode-line-misc-info  '(:eval (ai-mode-line-info)) t))


(ai-mode-line-initialize)

(provide 'ai-mode-line)

;;; ai-mode-line.el ends here
