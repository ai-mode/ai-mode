;;; ai-usage.el --- Usage statistics and telemetry for AI mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: help, tools, AI, usage, statistics

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
;; This module handles usage statistics and token counting for AI mode.
;; Features include:
;; - Token usage tracking and reporting
;; - Usage statistics display
;; - Session statistics management
;; - Cache hit/miss tracking

;;; Code:

(require 'cl-lib)

(defcustom ai-usage-enabled t
  "Enable usage statistics collection for AI mode.
When enabled, collects usage statistics and performance metrics."
  :type 'boolean
  :group 'ai-mode)

(defvar ai-usage--session-stats nil
  "Session-wide usage statistics.")

(defun ai-usage-create-usage-statistics-callback ()
  "Create a callback function for displaying usage statistics."
  (lambda (usage-stats)
    (let ((input-tokens (plist-get usage-stats :input-tokens))
          (output-tokens (plist-get usage-stats :output-tokens))
          (total-tokens (plist-get usage-stats :total-tokens))
          (input-tokens-write-cache (plist-get usage-stats :input-tokens-write-cache))
          (input-tokens-read-cache (plist-get usage-stats :input-tokens-read-cache)))
      (message "AI Usage: Input=%s, Output=%s, Total=%s%s%s"
               (or input-tokens "?")
               (or output-tokens "?")
               (or total-tokens "?")
               (if input-tokens-write-cache (format ", Cache Write=%s" input-tokens-write-cache) "")
               (if input-tokens-read-cache (format ", Cache Read=%s" input-tokens-read-cache) ""))
      (ai-usage--record-usage-stats usage-stats))))

(defun ai-usage--record-usage-stats (usage-stats)
  "Record USAGE-STATS to session telemetry data."
  (when ai-usage-enabled
    (unless ai-usage--session-stats
      (setq ai-usage--session-stats
            '(:total-requests 0
              :total-input-tokens 0
              :total-output-tokens 0
              :total-cache-read-tokens 0
              :total-cache-write-tokens 0
              :session-start-time nil)))

    (unless (plist-get ai-usage--session-stats :session-start-time)
      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :session-start-time (current-time))))

    (let ((input-tokens (or (plist-get usage-stats :input-tokens) 0))
          (output-tokens (or (plist-get usage-stats :output-tokens) 0))
          (cache-read-tokens (or (plist-get usage-stats :input-tokens-read-cache) 0))
          (cache-write-tokens (or (plist-get usage-stats :input-tokens-write-cache) 0)))

      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :total-requests
                       (1+ (plist-get ai-usage--session-stats :total-requests))))
      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :total-input-tokens
                       (+ (plist-get ai-usage--session-stats :total-input-tokens) input-tokens)))
      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :total-output-tokens
                       (+ (plist-get ai-usage--session-stats :total-output-tokens) output-tokens)))
      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :total-cache-read-tokens
                       (+ (plist-get ai-usage--session-stats :total-cache-read-tokens) cache-read-tokens)))
      (setq ai-usage--session-stats
            (plist-put ai-usage--session-stats :total-cache-write-tokens
                       (+ (plist-get ai-usage--session-stats :total-cache-write-tokens) cache-write-tokens))))))

(defun ai-usage-show-session-stats ()
  "Display session usage statistics."
  (interactive)
  (if (and ai-usage-enabled ai-usage--session-stats)
      (let* ((total-requests (plist-get ai-usage--session-stats :total-requests))
             (total-input (plist-get ai-usage--session-stats :total-input-tokens))
             (total-output (plist-get ai-usage--session-stats :total-output-tokens))
             (total-cache-read (plist-get ai-usage--session-stats :total-cache-read-tokens))
             (total-cache-write (plist-get ai-usage--session-stats :total-cache-write-tokens))
             (session-start (plist-get ai-usage--session-stats :session-start-time))
             (session-duration (when session-start
                                 (float-time (time-subtract (current-time) session-start))))
             (duration-str (when session-duration
                            (if (< session-duration 60)
                                (format "%.1f seconds" session-duration)
                              (format "%.1f minutes" (/ session-duration 60.0))))))
        (message "AI Session Stats: %d requests, %d input tokens, %d output tokens%s%s%s"
                 total-requests
                 total-input
                 total-output
                 (if (> total-cache-read 0) (format ", %d cache read tokens" total-cache-read) "")
                 (if (> total-cache-write 0) (format ", %d cache write tokens" total-cache-write) "")
                 (if duration-str (format " (session: %s)" duration-str) "")))
    (message "No usage data available or usage tracking disabled")))

(defun ai-usage-reset-session-stats ()
  "Reset session usage statistics."
  (interactive)
  (setq ai-usage--session-stats nil)
  (message "AI usage session statistics reset"))

(defun ai-usage-toggle ()
  "Toggle usage statistics collection on/off."
  (interactive)
  (setq ai-usage-enabled (not ai-usage-enabled))
  (customize-save-variable 'ai-usage-enabled ai-usage-enabled)
  (message "AI usage statistics %s" (if ai-usage-enabled "enabled" "disabled")))

(defun ai-usage-get-session-stats ()
  "Return current session usage statistics."
  ai-usage--session-stats)

(provide 'ai-usage)

;;; ai-usage.el ends here
