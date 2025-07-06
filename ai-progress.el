;;; ai-progress.el --- Progress management for AI operations -*- lexical-binding: t -*-

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
;; This module provides comprehensive progress management for AI operations.
;; It handles two types of progress:
;; 1. Single request progress - for individual LLM queries
;; 2. Batch operation progress - for multi-file operations like project indexing
;;
;; The module provides a unified interface for progress tracking, display,
;; and callback management across all AI operations.

;;; Code:

(require 'cl-lib)

;; Single request progress tracking variables
(defvar ai-progress--single-message nil
  "Current progress message for single requests.")

(defvar ai-progress--single-start-time nil
  "Time when current single request started.")

(defvar ai-progress--single-target-buffer nil
  "Buffer where single request progress was initiated.")

;; Batch operation progress tracking variables
(defvar ai-progress--batch-message nil
  "Current progress message for batch operations.")

(defvar ai-progress--batch-start-time nil
  "Time when current batch operation started.")

(defvar ai-progress--batch-target-buffer nil
  "Buffer where batch operation progress was initiated.")

;; Timer-based progress variables (buffer-local)
(defvar-local ai-progress--timer nil
  "Timer for progress indicator animation.")

(defvar-local ai-progress--counter 0
  "Counter for progress indicator animation.")

(defvar-local ai-progress--single-active nil
  "Flag indicating if single request progress is currently active.")

(defvar-local ai-progress--batch-active nil
  "Flag indicating if batch operation progress is currently active.")

;; Batch progress count tracking variables (buffer-local)
(defvar-local ai-progress--total-count nil
  "Total number of items for the current batch operation.")

(defvar-local ai-progress--current-count 0
  "Number of items processed so far for the current batch operation.")

;; Callback registration system
(defvar ai-progress--start-callbacks nil
  "List of callback functions called when progress starts.")

(defvar ai-progress--stop-callbacks nil
  "List of callback functions called when progress stops.")

(defvar ai-progress--increment-callbacks nil
  "List of callback functions called when progress is incremented.")

;;; Callback registration functions

(defun ai-progress-register-start-callback (callback)
  "Register a CALLBACK function to be called when progress starts.
The callback will be called with arguments: (message buffer total-count current-count operation-type)."
  (when (functionp callback)
    (add-to-list 'ai-progress--start-callbacks callback)))

(defun ai-progress-register-stop-callback (callback)
  "Register a CALLBACK function to be called when progress stops.
The callback will be called with arguments: (buffer final-processed-count total-count operation-type)."
  (when (functionp callback)
    (add-to-list 'ai-progress--stop-callbacks callback)))

(defun ai-progress-register-increment-callback (callback)
  "Register a CALLBACK function to be called when progress is incremented.
The callback will be called with arguments: (buffer current-count total-count operation-type)."
  (when (functionp callback)
    (add-to-list 'ai-progress--increment-callbacks callback)))

;;; Core progress functions

(defun ai-progress-start-single-request (&optional message target-buffer)
  "Start progress tracking for a single AI request.
MESSAGE is the optional progress message.
TARGET-BUFFER is the buffer where progress should be displayed."
  (let ((buffer (or target-buffer (current-buffer))))
    (setq ai-progress--single-message (or message "AI operation in progress")
          ai-progress--single-start-time (current-time)
          ai-progress--single-target-buffer buffer)

    ;; Set buffer-local progress variables
    (with-current-buffer buffer
      (setq ai-progress--single-active t
            ai-progress--counter 0))

    ;; Call registered start callbacks
    (dolist (callback ai-progress--start-callbacks)
      (condition-case err
          (funcall callback message buffer nil 0 'single-request)
        (error (message "Error in progress start callback: %s" (error-message-string err)))))

    (force-mode-line-update)))

(defun ai-progress-start-batch-operation (&optional message target-buffer total-count)
  "Start progress tracking for a batch AI operation.
MESSAGE is the optional progress message.
TARGET-BUFFER is the buffer where progress should be displayed.
TOTAL-COUNT is the total number of items to be processed."
  (let ((buffer (or target-buffer (current-buffer))))
    (setq ai-progress--batch-message (or message "AI batch operation in progress")
          ai-progress--batch-start-time (current-time)
          ai-progress--batch-target-buffer buffer)

    ;; Set buffer-local progress variables
    (with-current-buffer buffer
      (setq ai-progress--batch-active t
            ai-progress--counter 0
            ai-progress--total-count total-count
            ai-progress--current-count 0)) ; Reset current count on start

    ;; Call registered start callbacks
    (dolist (callback ai-progress--start-callbacks)
      (condition-case err
          (funcall callback message buffer total-count 0 'batch-operation)
        (error (message "Error in progress start callback: %s" (error-message-string err)))))

    (force-mode-line-update)))

(defun ai-progress-increment-batch-processed (&optional target-buffer)
  "Increment the processed count for batch operations.
TARGET-BUFFER is the buffer where progress should be updated."
  (when (ai-progress-is-batch-operation-p)
    (let ((buffer (or target-buffer ai-progress--batch-target-buffer (current-buffer))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq ai-progress--current-count (1+ ai-progress--current-count))))

      ;; Call registered increment callbacks
      (dolist (callback ai-progress--increment-callbacks)
        (condition-case err
            (funcall callback buffer ai-progress--current-count ai-progress--total-count 'batch-operation)
          (error (message "Error in progress increment callback: %s" (error-message-string err)))))

      (force-mode-line-update))))

(defun ai-progress-stop-single-request (&optional target-buffer)
  "Stop progress tracking for a single AI request.
TARGET-BUFFER is the buffer where progress should be stopped."
  (when (ai-progress-is-single-request-p)
    (let ((buffer (or target-buffer ai-progress--single-target-buffer (current-buffer))))
      (when (or (null ai-progress--single-target-buffer)
                (eq buffer ai-progress--single-target-buffer))

        ;; Call registered stop callbacks before clearing state
        (dolist (callback ai-progress--stop-callbacks)
          (condition-case err
              (funcall callback buffer 0 nil 'single-request)
            (error (message "Error in progress stop callback: %s" (error-message-string err)))))

        (setq ai-progress--single-message nil
              ai-progress--single-start-time nil
              ai-progress--single-target-buffer nil)

        ;; Clean up buffer-local progress variables only if no active progress remains
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq ai-progress--single-active nil)
            (unless (or ai-progress--single-active ai-progress--batch-active)
              (when ai-progress--timer
                (cancel-timer ai-progress--timer)
                (setq ai-progress--timer nil)))))

        (force-mode-line-update)))))

(defun ai-progress-stop-batch-operation (&optional target-buffer)
  "Stop progress tracking for a batch AI operation.
TARGET-BUFFER is the buffer where progress should be stopped."
  (when (ai-progress-is-batch-operation-p)
    (let ((buffer (or target-buffer ai-progress--batch-target-buffer (current-buffer))))
      (when (or (null ai-progress--batch-target-buffer)
                (eq buffer ai-progress--batch-target-buffer))

        ;; Store current counts before clearing
        (let ((final-processed (if (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     ai-progress--current-count)
                                 0))
              (final-total (if (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 ai-progress--total-count)
                             nil)))

          ;; Call registered stop callbacks before clearing state
          (dolist (callback ai-progress--stop-callbacks)
            (condition-case err
                (funcall callback buffer final-processed final-total 'batch-operation)
              (error (message "Error in progress stop callback: %s" (error-message-string err)))))

          (setq ai-progress--batch-message nil
                ai-progress--batch-start-time nil
                ai-progress--batch-target-buffer nil)

          ;; Clean up buffer-local progress variables only if no active progress remains
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq ai-progress--batch-active nil
                    ai-progress--total-count nil
                    ai-progress--current-count 0)
              (unless (or ai-progress--single-active ai-progress--batch-active)
                (when ai-progress--timer
                  (cancel-timer ai-progress--timer)
                  (setq ai-progress--timer nil)))))

          (force-mode-line-update))))))

(defun ai-progress-wrap-callback (callback &optional target-buffer)
  "Wrap CALLBACK to stop single request progress in TARGET-BUFFER when called.
Returns the original callback if it's a function, or a simple progress stopper if callback is nil."
  (let ((buffer (or target-buffer (current-buffer))))
    (if callback
        (lambda (&rest args)
          (when (and (ai-progress-is-single-request-p)
                     (eq buffer ai-progress--single-target-buffer))
            (ai-progress-stop-single-request buffer))
          (apply callback args))
      (lambda (&rest args)
        (when (and (ai-progress-is-single-request-p)
                   (eq buffer ai-progress--single-target-buffer))
          (ai-progress-stop-single-request buffer))))))

(defun ai-progress-wrap-batch-callback (callback &optional target-buffer)
  "Wrap CALLBACK to stop batch operation progress in TARGET-BUFFER when called.
Returns the original callback if it's a function, or a simple progress stopper if callback is nil."
  (let ((buffer (or target-buffer (current-buffer))))
    (if callback
        (lambda (&rest args)
          (when (and (ai-progress-is-batch-operation-p)
                     (eq buffer ai-progress--batch-target-buffer))
            (ai-progress-stop-batch-operation buffer))
          (apply callback args))
      (lambda (&rest args)
        (when (and (ai-progress-is-batch-operation-p)
                   (eq buffer ai-progress--batch-target-buffer))
          (ai-progress-stop-batch-operation buffer))))))

(defun ai-progress-get-display-info ()
  "Get current progress information for display purposes.
Returns a plist with progress details, or nil if no progress.
The plist contains:
:message (string) - current message
:start-time (time) - start time of the operation
:active (boolean) - true if progress is active
:counter (integer) - current animation counter
:total (integer) - total number of items to process (nil for single requests)
:processed (integer) - number of items processed so far
:target-buffer (buffer) - buffer where progress was initiated
:operation-type (symbol) - type of operation: 'single-request or 'batch-operation"
  (cond
   ;; Prioritize batch operation display if both are active
   ((ai-progress-is-batch-operation-p)
    (let ((target-buffer (or ai-progress--batch-target-buffer (current-buffer))))
      `(:message ,ai-progress--batch-message
        :start-time ,ai-progress--batch-start-time
        :active ,(and (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-progress--batch-active))
        :counter ,(if (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-progress--counter)
                    0)
        :total ,(if (buffer-live-p target-buffer)
                    (with-current-buffer target-buffer
                      ai-progress--total-count)
                  nil)
        :processed ,(if (buffer-live-p target-buffer)
                        (with-current-buffer target-buffer
                          ai-progress--current-count)
                      0)
        :target-buffer ,target-buffer
        :operation-type batch-operation)))
   ((ai-progress-is-single-request-p)
    (let ((target-buffer (or ai-progress--single-target-buffer (current-buffer))))
      `(:message ,ai-progress--single-message
        :start-time ,ai-progress--single-start-time
        :active ,(and (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-progress--single-active))
        :counter ,(if (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-progress--counter)
                    0)
        :total nil
        :processed 0
        :target-buffer ,target-buffer
        :operation-type single-request)))
   (t nil)))

;;; Specific progress info functions for ai-mode-line.el

(defun ai-progress-get-batch-info ()
  "Get batch operation progress information for display.
Returns a plist with batch-specific progress details, or nil if no batch progress.
Format matches what ai-mode-line.el expects."
  (when (ai-progress-is-batch-operation-p)
    (let ((target-buffer (or ai-progress--batch-target-buffer (current-buffer))))
      `(:active t
        :total ,(if (buffer-live-p target-buffer)
                    (with-current-buffer target-buffer
                      ai-progress--total-count)
                  nil)
        :processed ,(if (buffer-live-p target-buffer)
                        (with-current-buffer target-buffer
                          ai-progress--current-count)
                      0)
        :start-time ,ai-progress--batch-start-time))))

(defun ai-progress-get-single-info ()
  "Get single request progress information for display.
Returns a plist with single-request progress details, or nil if no single request progress.
Format matches what ai-mode-line.el expects."
  (when (ai-progress-is-single-request-p)
    (let ((target-buffer (or ai-progress--single-target-buffer (current-buffer))))
      `(:active ,(and (buffer-live-p target-buffer)
                      (with-current-buffer target-buffer
                        ai-progress--single-active))
        :start-time ,ai-progress--single-start-time))))

;;; Utility functions

(defun ai-progress-is-active-p ()
  "Return non-nil if any progress tracking is currently active."
  (or (ai-progress-is-single-request-p)
      (ai-progress-is-batch-operation-p)))

(defun ai-progress-is-buffer-active-p (&optional buffer)
  "Return non-nil if progress animation is active in BUFFER."
  (let ((target-buffer (or buffer (current-buffer))))
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (or ai-progress--single-active ai-progress--batch-active)))))

(defun ai-progress-is-batch-operation-p ()
  "Return non-nil if current progress is for a batch operation."
  (and ai-progress--batch-message
       ai-progress--batch-target-buffer
       (buffer-live-p ai-progress--batch-target-buffer)
       (with-current-buffer ai-progress--batch-target-buffer
         ai-progress--batch-active)))

(defun ai-progress-is-single-request-p ()
  "Return non-nil if current progress is for a single request."
  (and ai-progress--single-message
       ai-progress--single-target-buffer
       (buffer-live-p ai-progress--single-target-buffer)
       (with-current-buffer ai-progress--single-target-buffer
         ai-progress--single-active)))

(defun ai-progress-get-elapsed-time ()
  "Get elapsed time since progress started.
Returns time in seconds as a float, or nil if no progress is active.
Prioritizes batch operation time if both are active."
  (cond
   ((ai-progress-is-batch-operation-p)
    (when ai-progress--batch-start-time
      (- (float-time) (float-time ai-progress--batch-start-time))))
   ((ai-progress-is-single-request-p)
    (when ai-progress--single-start-time
      (- (float-time) (float-time ai-progress--single-start-time))))
   (t nil)))

(defun ai-progress-format-elapsed-time (&optional start-time)
  "Format elapsed time since START-TIME (or current progress start) as human-readable string."
  (let* ((start (or start-time
                    (cond
                     ((ai-progress-is-batch-operation-p) ai-progress--batch-start-time)
                     ((ai-progress-is-single-request-p) ai-progress--single-start-time)
                     (t nil))))
         (elapsed (when start (- (float-time) (float-time start))))
         (minutes (when elapsed (floor (/ elapsed 60))))
         (seconds (when elapsed (floor (mod elapsed 60)))))
    (cond
     ((null elapsed) "")
     ((> minutes 0) (format "%dm%ds" minutes seconds))
     (t (format "%ds" seconds)))))

(provide 'ai-progress)

;;; ai-progress.el ends here
