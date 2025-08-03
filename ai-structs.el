;;; ai-structs.el --- Data structures for AI mode -*- lexical-binding: t -*-

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
;; This module contains formal definitions of all common data structures
;; used throughout AI-mode, including the unified command system structures
;; and helper functions for working with these structures.

;;; Code:

(require 'cl-lib)

;; Unified command system structures

(cl-defstruct ai-command-behavior
  user-input            ; boolean: t if the command requires additional user input.
  action-type           ; string, optional: The high-level action type (e.g., "modify", "complete") used for structuring the request.
  result-action         ; symbol: Specifies how the AI's response should be handled.
                        ; Possible values: show, replace, eval, insert-at-point, complete.
  needs-buffer-context  ; boolean, optional: t if the full buffer content is required for context.
  needs-project-context ; boolean, optional: t if project-wide context is required.
  needs-global-context  ; boolean, optional: t if global AI context (e.g., global memory) is required.
  preceding-context-size ; integer or nil, optional: Number of lines for preceding context. Nil for full.
  following-context-size ; integer or nil, optional: Number of lines for following context. Nil for full.
  )

(cl-defstruct ai-command
  name                    ; Display name of the command
  canonical-name         ; Canonical name for file lookups
  base-name              ; Base name without modifiers
  instructions           ; string, optional: Specific instructions for the AI for this command.
  behavior               ; ai-command-behavior struct
  source                 ; Command source (:config/:file/:hybrid)
  location               ; Location (:default/:global/:local)
  file-path              ; Path to instruction file (if source is file)
  priority              ; Priority for sorting
  )

(cl-defstruct ai-modifier
  name           ; Modifier name
  type           ; Type (:action/:behavior/:context)
  config         ; Configuration to apply
  display-char   ; Character for UI display
  description    ; Modifier description
  conflicts      ; Conflicting modifiers
  )

(cl-defstruct ai-command-provider
  name           ; Provider name
  priority       ; Provider priority
  location       ; Location (:default/:global/:local/:config)
  loader-fn      ; Function to load commands
  )

(cl-defstruct ai-buffer-state
  ;; Buffer identification
  buf-obj                  ; The buffer object
  buf-name                 ; Buffer name (for identification if buffer becomes invalid)
  buf-filename             ; File name associated with buffer (nil for non-file buffers)

  ;; Cursor position
  cur-point                ; Current cursor position (point)
  cur-line                 ; Current line number (1-based)
  cur-column               ; Current column number (0-based)

  ;; Region state
  reg-active               ; t if region is active, nil otherwise
  reg-beginning            ; Beginning of the region if active, nil otherwise
  reg-end                  ; End of the region if active, nil otherwise
  reg-content              ; Content of selected region (if any)

  ;; Buffer bounds
  buf-point-min            ; point-min of the buffer
  buf-point-max            ; point-max of the buffer

  ;; Major mode information
  buf-major-mode           ; Major mode of the buffer

  ;; Markers for position tracking across buffer changes
  cur-point-marker         ; Marker at current point position
  reg-start-marker         ; Marker at region start (if region active)
  reg-end-marker           ; Marker at region end (if region active)
  )

;; Helper functions for working with ai-command structs

(defun ai-structs--get-command-name (ai-command)
  "Get the display name from AI-COMMAND struct.
Returns the name or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-name ai-command)))

(defun ai-structs--get-command-canonical-name (ai-command)
  "Get canonical name from AI-COMMAND struct.
Returns the canonical name or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-canonical-name ai-command)))

(defun ai-structs--get-command-base-name (ai-command)
  "Get base name (without modifiers) from AI-COMMAND struct.
Returns the base name or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-base-name ai-command)))

(defun ai-structs--get-command-source (ai-command)
  "Get source type from AI-COMMAND struct (:config/:file/:hybrid).
Returns the source or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-source ai-command)))

(defun ai-structs--get-command-location (ai-command)
  "Get location type from AI-COMMAND struct (:default/:global/:local).
Returns the location or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-location ai-command)))

(defun ai-structs--get-command-file-path (ai-command)
  "Get file path from AI-COMMAND struct.
Returns the file path or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-file-path ai-command)))

(defun ai-structs--get-command-priority (ai-command)
  "Get priority from AI-COMMAND struct.
Returns the priority or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-priority ai-command)))

(defun ai-structs--get-command-instructions (ai-command)
  "Get instructions string from AI-COMMAND struct.
Returns the instructions or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-instructions ai-command)))

(defun ai-structs--get-command-behavior (ai-command)
  "Get the ai-command-behavior struct from AI-COMMAND.
Returns the behavior struct or nil if AI-COMMAND is nil."
  (when (and ai-command (ai-command-p ai-command))
    (ai-command-behavior ai-command)))

(defun ai-structs--get-result-action (ai-command)
  "Get result action from AI-COMMAND struct.
Returns the result action or nil if AI-COMMAND is nil or its behavior has no result action."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-result-action behavior)))

(defun ai-structs--command-needs-user-input-p (ai-command)
  "Check if AI-COMMAND requires user input.
Returns t if user input is required, nil otherwise."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-user-input behavior)))

(defun ai-structs--command-needs-buffer-context-p (ai-command)
  "Check if AI-COMMAND needs buffer context.
Returns t if buffer context is needed, nil otherwise."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-needs-buffer-context behavior)))

(defun ai-structs--command-needs-project-context-p (ai-command)
  "Check if AI-COMMAND needs project context.
Returns t if project context is needed, nil otherwise."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-needs-project-context behavior)))

(defun ai-structs--command-needs-global-context-p (ai-command)
  "Check if AI-COMMAND needs global context.
Returns t if global context is needed, nil otherwise."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-needs-global-context behavior)))

(defun ai-structs--get-command-preceding-context-size (ai-command)
  "Get preceding context size from AI-COMMAND struct.
Returns the size or nil."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-preceding-context-size behavior)))

(defun ai-structs--get-command-following-context-size (ai-command)
  "Get following context size from AI-COMMAND struct.
Returns the size or nil."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-following-context-size behavior)))

(defun ai-structs--get-command-action-type (ai-command)
  "Get action type string from AI-COMMAND struct.
Returns the action type or nil."
  (when-let ((behavior (ai-structs--get-command-behavior ai-command)))
    (ai-command-behavior-action-type behavior)))

;; Helper function for backward compatibility with existing code that expects a plist config

(defun ai-structs--get-command-config (ai-command)
  "Get configuration from AI-COMMAND struct, converting to plist for backward compatibility.
This function is intended for internal use by modules that still expect a plist
configuration. New code should use direct accessor functions."
  (when (and ai-command (ai-command-p ai-command))
    (let ((behavior (ai-command-behavior ai-command))
          (config nil))
      ;; Add command-level properties
      (when-let ((instructions (ai-command-instructions ai-command)))
        (setq config (plist-put config :instructions instructions)))

      ;; Add behavior properties
      (when behavior
        (cl-loop for (key accessor) in
                 '(
                   (:user-input ai-command-behavior-user-input)
                   (:action-type ai-command-behavior-action-type)
                   (:result-action ai-command-behavior-result-action)
                   (:needs-buffer-context ai-command-behavior-needs-buffer-context)
                   (:needs-project-context ai-command-behavior-needs-project-context)
                   (:needs-global-context ai-command-behavior-needs-global-context)
                   (:preceding-context-size ai-command-behavior-preceding-context-size)
                   (:following-context-size ai-command-behavior-following-context-size)
                   )
                 when (funcall accessor behavior)
                 do (setq config (plist-put config key (funcall accessor behavior)))))
      config)))

;; Helper functions for buffer state

(defun ai-structs--create-buffer-state (&optional buffer)
  "Create a buffer state snapshot for BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((region-active (region-active-p))
           (point-pos (point))
           (line-num (line-number-at-pos point-pos))
           (col-num (current-column))
           (region-start (when region-active (region-beginning)))
           (region-end (when region-active (region-end))))
      (make-ai-buffer-state
       ;; Buffer identification
       :buf-obj (current-buffer)
       :buf-name (buffer-name)
       :buf-filename (buffer-file-name)

       ;; Cursor position
       :cur-point point-pos
       :cur-line line-num
       :cur-column col-num

       ;; Region state
       :reg-active region-active
       :reg-beginning region-start
       :reg-end region-end
       :reg-content (when region-active (buffer-substring region-start region-end))

       ;; Buffer bounds
       :buf-point-min (point-min)
       :buf-point-max (point-max)

       ;; Major mode information
       :buf-major-mode major-mode

       ;; Markers for position tracking
       :cur-point-marker (point-marker)
       :reg-start-marker (when region-active (copy-marker region-start))
       :reg-end-marker (when region-active (copy-marker region-end))))))

(defun ai-structs--buffer-state-valid-p (buffer-state)
  "Check if BUFFER-STATE is still valid (buffer exists and accessible)."
  (and buffer-state
       (ai-buffer-state-p buffer-state)
       (buffer-live-p (ai-buffer-state-buf-obj buffer-state))))

(defun ai-structs--get-current-position-from-state (buffer-state)
  "Get current position information from BUFFER-STATE, updated if markers moved.
Returns a plist with current position data or nil if state is invalid."
  (when (ai-structs--buffer-state-valid-p buffer-state)
    (let ((point-marker (ai-buffer-state-cur-point-marker buffer-state)))
      (when (and point-marker (marker-buffer point-marker))
        (with-current-buffer (marker-buffer point-marker)
          (list :point (marker-position point-marker)
                :line (line-number-at-pos (marker-position point-marker))
                :column (save-excursion
                         (goto-char (marker-position point-marker))
                         (current-column))))))))

(defun ai-structs--cleanup-buffer-state (buffer-state)
  "Clean up resources used by BUFFER-STATE (markers, etc.)."
  (when (and buffer-state (ai-buffer-state-p buffer-state))
    ;; Clean up markers to prevent memory leaks
    (when-let ((marker (ai-buffer-state-cur-point-marker buffer-state)))
      (set-marker marker nil))
    (when-let ((marker (ai-buffer-state-reg-start-marker buffer-state)))
      (set-marker marker nil))
    (when-let ((marker (ai-buffer-state-reg-end-marker buffer-state)))
      (set-marker marker nil))))


(provide 'ai-structs)

;;; ai-structs.el ends here
