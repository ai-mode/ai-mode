;;; ai-completions.el --- Code autocompletion via AI -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: help, tools

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
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
;; This package provides interactive, multi-candidate code completion capabilities
;; using various AI engines within Emacs. It manages the full lifecycle of a
;; completion session, including:
;; - Requesting completion candidates from AI models based on buffer context.
;; - Displaying real-time previews of suggestions using overlays.
;; - Allowing users to navigate through multiple candidates (next/previous).
;; - Applying the selected completion to the buffer.
;; - Dynamic adjustment of preceding and following context sizes.
;; - Integration of user instructions during an active session.
;; - A dedicated minor mode (`ai-completions-mode`) with custom keybindings
;;   for seamless interaction and session management.
;; This enhances coding efficiency by providing an intuitive and powerful AI-driven
;; code suggestion system.
;;

;;; Code:

(require 'ai-common)
(require 'ai-structs)
(require 'ai-model-management)
(require 'ai-execution)
(require 'ai-context-management)
(require 'ai-command-management)
(require 'ai-usage)
(require 'ai-progress)
(require 'ai-telemetry)
(require 'ai-logging)
(require 'ai-request-audit)

(defgroup ai-completions nil
  "AI code completion tool for Emacs."
  :group 'ai-mode)

(defcustom ai-completions--context-size-step 10
  "Step size for increasing context during completion."
  :type 'integer
  :group 'ai-completions)

(defvar-local ai-completions--current-command-struct nil
  "Current command struct for the completion process.")

(defvar-local ai-completions--current-precending-context-size nil
  "Current preceding context size for completion.")

(defvar-local ai-completions--current-forwarding-context-size nil
  "Current following context size for completion.")

(defvar-local ai-completions--use-full-context nil
  "Flag indicating whether to use full context for current completion.")

(defvar ai-completions--global-system-instructions '()
  "Global system instructions for AI completion.")

(defvar-local ai-completions--current-buffer-clone nil
  "Clone of the current buffer used for processing complete operations.")

(defvar ai-completions--models-providers nil
  "List of providers for AI models.")

(defcustom ai-completions--continue-commands '(not save-buffer save-some-buffers
                                                   save-buffers-kill-terminal
                                                   save-buffers-kill-emacs)
  "A list of commands that are allowed during completion."
  :type '(choice (const :tag "Any command" t)
                 (cons  :tag "Any except"
                        (const not)
                        (repeat :tag "Commands" function))
                 (repeat :tag "Commands" function))
  :group 'ai-completions)

(defcustom ai-completions--abort-commands '(not ai-completions--select-next-or-abort
                                                ai-completions--select-prev-or-abort
                                                ai-completions--select-current
                                                ai-completions--increase-current-context
                                                ai-completions--maximize-current-context
                                                ai-completions--add-instruction
                                                ai-completions--complete-at-point-with-full-context
                                                ai-completions-complete-code-at-point
                                                ai-completions--complete-at-point-with-limited-context
                                                self-insert-command
                                                exit-minibuffer
                                                delete-backward-char
                                                ignore
                                                switch-to-buffer
                                                other-window
                                                other-buffer
                                                mwheel-scroll)
  "A list of commands that abort completion."
  :type '(choice (const :tag "Any command" t)
                 (cons  :tag "Any except"
                        (const not)
                        (repeat :tag "Commands" function))
                 (repeat :tag "Commands" function))
  :group 'ai-completions)

(defvar-local ai-completions--candidates '()
  "List of candidate completions.")

(defvar-local ai-completions--current-candidate 0
  "Index of the current candidate being reviewed.")

(defvar-local ai-completions--active nil
  "Flag indicating whether a completion session is active.")

(defvar ai-completions--emulation-alist '((t . nil))
  "Emulation alist used for managing keymap overrides.")

(defvar-local ai-completions--preview-overlay nil
  "Overlay showing the preview of the current candidate.")

(defvar-local ai-completions--pseudo-tooltip-overlay nil
  "Overlay for displaying pseudo-tooltip for previews.")

(defvar-local ai-completions--complete-at-point nil
  "Point at which completion occurs.")

(defvar-local ai-completions--activated-keymap nil
  "Current activated keymap for completion handling.")

(defvar-local ai-completions--preview-at-point nil
  "Point at which the completion preview should be displayed.")

(defvar-local ai-completions--strategy nil
  "Current completion strategy.")

(defvar ai-completions--mode-keymap
  (let ((keymap (make-sparse-keymap)))
    ;; Define keys for managing candidates
    (define-key keymap "\e\e\e" 'ai-completions--abort)
    (define-key keymap "\C-g" 'ai-completions--abort)
    (define-key keymap (kbd "C-n") 'ai-completions--select-next-or-abort)
    (define-key keymap (kbd "C-p") 'ai-completions--select-prev-or-abort)
    (define-key keymap (kbd "<down>") 'ai-completions--select-next-or-abort)
    (define-key keymap (kbd "<up>") 'ai-completions--select-prev-or-abort)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [return] 'ai-completions--select-current)
    (define-key keymap (kbd "RET") 'ai-completions--select-current)
    (define-key keymap (kbd "C-i") 'ai-completions--increase-current-context)
    (define-key keymap (kbd "C-f") 'ai-completions--maximize-current-context)
    (define-key keymap (kbd "C-<tab>") 'ai-completions--complete-at-point-with-limited-context)
    (define-key keymap (kbd "C-M-<tab>") 'ai-completions--complete-at-point-with-full-context)
    (define-key keymap (kbd "C-a") 'ai-completions--add-instruction)
    keymap)
  "Keymap used for managing complete candidates.")

(defun ai-completions-complete-code-at-point ()
  "Start a completion at the current cursor position."
  (interactive)
  (ai-completions--coordinator :action-type "complete"))

(defun ai-completions--complete-at-point-with-limited-context ()
  "Perform autocompletion with limited context:
uses the active region if available, otherwise restricts the context around the cursor."
  (interactive)
  (ai-completions--coordinator :action-type "complete"))

(defun ai-completions--complete-at-point-with-full-context ()
  "Perform autocompletion with full file context."
  (interactive)
  (let* ((ai-completions--use-full-context t))
    (ai-completions--coordinator :action-type "complete")))

(cl-defun ai-completions--coordinator (&key (action-type nil) (strategy nil))
  "Decide whether to start a new completion process or continue the previous one.
ACTION-TYPE specifies what kind of action to perform.
STRATEGY may be specified to alter completion behavior."
  (condition-case-unless-debug err
      (progn
        (ai-logging--message 'info "completion" "Starting coordinator for action-type: %s, strategy: %s" action-type strategy)
        (if (or (ai-completions--is-new-completion-required)
                (not (ai-completions--should-continue this-command)))
            (ai-completions--begin :action-type action-type :strategy strategy)
          (ai-completions--continue)))
    (error (ai-logging--message 'info  "completion"  "An error occurred in ai-completions--coordinator => %s" (error-message-string err))
           (ai-completions--cancel))))

(defun ai-completions--get-or-create-completion-command (action-type)
  "Get or create completion command struct for ACTION-TYPE."
  (ai-completions--create-default-completion-command action-type))

(defun ai-completions--create-default-completion-command (action-type)
  "Create default completion command struct for ACTION-TYPE."

  (let* ((use-buffer-context ai-completions--use-full-context)
         (preceding-size (unless use-buffer-context
                           ai-context-management--current-precending-context-size))
         (following-size (unless use-buffer-context
                           ai-context-management--current-forwarding-context-size))
         (behavior (make-ai-command-behavior
                    :user-input nil
                    :action-type "complete"
                    :result-action 'complete
                    :needs-buffer-context use-buffer-context
                    :needs-project-context nil
                    :needs-global-context nil
                    :preceding-context-size preceding-size
                    :following-context-size following-size))
         (priority 100))
    (make-ai-command
     :name action-type
     :canonical-name action-type
     :base-name action-type
     :instructions nil
     :behavior behavior
     :source :completion
     :location :dynamic
     :file-path nil
     :priority priority)))

(cl-defun ai-completions--begin (&key (action-type nil) (strategy nil))
  "Initiate a new completion session.
ACTION-TYPE specifies the action to be completed.
STRATEGY may alter the completion behavior."

  (ai-completions--cancel)

  ;; Get or create command struct for completion
  (let ((command-struct (ai-completions--get-or-create-completion-command action-type)))
    (setq-local ai-completions--complete-at-point (ai-completions--get-completion-point strategy)
                ai-completions--preview-at-point (ai-completions--get-preview-point strategy)
                ai-completions--active t
                ai-completions--current-command-struct command-struct
                ai-completions--current-buffer-clone (ai-utils--clone-buffer (current-buffer))
                ai-completions--strategy strategy
                ;; Initialize context sizes from ai-context-management defaults, not from command
                ai-completions--current-precending-context-size ai-context-management--current-precending-context-size
                ai-completions--current-forwarding-context-size ai-context-management--current-forwarding-context-size)

    (condition-case-unless-debug err
        (progn (ai-completions-mode 1)
               (ai-completions--update-candidates (current-buffer)))
      (error (ai-logging--message 'error  "completion"  "An error occurred in ai-completions--begin => %s" (error-message-string err))
             (ai-completions--cancel)))))

(defun ai-completions--get-completion-point (strategy)
  "Get the position where completion should begin based on STRATEGY."
  (cond
   ((and (region-active-p) (or  (equal strategy 'complete)
                                (equal strategy nil)))
    (region-end))
   ((and (region-active-p) (equal strategy 'replace))
    (region-beginning))
   (t (point))))

(defun ai-completions--get-preview-point (strategy)
  "Determine the position for displaying a preview based on STRATEGY."
  (cond
   ((and (region-active-p) (equal strategy 'replace))
    (save-excursion
      (forward-line 1)
      (line-beginning-position)))
   ((and (region-active-p) (or  (equal strategy 'complete)
                                (equal strategy nil)))
    (region-end))
   (t (point))))

(defun ai-completions--continue ()
  "Continue an ongoing completion session."
  (condition-case-unless-debug err
      (progn
        (ai-logging--message 'info  "completion"  "Continuing completion session")
        (ai-completions--select-next-or-abort))
    (error (ai-logging--message 'error  "completion" "An error occurred in ai-completions--continue => %s" (error-message-string err))
           (ai-completions--cancel))))

(defun ai-completions--show-candidate ()
  "Display the current candidate for completion."
  (ai-logging--message 'info  "completion"  "Showing candidate %d of %d"
                              (1+ ai-completions--current-candidate)
                              (length ai-completions--candidates))
  (ai-completions--update-preview)
  (ai-completions--prepare-keymap)
  (ai-completions--activate-keymap))

(defun ai-completions--should-continue (command)
  "Determine if completion should proceed for COMMAND."
  (or (eq t ai-completions--continue-commands)
      (if (eq 'not (car ai-completions--abort-commands))
          (consp (memq command (cdr ai-completions--abort-commands)))
        (or (memq command ai-completions--abort-commands)
            (and (symbolp command)
                 (string-match-p "\\`ai-" (symbol-name command)))))))

(defun ai-completions--get-current-model ()
  "Retrieve the current model for completion."
  (ai-model-management-get-current))

(defun ai-completions--is-new-completion-required ()
  "Check if a new completion process is necessary."
  (or (not (equal (point) ai-completions--preview-at-point))
      (not ai-completions--active)))

(defun ai-completions--create-candidates-context ()
  "Create a structured context containing information about previous candidates."
  (when (and ai-completions--candidates
             (> (length ai-completions--candidates) 0))
    (let ((candidates-list
           (mapcar (lambda (candidate)
                     (ai-common--update-typed-struct
                      candidate
                      :type 'completion-candidate))
                   ai-completions--candidates)))
      (ai-common--make-typed-struct
       candidates-list
       'previous-candidates
       'completion-session))))

(cl-defun ai-completions--update-candidates (buffer)
  "Update the list of candidates for BUFFER using unified command system."
  (let* ((command-struct ai-completions--current-command-struct)
         (execution-model (ai-completions--get-current-model))
         (candidates-context (ai-completions--create-candidates-context))
         (external-contexts (when candidates-context (list candidates-context)))

         ;; Create a temporary command struct with current context sizes
         (updated-command-struct
          (let ((behavior (copy-ai-command-behavior (ai-structs--get-command-behavior command-struct))))
            (setf (ai-command-behavior-preceding-context-size behavior)
                  ai-completions--current-precending-context-size)
            (setf (ai-command-behavior-following-context-size behavior)
                  ai-completions--current-forwarding-context-size)
            (let ((updated-command (copy-ai-command command-struct)))
              (setf (ai-command-behavior updated-command) behavior)
              updated-command)))

         ;; Get execution context using the unified system
         (execution-context
          (ai-context-management--get-executions-context-for-command
           updated-command-struct
           :model execution-model
           :external-contexts external-contexts))

         (success-callback (lambda (context candidates &optional usage-stats)
                             (with-current-buffer buffer
                               (ai-logging--message 'info  "completion" "Received %d candidates" (length candidates))
                               (ai-completions--add-candidates candidates)
                               (ai-completions--show-candidate))))
         (fail-callback (lambda (context response-error)
                          (with-current-buffer buffer
                            (ai-logging--message 'error  "completion" "Request failed: %s"
                                                        (ai-common--get-text-content-from-struct response-error))
                            ;; Use ai-telemetry--log-and-error for structured error logging
                            (ai-telemetry--log-and-error (format "AI completion: Response error: %s"
                                                                (ai-common--get-text-content-from-struct response-error)))))))

    ;; Execute using ai-execution module with new callback signature
    (ai-execution--execute-context execution-context
                                   success-callback
                                   :fail-callback fail-callback
                                   :model execution-model)))

(defun ai-completions--get-current-buffer-clone ()
  "Retrieve or create a clone of the current buffer."
  (current-buffer))

(defun ai-completions--clear-buffer-clone ()
  "Clear the cloned buffer used for completion."
  (if ai-completions--current-buffer-clone
      (kill-buffer ai-completions--current-buffer-clone)))

(defun ai-completions--update-preview ()
  "Display the preview using the current candidate."
  (let* ((candidate (nth ai-completions--current-candidate ai-completions--candidates))
         (completion (ai-common--get-text-content-from-struct candidate)))
    (ai-completions--preview-show-at-point ai-completions--preview-at-point completion)))

(defun ai-completions--add-candidates (candidates)
  "Append CANDIDATES to the list of current candidates."
  (ai-logging--message 'info  "completion" "Adding %d candidates to existing %d candidates"
                              (length candidates) (length ai-completions--candidates))
  (setq ai-completions--candidates (append ai-completions--candidates candidates)
        ai-completions--current-candidate (- (length ai-completions--candidates) (length candidates))))

(defun ai-completions--preview-hide ()
  "Hide the preview of the current candidate."
  (when ai-completions--preview-overlay
    (delete-overlay ai-completions--preview-overlay)
    (setq ai-completions--preview-overlay nil)))

(defun ai-completions--reset-variables-to-defaults ()
  "Reset variables to their default values when completion process is interrupted or canceled."
  (ai-logging--message 'info  "completion" "Resetting variables to defaults")
  (setq-local ai-completions--current-candidate 0
              ai-completions--candidates '()
              ai-completions--complete-at-point nil
              ai-completions--preview-at-point nil
              ai-completions--active nil
              ai-completions--current-command-struct nil
              ai-completions--current-precending-context-size nil
              ai-completions--current-forwarding-context-size nil
              ai-completions--current-buffer-clone nil
              ai-completions--strategy nil
              ai-completions--use-full-context nil))

(defun ai-completions--abort ()
  "Abort the completion process."
  (interactive)
  (ai-logging--message 'info "completion"  "Aborting completion session")
  (ai-progress-stop-single-request)
  (ai-completions--reset-variables-to-defaults)
  (ai-completions--cancel))

(defun ai-completions-finish (candidate)
  "Complete the completion process with the given CANDIDATE."
  (ai-logging--message 'info  "completion" "Finishing completion with candidate")
  (ai-completions--preview-hide)
  (ai-completions--insert-candidate candidate)
  (ai-completions--reset-variables-to-defaults)
  (ai-completions--cancel))

(defun ai-completions--cancel ()
  "Cancel the ongoing completion process, resetting the state."
  (ai-logging--message 'info  "completion" "Canceling completion session")
  (ai-progress-stop-single-request)
  (ai-completions--preview-hide)
  (ai-completions--clear-buffer-clone)
  (setq-local ai-completions--current-candidate 0
        ai-completions--candidates '()
        ai-completions--complete-at-point nil
        ai-completions--preview-at-point nil
        ai-completions--active nil
        ai-completions--current-command-struct nil
        ai-completions--current-precending-context-size nil
        ai-completions--current-forwarding-context-size nil
        ai-completions--current-buffer-clone nil
        ai-completions--strategy nil)
  (ai-completions--destroy-keymap)
  (ai-completions-mode 0)
  nil)

(defun ai-completions--insert-candidate (candidate)
  "Insert the selected CANDIDATE into the buffer."
  (when (> (length candidate) 0)
    (let* ((completion-text (substring-no-properties (ai-common--get-text-content-from-struct candidate))))
      (ai-logging--message 'info  "completion" "Inserting candidate with %d characters" (length completion-text))
      (when (and (equal ai-completions--strategy 'replace)
                 (region-active-p))
        (delete-region (region-beginning) (region-end)))
      (ai-utils--insert-completion-at-point ai-completions--complete-at-point completion-text))))

(defun ai-completions--select-current ()
  "Insert the currently selected candidate."
  (interactive)
  (let* ((candidate (nth ai-completions--current-candidate ai-completions--candidates)))
    (ai-logging--message 'info  "completion" "Selecting current candidate %d" ai-completions--current-candidate)
    (ai-completions-finish candidate)))

(defun ai-completions--select-next-or-abort ()
  "Display the next candidate or trigger an update if necessary."
  (interactive)
  (let ((next-candidate-index (+ ai-completions--current-candidate 1)))
    (cond
     ((>= next-candidate-index (length ai-completions--candidates))
      (ai-logging--message 'info  "completion" "No more candidates available, requesting more")
      (ai-completions--update-candidates (ai-completions--get-current-buffer-clone)))
     (t (setq ai-completions--current-candidate next-candidate-index)
        (ai-logging--message 'info  "completion" "Moving to next candidate %d" next-candidate-index)
        (ai-completions--show-candidate)))))

(defun ai-completions--select-prev-or-abort ()
  "Display the previous candidate."
  (interactive)
  (let ((prev-candidate-index (- ai-completions--current-candidate 1)))
    (cond
     ((>= prev-candidate-index 0)
      (let ((result (nth prev-candidate-index ai-completions--candidates)))
        (setq ai-completions--current-candidate prev-candidate-index)
        (ai-logging--message 'info  "completion" "Moving to previous candidate %d" prev-candidate-index)
        (ai-completions--show-candidate)))
     (t (ai-logging--message 'info  "completion" "No previous candidate available")
        (message "No previous candidate available")))))

(defun ai-completions--increase-current-context ()
  "Increase the context size for the current completion."
  (interactive)
  (setq ai-completions--current-precending-context-size (+ (or ai-completions--current-precending-context-size
                                                               ai-context-management--current-precending-context-size)
                                                           ai-completions--context-size-step)
        ai-completions--current-forwarding-context-size (+ (or ai-completions--current-forwarding-context-size
                                                               ai-context-management--current-forwarding-context-size)
                                                           ai-completions--context-size-step))
  (ai-logging--message 'info  "completion" "Increased context size to %d-%d"
                              ai-completions--current-precending-context-size
                              ai-completions--current-forwarding-context-size)
  (ai-completions--show-candidate)
  (message (format "Current completion context size: %d - %d"
                   ai-completions--current-precending-context-size
                   ai-completions--current-forwarding-context-size)))

(defun ai-completions--maximize-current-context ()
  "Maximize the context window for completion by setting needs-buffer-context = t."
  (interactive)
  (when ai-completions--current-command-struct
    ;; Update the command struct to use buffer context
    (let* ((behavior (copy-ai-command-behavior (ai-structs--get-command-behavior ai-completions--current-command-struct)))
           (updated-command (copy-ai-command ai-completions--current-command-struct)))
      (setf (ai-command-behavior-needs-buffer-context behavior) t)
      (setf (ai-command-behavior-preceding-context-size behavior) nil)
      (setf (ai-command-behavior-following-context-size behavior) nil)
      (setf (ai-command-behavior updated-command) behavior)
      (setq ai-completions--current-command-struct updated-command)
      ;; Reset local context variables since we're using buffer context now
      (setq ai-completions--current-precending-context-size nil
            ai-completions--current-forwarding-context-size nil)))
  (ai-logging--message 'info  "completion" "Maximized context to use full buffer context")
  (ai-completions--show-candidate)
  (message "Current completion context: full buffer"))

(defun ai-completions--add-instruction (input)
  "Add an instruction INPUT to the current query using the centralized context pool."
  (interactive (list (read-string "Enter query instruction: ")))
  (ai-logging--message 'info  "completion" "Adding user instruction: %s" input)
  (ai-context-management--add-to-context-pool
   (ai-common--make-typed-struct input 'user-instruction 'user-input))
  (when ai-completions--active
    (condition-case-unless-debug err
        (progn (ai-completions-mode 1)
               (ai-completions--update-candidates (current-buffer)))
      (error (ai-logging--message 'error  "completion"  "An error occurred in ai-completions--add-instruction => %s" (error-message-string err))
             (ai-completions--cancel)))))

(define-minor-mode ai-completions-mode
  "AI completion mode."
  :after-hook (force-mode-line-update t)
  (if ai-completions-mode
      (progn
        (ai-logging--message 'info  "completion" "Enabling completion mode")
        (add-hook 'pre-command-hook 'ai-completions-mode-pre-command)
        (add-hook 'post-command-hook 'ai-completions-mode-post-command))
    (ai-logging--message 'info  "completion" "Disabling completion mode")
    (remove-hook 'pre-command-hook 'ai-completions-mode-pre-command)
    (remove-hook 'post-command-hook 'ai-completions-mode-post-command)))

(defun ai-completions-mode-pre-command ()
  "Hook function called before executing a command in AI completions mode."
  (if (not (ai-completions--should-continue this-command))
      (progn
        (ai-logging--message 'info  "completion"  "Command %s should abort completion" this-command)
        (ai-completions--abort))
    (ai-completions--deactivate-keymap)))

(defun ai-completions-mode-post-command ()
  "Hook function called after executing a command in AI completions mode."
  (when ai-completions--active
    (ai-completions--activate-keymap)))

(defun ai-completions--prepare-keymap ()
  "Prepare the keymap for AI completion."
  (ai-completions-ensure-emulation-alist)
  (ai-completions--enable-overriding-keymap ai-completions--mode-keymap))

(defun ai-completions--destroy-keymap ()
  "Destroy the active keymap for AI completion."
  (ai-completions--deactivate-keymap)
  (ai-completions--enable-overriding-keymap nil))

(defun ai-completions--enable-overriding-keymap (keymap)
  "Activate the given KEYMAP."
  (ai-completions--deactivate-keymap)
  (setf (cdar ai-completions--emulation-alist) nil)
  (setq ai-completions--activated-keymap keymap))

(defun ai-completions--deactivate-keymap ()
  "Deactivate any currently active keymap."
  (setf (cdar ai-completions--emulation-alist) nil))

(defun ai-completions--activate-keymap ()
  "Reactivate the keymap if needed."
  (unless (or (cdar ai-completions--emulation-alist)
              (null ai-completions--activated-keymap))
    (setf (cdar ai-completions--emulation-alist) ai-completions--activated-keymap)))

(defun ai-completions-ensure-emulation-alist ()
  "Ensure the emulation alist is set up correctly."
  (unless (eq 'ai-completions--emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'ai-completions--emulation-alist
                (delq 'ai-completions--emulation-alist emulation-mode-map-alists)))))

(defface ai-completions--preview-face
  '((((class color) (min-colors 88) (background light))
     (:background "light blue"))
    (((class color) (min-colors 88) (background dark))
     (:background "gray31"))
    (t (:background "green"))
    (((class color) (min-colors 88) (background light))
     (:foreground "black" :background "cornsilk"))
    (((class color) (min-colors 88) (background dark))
     (:background "gray26"))
    (t (:foreground "black" :background "yellow")))
  "Face used for the selection in the tooltip.")

(defun ai-completions--preview-show-at-point (pos completion)
  "Show COMPLETION preview at POS."
  (ai-completions--preview-hide)

  (let* ((completion-copy (copy-sequence completion)))

    (add-face-text-property 0 (length completion-copy) 'ai-completions--preview-face nil completion-copy)

    (and (equal pos (point))
         (not (equal completion-copy ""))
         (add-text-properties 0 1 '(cursor 1) completion-copy))

    (let* ((beg pos)
           (pto ai-completions--pseudo-tooltip-overlay)
           (ptf-workaround (and
                            pto
                            (char-before pos)
                            (eq pos (overlay-start pto)))))
      ;; Try to accommodate for the pseudo-tooltip overlay,
      ;; which may start at the same position if it's at eol.
      (when ptf-workaround
        (cl-decf beg)
        (setq completion-copy (concat (buffer-substring beg pos) completion-copy)))

      (setq ai-completions--preview-overlay (make-overlay beg pos))

      (let ((ov ai-completions--preview-overlay))
        (overlay-put ov (if ptf-workaround 'display 'after-string)
                     completion-copy)
        (overlay-put ov 'window (selected-window))))))

(defun ai-completions--add-system-instructions (input)
  "Add INPUT as system instructions to the list."
  (interactive (list (read-string "Enter system instruction: ")))
  (with-current-buffer (current-buffer)
    (ai-logging--message 'info  "completion" "Adding system instruction: %s" input)
    (setq-local ai-completions--global-system-instructions
                (append ai-completions--global-system-instructions
                        `((("role" . "system") ("content" . ,input)))))))

(defun ai-completions--clear-system-instructions ()
  "Clear all system instructions."
  (interactive)
  (with-current-buffer (current-buffer)
    (ai-logging--message 'info  "completion" "Clearing system instructions")
    (setq-local ai-completions--global-system-instructions '())))

(provide 'ai-completions)

;;; ai-completions.el ends here
