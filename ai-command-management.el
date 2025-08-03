;;; ai-command-management.el --- Command management for AI mode -*- lexical-binding: t -*-

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
;; This module handles command management for AI mode, including:
;; - Command definition, parsing, and configuration
;; - Instruction file loading and caching
;; - Command modifier parsing and application
;; - File-based command system with hot-reloading
;; - Unified Command Registry and Modifier System
;; - Command selection and filtering utilities

;;; Code:

(require 'cl-lib)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-structs)
(require 'ai-prompt-management)
(require 'ai-logging)
(require 'ai-project) ; For project root detection

(defcustom ai-command-management-instruction-file-extension ".md"
  "File extension used for instruction files."
  :type 'string
  :group 'ai)

(defcustom ai-command-management-instruction-watching-enabled t
  "Enable file watching for instruction files to auto-reload cache."
  :type 'boolean
  :group 'ai)

(defcustom ai-command-management-file-command-action-modifiers
  '(("show" . show)
    ("eval" . eval)
    ("replace" . replace)
    ("insert" . insert-at-point)
    ("complete" . complete))
  "Mapping of action modifier names to result-action symbols for file-based commands.
These modifiers can be used in file command names to specify the result action."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (symbol :tag "Result Action"))
  :group 'ai-mode)

(defcustom ai-command-management-file-command-behavior-modifiers
  '(("user" . (:user-input t))
    ("buffer" . (:needs-buffer-context t))
    ("project" . (:needs-project-context t))
    ("global" . (:needs-global-context t)))
  "Mapping of behavior modifier names to configuration plists for file-based commands.
These modifiers control command execution behavior."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (plist :tag "Configuration"))
  :group 'ai-mode)

(defcustom ai-command-management-file-command-context-modifiers
  '(("small" . (:preceding-context-size 5 :following-context-size 5))
    ("large" . (:preceding-context-size 20 :following-context-size 20))
    ("full" . (:preceding-context-size nil :following-context-size nil)))
  "Mapping of context modifier names to context size configuration for file-based commands.
These modifiers control the amount of context included with commands."
  :type '(alist :key-type (string :tag "Modifier Name")
                :value-type (plist :tag "Context Configuration"))
  :group 'ai-mode)

(defcustom ai-command-management-commands-config-map
  '(("modify" . (:template "" :instructions nil :user-input t :result-action replace))
    ("generate code from selection" . (:instructions nil :result-action replace))
    ("generate code from user input" . (:instructions nil :user-input t :result-action insert-at-point :needs-buffer-context t))
    ("create from user input" . (:instructions nil :user-input t :result-action insert-at-point :needs-buffer-context t))
    ("create from selection" . (:instructions nil :result-action replace))
    ("execute prompt inplace" . (:instructions nil :result-action replace))
    ("explain" . (:instructions nil :result-action show))
    ("explain with full context" . (:instructions nil :user-input t :result-action show :needs-buffer-context t))
    ("explain with user input" . (:instructions nil :user-input t :result-action show))
    ("doc" . (:instructions nil :result-action replace))
    ("fix" . (:instructions nil :result-action replace))
    ("simplify" . (:instructions nil :result-action replace))
    ("improve" . (:instructions nil :result-action replace))
    ("optimize" . (:instructions nil :result-action replace))
    ("spellcheck" . (:instructions nil :result-action replace))
    ("index file" . (:instructions nil :result-action show))

    ("answer" . (:instructions nil :user-input t :result-action show ))
    ("add to memory" . (:instructions nil :result-action replace)))

  "An association list mapping AI commands to their configurations.
Each entry is a pair: `(COMMAND . CONFIG-PLIST)`.

`COMMAND` is a string (e.g., \"modify\", \"explain\").
`CONFIG-PLIST` is a property list with the following keys:
- `:template` (string, optional): A template string for the command.
- `:instructions` (string, optional): Specific instructions for the AI for this command.
- `:user-input` (boolean): `t` if the command requires additional user input.
- `:action-type` (string, optional): The high-level action type (e.g., \"modify\", \"complete\") used for structuring the request.
- `:result-action` (symbol): Specifies how the AI's response should be handled.
  Possible values: `show` (display in a buffer), `replace` (replace selection/buffer),
  `eval` (display and offer to evaluate), `insert-at-point` (insert at cursor position).
- `:needs-buffer-context` (boolean, optional): `t` if the full buffer content
  is required for context, even if a region is active."
  :type '(alist :key-type (string :tag "Command")
                :value-type (plist :tag "Format Specification"
                                   :options ((:template (string :tag "Template") :optional t)
                                             (:instructions (string :tag "Instructions") :optional t)
                                             (:user-input (boolean :tag "User input"))
                                             (:action-type (string :tag "Action Type") :optional t)
                                             (:result-action (symbol :tag "Result Action" :value-type (choice (const show) (const replace) (const eval) (const insert-at-point))))
                                             (:needs-buffer-context (boolean :tag "Needs Buffer Context") :optional t))))
  :group 'ai-mode)

(defcustom ai-command-management-completion-config
  `(:action "complete" :instructions nil :action-type "complete" :result-action complete)
  "Configuration for code completion."
  :group 'ai-mode)

(defcustom ai-command-management-command-config
  `(:instructions nil)
  "Configuration for generic commands."
  :group 'ai-mode)

(defcustom ai-command-management-command-prompt "Command or query: "
  "Prompt for selecting the command."
  :type 'string
  :group 'ai-mode)

;; ============================================================================
;; New System: Command Registry and Modifier System
;; ============================================================================

;; Variables for new command registry - now project-aware
(defvar ai-command-management--project-command-registries (make-hash-table :test 'equal)
  "Hash table mapping project roots to command registries.")

(defvar ai-command-management--command-providers nil
  "List of registered command providers.")

(defvar ai-command-management--registries-dirty (make-hash-table :test 'equal)
  "Hash table tracking which project registries need refresh.")

;; Variables for modifier system
(defvar ai-command-management--modifier-registry (make-hash-table :test 'equal)
  "Registry of available command modifiers.")

;; Buffer-local project key caching
(defvar-local ai-command-management--buffer-project-key nil
  "Cached project key for current buffer.")

;; New variables for optimized auto-update system
(defvar ai-command-management--refresh-timer nil
  "Timer for debounced registry refresh.")

(defvar ai-command-management--refresh-delay 0.5
  "Delay in seconds for debounced registry refresh.")

;; ============================================================================
;; Project-aware registry management
;; ============================================================================

(defun ai-command-management--get-buffer-project-key ()
  "Get project key for current buffer with caching."
  (or ai-command-management--buffer-project-key
      (setq ai-command-management--buffer-project-key
            (or (ai-project--get-project-root) "global"))))

(defun ai-command-management--get-project-registry (&optional project-key)
  "Get command registry for PROJECT-KEY (or current buffer project)."
  (let ((key (or project-key (ai-command-management--get-buffer-project-key))))
    (or (gethash key ai-command-management--project-command-registries)
        (let ((new-registry (make-hash-table :test 'equal)))
          (puthash key new-registry ai-command-management--project-command-registries)
          new-registry))))

(defun ai-command-management--mark-project-registry-dirty (&optional project-key)
  "Mark project registry as dirty for PROJECT-KEY (or current buffer project)."
  (let ((key (or project-key (ai-command-management--get-buffer-project-key))))
    (puthash key t ai-command-management--registries-dirty)))

(defun ai-command-management--is-project-registry-dirty (&optional project-key)
  "Check if project registry is dirty for PROJECT-KEY (or current buffer project)."
  (let ((key (or project-key (ai-command-management--get-buffer-project-key))))
    (gethash key ai-command-management--registries-dirty)))

(defun ai-command-management--detect-project-change ()
  "Detect if project has changed and invalidate buffer cache if needed."
  (let ((current-key (or (ai-project--get-project-root) "global")))
    (unless (equal current-key ai-command-management--buffer-project-key)
      (setq ai-command-management--buffer-project-key current-key)
      (ai-command-management--mark-project-registry-dirty current-key)
      (ai-logging--message 'info  "command-management" "Project change detected for buffer, new key: %s" current-key))))

(defun ai-command-management--debounced-refresh-registry (&optional delay project-key)
  "Refresh registry with debouncing to avoid excessive updates.
DELAY defaults to `ai-command-management--refresh-delay`.
PROJECT-KEY defaults to current buffer project."
  (when ai-command-management--refresh-timer
    (cancel-timer ai-command-management--refresh-timer))
  (setq ai-command-management--refresh-timer
        (run-with-timer (or delay ai-command-management--refresh-delay) nil
                        (lambda ()
                          (ai-command-management--refresh-command-registry project-key)
                          (setq ai-command-management--refresh-timer nil)))))

;; ============================================================================
;; Improved prompt management cache update hook
;; ============================================================================

(defun ai-command-management--on-prompt-cache-update (location cache-type stats)
  "Hook function called when prompt management cache is updated.
LOCATION is the cache location (:default/:global/:local).
CACHE-TYPE is the type of cache (:instruction/:system-prompt).
STATS contains update statistics.

This function marks command registries as dirty when instruction caches are updated."
  ;; Only react to instruction cache updates (not system prompts)
  (when (eq cache-type :instruction)
    (cond
     ;; Global or default cache updates affect all project registries - use debouncing
     ((or (eq location :global) (eq location :default))
      (ai-logging--message 'info  "command-management" "Scheduling debounced registry refresh due to %s instruction cache update" location)
      (clrhash ai-command-management--registries-dirty)
      (maphash (lambda (project-key _registry)
                 (puthash project-key t ai-command-management--registries-dirty))
               ai-command-management--project-command-registries)
      ;; Use longer delay for global changes to batch multiple file changes
      (ai-command-management--debounced-refresh-registry 0.8))
     ;; Local cache updates affect specific project registry - faster response
     ((eq location :local)
      (let* ((directory (plist-get stats :directory))
             (project-key (when directory
                            ;; Extract project root from directory path
                            (when (string-match "\\(.+\\)/.ai/" directory)
                              (match-string 1 directory)))))
        (if project-key
            (progn
              (ai-logging--message 'info "command-management" "Scheduling refresh for project registry: %s" project-key)
              (puthash project-key t ai-command-management--registries-dirty)
              ;; Faster refresh for local changes
              (ai-command-management--debounced-refresh-registry 0.3 project-key))
          ;; Fallback: mark current project registry dirty
          (ai-logging--message 'info  "command-management" "Marking current project registry dirty due to local instruction cache update")
          (ai-command-management--mark-project-registry-dirty)
          (ai-command-management--debounced-refresh-registry 0.3)))))))

;; ============================================================================
;; Helper functions for new command system (moved from ai-core)
;; ============================================================================

(defun ai-command-management--get-command-by-filter (filter-fn prompt)
  "Get command using new registry system with FILTER-FN and PROMPT.
Returns ai-command structure if found, creates one for user input otherwise."
  ;; Ensure registry is updated and handle project changes
  (ai-command-management--ensure-registry-updated)

  ;; If registry is empty after update, force refresh
  (let ((registry (ai-command-management--get-project-registry)))
    (when (= (hash-table-count registry) 0)
      (ai-logging--message 'info "command-management" "Registry is empty, forcing immediate refresh")
      (ai-command-management--refresh-command-registry)))

  (let* ((commands (ai-command-management--filter-commands filter-fn))
         (command-displays (mapcar #'ai-command-management--format-command-display-name commands))
         (command-alist (cl-mapcar #'cons command-displays commands))
         (user-input (completing-read prompt command-displays nil nil))
         (selected-command (cdr (assoc user-input command-alist))))
    (or selected-command
        ;; Create ai-command for arbitrary user input
        (ai-command-management--create-user-input-command user-input))))

(defun ai-command-management--create-user-input-command (user-input)
  "Create an ai-command struct for arbitrary USER-INPUT with default settings."
  (let* ((behavior (make-ai-command-behavior
                    :user-input nil
                    :action-type nil
                    :result-action 'show  ; Default to show for user queries
                    :needs-buffer-context nil
                    :needs-project-context nil
                    :needs-global-context nil
                    :preceding-context-size nil
                    :following-context-size nil))
         (priority 1000)) ; Lower priority for user input commands
    (make-ai-command
     :name user-input
     :canonical-name user-input
     :base-name user-input
     :instructions user-input  ; Use the user input as instructions
     :behavior behavior
     :source :user-input
     :location :dynamic
     :file-path nil
     :priority priority)))

(defun ai-command-management--get-show-commands ()
  "Get commands suitable for show action (informational commands)."
  (ai-command-management--get-command-by-filter
   (lambda (cmd) (eq (ai-structs--get-result-action cmd) 'show))
   "Show command: "))

(defun ai-command-management--get-eval-commands ()
  "Get commands suitable for eval action (executable commands)."
  (ai-command-management--get-command-by-filter
   (lambda (cmd) (eq (ai-structs--get-result-action cmd) 'eval))
   "Execute command: "))

(defun ai-command-management--get-unrestricted-command ()
  "Get any command without restrictions."
  (ai-command-management--get-command-by-filter
   (lambda (_cmd) t) ; Accept all commands
   "Execute command: "))

(defun ai-command-management--get-coordinator-command ()
  "Get command for coordinator (completion/continuation)."
  (ai-command-management--get-command-by-filter
   (lambda (_cmd) t) ; Accept all commands
   "Coordinator command: "))

(defun ai-command-management--get-debug-command ()
  "Get command for debugging purposes."
  (ai-command-management--get-command-by-filter
   (lambda (_cmd) t) ; Accept all commands
   "Debug command: "))

;; ============================================================================
;; Modifier Registry API
;; ============================================================================

(defun ai-command-management--register-modifier (modifier)
  "Register MODIFIER in the modifier registry."
  (unless (ai-modifier-p modifier)
    (error "MODIFIER must be an ai-modifier struct: %s" modifier))
  (puthash (ai-modifier-name modifier) modifier ai-command-management--modifier-registry))

(defun ai-command-management--get-modifier (name)
  "Get modifier by NAME from registry."
  (gethash name ai-command-management--modifier-registry))

(defun ai-command-management--parse-modifiers-from-name (command-name)
  "Parse modifiers from COMMAND-NAME using registered modifiers.
Returns (base-name . modifiers-list)."
  (let ((parts (split-string command-name "__"))
        (modifiers '())
        (command-parts '()))

    ;; Separate modifier parts from command parts
    (dolist (part parts)
      (if-let ((modifier (ai-command-management--get-modifier part)))
          (push modifier modifiers)
        ;; Not a modifier - it's part of command name
        (push part command-parts)))

    ;; Build base name from command parts (convert underscores to spaces)
    (let ((base-name (if command-parts
                        (mapconcat (lambda (part) (replace-regexp-in-string "_" " " part))
                                   (reverse command-parts) " ")
                       "")))
      (cons base-name (reverse modifiers)))))

(defun ai-command-management--create-behavior-from-config (name config)
  "Create ai-command-behavior struct from CONFIG plist."
  (ai-logging--message 'debug "command-management" "Creating ai-command-behavior for '%s' from config: %S" name config)

  ;; Ensure config is a proper plist
  (unless (and (listp config) (cl-evenp (length config)))
    (error "Invalid config format for command %s: %S" name config))

  (let* ((user-input (plist-get config :user-input))
         (action-type (plist-get config :action-type))
         (result-action (plist-get config :result-action))
         (needs-buffer-context (plist-get config :needs-buffer-context))
         (needs-project-context (plist-get config :needs-project-context))
         (needs-global-context (plist-get config :needs-global-context))
         (preceding-context-size (plist-get config :preceding-context-size))
         (following-context-size (plist-get config :following-context-size)))

    ;; Add debug output
    (ai-logging--message 'debug "command-management" "Extracted values for command %s: user-input=%S, action-type=%S, result-action=%S, needs-buffer-context=%S"
                                 name user-input action-type result-action needs-buffer-context)

    (make-ai-command-behavior
     :user-input user-input
     :action-type action-type
     :result-action result-action
     :needs-buffer-context needs-buffer-context
     :needs-project-context needs-project-context
     :needs-global-context needs-global-context
     :preceding-context-size preceding-context-size
     :following-context-size following-context-size)))

(defun ai-command-management--apply-modifiers-to-config (base-config modifiers)
  "Apply MODIFIERS to BASE-CONFIG and return merged configuration."
  (let ((final-config (copy-sequence base-config)))
    (dolist (modifier modifiers)
      (let ((modifier-config (ai-modifier-config modifier)))
        ;; Merge modifier configuration with base configuration
        (while modifier-config
          (let ((key (car modifier-config))
                (value (cadr modifier-config)))
            (setq final-config (plist-put final-config key value)))
          (setq modifier-config (cddr modifier-config)))))
    final-config))

(defun ai-command-management--get-modifier-display-chars (modifiers)
  "Get display characters for MODIFIERS list."
  (mapconcat (lambda (modifier)
               (or (ai-modifier-display-char modifier) "?"))
             modifiers ""))

;; ============================================================================
;; Command Provider API
;; ============================================================================

(defun ai-command-management--register-provider (provider)
  "Register command PROVIDER in the system."
  (unless (ai-command-provider-p provider)
    (error "PROVIDER must be an ai-command-provider struct: %s" provider))
  ;; Remove existing registration of the same provider
  (setq ai-command-management--command-providers
        (cl-remove (ai-command-provider-name provider) ai-command-management--command-providers
                   :key #'ai-command-provider-name :test #'equal))
  ;; Add new registration
  (push provider ai-command-management--command-providers)
  ;; Sort by priority (smaller numbers have higher priority)
  (setq ai-command-management--command-providers
        (sort ai-command-management--command-providers
              (lambda (a b) (< (ai-command-provider-priority a)
                              (ai-command-provider-priority b)))))
  ;; Mark all project registries as dirty
  (clrhash ai-command-management--registries-dirty)
  (maphash (lambda (project-key _registry)
             (puthash project-key t ai-command-management--registries-dirty))
           ai-command-management--project-command-registries))

(defun ai-command-management--load-commands-from-provider (provider)
  "Load commands from PROVIDER and return list of ai-command structs."

  (condition-case-unless-debug err
      (funcall (ai-command-provider-loader-fn provider))
    (error
     (ai-logging--message 'error  "command-management" "Error loading commands from provider %s: %s"
                                  (ai-command-provider-name provider) err)
     nil)))

(defun ai-command-management--create-config-provider ()
  "Create provider for configuration-based commands."
  (make-ai-command-provider
   :name "config"
   :priority 400
   :location :config
   :loader-fn #'ai-command-management--load-config-commands))

(defun ai-command-management--create-file-provider (location)
  "Create provider for file-based commands from LOCATION."
  (let ((priority (cond
                   ((eq location :default) 300)
                   ((eq location :global) 200)
                   ((eq location :local) 100)
                   (t 400))))
    (make-ai-command-provider
     :name (format "file-%s" location)
     :priority priority
     :location location
     :loader-fn (lambda () (ai-command-management--load-file-commands location)))))

;; ============================================================================
;; Command Registry API
;; ============================================================================

(defun ai-command-management--refresh-command-registry (&optional project-key)
  "Refresh command registry for PROJECT-KEY (or current buffer project)."
  (let* ((key (or project-key (ai-command-management--get-buffer-project-key)))
         (registry (ai-command-management--get-project-registry key)))

    (clrhash registry)

    (dolist (provider ai-command-management--command-providers)
      (let ((commands (ai-command-management--load-commands-from-provider provider)))
        (dolist (command commands)
          (when (ai-command-p command)
            ;; Use canonical name as key
            (puthash (ai-structs--get-command-canonical-name command) command registry)))))

    (remhash key ai-command-management--registries-dirty)
    (ai-logging--message 'info "command-management" "Command registry refreshed for project %s: %d commands loaded"
                                 key (hash-table-count registry))))

(defun ai-command-management--get-command-from-registry (name &optional project-key)
  "Get command by NAME from registry for PROJECT-KEY (or current buffer project)."
  (ai-command-management--ensure-registry-updated project-key)
  (let ((registry (ai-command-management--get-project-registry project-key)))
    (gethash name registry)))

(defun ai-command-management--get-all-commands-from-registry (&optional project-key)
  "Get all commands from registry for PROJECT-KEY (or current buffer project)."
  (ai-command-management--ensure-registry-updated project-key)
  (let ((registry (ai-command-management--get-project-registry project-key))
        (commands nil))
    (maphash (lambda (_name command) (push command commands)) registry)
    ;; Sort by priority
    (sort commands (lambda (a b) (< (ai-structs--get-command-priority a)
                                   (ai-structs--get-command-priority b))))))

(defun ai-command-management--filter-commands (filter-fn &optional project-key)
  "Filter commands from registry using FILTER-FN for PROJECT-KEY (or current buffer project)."
  (ai-command-management--ensure-registry-updated project-key)
  (let ((registry (ai-command-management--get-project-registry project-key))
        (filtered-commands nil))
    (maphash (lambda (_name command)
               (when (funcall filter-fn command)
                 (push command filtered-commands)))
             registry)
    ;; Sort by priority
    (sort filtered-commands (lambda (a b) (< (ai-structs--get-command-priority a)
                                            (ai-structs--get-command-priority b))))))

(defun ai-command-management--ensure-registry-updated (&optional project-key)
  "Ensure command registry is up to date for PROJECT-KEY (or current buffer project).
This function includes project change detection and lazy loading."
  ;; Detect project changes first
  (ai-command-management--detect-project-change)

  (let ((key (or project-key (ai-command-management--get-buffer-project-key))))
    (when (ai-command-management--is-project-registry-dirty key)
      (ai-command-management--refresh-command-registry key))))

;; ============================================================================
;; Registry management functions
;; ============================================================================

(defun ai-command-management-refresh-registry ()
  "Manually refresh command registry for current project."
  (interactive)
  (when ai-command-management--refresh-timer
    (cancel-timer ai-command-management--refresh-timer)
    (setq ai-command-management--refresh-timer nil))
  (ai-command-management--refresh-command-registry)
  (ai-logging--message 'info "command-management" "Command registry refreshed"))

(defun ai-command-management-refresh-all-registries ()
  "Manually refresh command registries for all projects."
  (interactive)
  (when ai-command-management--refresh-timer
    (cancel-timer ai-command-management--refresh-timer)
    (setq ai-command-management--refresh-timer nil))
  (let ((count 0))
    (maphash (lambda (project-key _registry)
               (ai-command-management--refresh-command-registry project-key)
               (setq count (1+ count)))
             ai-command-management--project-command-registries)
    (ai-logging--message 'info "command-management" "Refreshed %d project registries" count)))

(defun ai-command-management-registry-status ()
  "Show status of command registry for current project."
  (interactive)
  (let* ((project-key (ai-command-management--get-buffer-project-key))
         (registry (ai-command-management--get-project-registry project-key))
         (count (hash-table-count registry))
         (is-dirty (ai-command-management--is-project-registry-dirty project-key))
         (has-timer (not (null ai-command-management--refresh-timer))))
    (ai-logging--message 'info "command-management" "Command registry for project '%s' contains %d commands%s%s"
             project-key count
             (if is-dirty " [DIRTY]" " [CLEAN]")
             (if has-timer " [SCHEDULED FOR REFRESH]" ""))))

(defun ai-command-management-registry-info ()
  "Show detailed information about command registry for current project."
  (interactive)
  (ai-command-management--ensure-registry-updated)
  (let* ((all-commands (ai-command-management--get-all-commands-from-registry))
         (config-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-source cmd) :config)) all-commands))
         (file-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-source cmd) :file)) all-commands))
         (local-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-location cmd) :local)) all-commands))
         (global-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-location cmd) :global)) all-commands))
         (default-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-location cmd) :default)) all-commands))
         (user-input-commands (cl-count-if (lambda (cmd) (eq (ai-structs--get-command-source cmd) :user-input)) all-commands))
         (project-key (ai-command-management--get-buffer-project-key)))
    (ai-logging--message 'info "command-management" "Registry for '%s': %d total (%d config, %d file, %d user-input | %d local, %d global, %d default)"
             project-key (length all-commands) config-commands file-commands user-input-commands
             local-commands global-commands default-commands)))

;; ============================================================================
;; Display functions for new system
;; ============================================================================

(defun ai-command-management--format-command-display-name (command-struct)
  "Format COMMAND-STRUCT for display with appropriate indicators."
  (let* ((indicators (ai-command-management--get-command-indicators command-struct))
         (name (ai-structs--get-command-name command-struct)))
    (if (string-empty-p indicators)
        name
      (format "%s %s" indicators name))))

(defun ai-command-management--get-command-indicators (command-struct)
  "Get indicator string for COMMAND-STRUCT."
  (let ((indicators nil)
        (source (ai-structs--get-command-source command-struct))
        (location (ai-structs--get-command-location command-struct))
        (behavior (ai-structs--get-command-behavior command-struct)))

    ;; Source indicators
    (cond
     ((eq source :config) (push "C" indicators))
     ((eq source :user-input) (push "U" indicators))
     ((eq source :file)
      (cond
       ((eq location :default) (push "D" indicators))
       ((eq location :global) (push "G" indicators))
       ((eq location :local) (push "L" indicators)))))

    ;; Behavior indicators
    (when behavior
      (when (ai-command-behavior-user-input behavior)
        (push "I" indicators))  ; Requires input
      (when (ai-command-behavior-needs-buffer-context behavior)
        (push "B" indicators))
      (when (ai-command-behavior-needs-project-context behavior)
        (push "P" indicators))
      (when (ai-command-behavior-needs-global-context behavior)
        (push "G" indicators))

      ;; Context size indicators
      (let ((preceding (ai-command-behavior-preceding-context-size behavior))
            (following (ai-command-behavior-following-context-size behavior)))
        (cond
         ((and (null preceding) (null following))
          (push "F" indicators))  ; Full context
         ((and (eq preceding 5) (eq following 5))
          (push "s" indicators))  ; Small context
         ((and (eq preceding 20) (eq following 20))
          (push "L" indicators))))  ; Large context

      ;; Result action indicator
      (when-let ((result-action (ai-command-behavior-result-action behavior)))
        (let ((action-indicator (cond
                                ((eq result-action 'show) "S")
                                ((eq result-action 'eval) "E")
                                ((eq result-action 'replace) "R")
                                ((eq result-action 'insert-at-point) "I")
                                ((eq result-action 'complete) "C")
                                (t "?"))))
          (push action-indicator indicators))))

    ;; Assemble indicators
    (if indicators
        (format "[%s]" (string-join (reverse indicators) ""))
      "")))

;; ============================================================================
;; Modifier system initialization
;; ============================================================================

(defun ai-command-management--initialize-modifier-registry ()
  "Initialize modifier registry with default modifiers."
  (clrhash ai-command-management--modifier-registry)

  ;; Register action modifiers
  (dolist (entry ai-command-management-file-command-action-modifiers)
    (let* ((name (car entry))
           (result-action (cdr entry))
           (display-char (cond
                          ((eq result-action 'show) "S")
                          ((eq result-action 'eval) "E")
                          ((eq result-action 'replace) "R")
                          ((eq result-action 'insert-at-point) "I")
                          ((eq result-action 'complete) "C")
                          (t "?")))
           (modifier (make-ai-modifier
                      :name name
                      :type :action
                      :config `(:result-action ,result-action)
                      :display-char display-char
                      :description (format "Result action: %s" result-action))))
      (ai-command-management--register-modifier modifier)))

  ;; Register behavior modifiers
  (dolist (entry ai-command-management-file-command-behavior-modifiers)
    (let* ((name (car entry))
           (config (cdr entry))
           (display-char (cond
                          ((string= name "user") "U")
                          ((string= name "buffer") "B")
                          ((string= name "project") "P")
                          ((string= name "global") "G")
                          (t "?")))
           (modifier (make-ai-modifier
                      :name name
                      :type :behavior
                      :config config
                      :display-char display-char
                      :description (format "Behavior modifier: %s" name))))
      (ai-command-management--register-modifier modifier)))

  ;; Register context modifiers
  (dolist (entry ai-command-management-file-command-context-modifiers)
    (let* ((name (car entry))
           (config (cdr entry))
           (display-char (cond
                          ((string= name "small") "s")
                          ((string= name "large") "L")
                          ((string= name "full") "F")
                          (t "?")))
           (modifier (make-ai-modifier
                      :name name
                      :type :context
                      :config config
                      :display-char display-char
                      :description (format "Context size: %s" name))))
      (ai-command-management--register-modifier modifier))))

;; ============================================================================
;; Command provider initialization
;; ============================================================================

(defun ai-command-management--initialize-command-providers ()
  "Initialize default command providers with file providers enabled."
  (setq ai-command-management--command-providers nil)

  ;; Register configuration command provider
  (ai-command-management--register-provider (ai-command-management--create-config-provider))

  ;; Register file providers (now enabled with fixed typo)
  (ai-command-management--register-provider (ai-command-management--create-file-provider :default))
  (ai-command-management--register-provider (ai-command-management--create-file-provider :global))
  (ai-command-management--register-provider (ai-command-management--create-file-provider :local))

  ;; Mark current project registry as dirty
  (ai-command-management--mark-project-registry-dirty))

;; ============================================================================
;; Command loaders for providers
;; ============================================================================

(defun ai-command-management--load-config-commands ()
  "Load commands from configuration map with proper ai-command structure and pre-loaded instructions."
  (ai-logging--message 'info "command-management" "Load config command")

  (let ((commands nil)
        (priority 100))
    (dolist (entry ai-command-management-commands-config-map)
      (let* ((name (car entry))
             (config-plist (cdr entry))

             ;; Load instructions from files for config commands during cache update
             (file-instructions (ai-prompt-management--get-instruction-content-with-fallback name))
             ;; Create proper config copy
             (final-config (copy-tree config-plist))
             ;; Create behavior struct
             (behavior (ai-command-management--create-behavior-from-config name final-config))
             ;; Get file path if instructions were loaded from file
             (file-path (when file-instructions
                          (ai-prompt-management-get-instruction-file-path name :default)))
             (instructions (or file-instructions (plist-get final-config :instructions)))

             (command (make-ai-command
                       :name name
                       :canonical-name name
                       :base-name name
                       :instructions instructions
                       :behavior behavior
                       :source :config
                       :location :config
                       :file-path file-path
                       :priority priority)))

        (push command commands)
        (setq priority (1+ priority))))
    commands))


(defun ai-command-management--load-file-commands (location)
  "Load commands from files for LOCATION with proper ai-command structure and all fields populated."
  (let ((commands nil)
        (priority (cond
                   ((eq location :local) 100)
                   ((eq location :global) 200)
                   ((eq location :default) 300)
                   (t 400))))

    ;; Get all instruction names for the specified location
    (let ((cache-type (cond
                       ((eq location :default) 'default)

                       ((eq location :global) 'global)
                       ((eq location :local) 'local)
                       (t 'default))))

      (dolist (name (ai-prompt-management--get-all-instruction-names-from-cache cache-type))
        (let* ((canonical-name name)
               (file-instructions (ai-prompt-management--get-instruction-content name))
               (file-path (ai-prompt-management-get-instruction-file-path name location))
               ;; Parse modifiers from name
               (parsed (ai-command-management--parse-modifiers-from-name name))
               (base-name (car parsed))
               (modifiers (cdr parsed))
               ;; Create configuration by applying modifiers
               (base-config ai-command-management-command-config)
               (config (if modifiers
                           (ai-command-management--apply-modifiers-to-config base-config modifiers)
                         base-config))
               ;; Create behavior struct
               (behavior (ai-command-management--create-behavior-from-config name config))
               ;; Use base name for display if there are modifiers
               (display-name (if modifiers base-name name))
               (command (make-ai-command
                         :name display-name
                         :canonical-name canonical-name
                         :base-name base-name
                         :instructions file-instructions
                         :behavior behavior
                         :source :file
                         :location location
                         :file-path file-path
                         :priority priority)))
          (push command commands)
          (setq priority (1+ priority)))))

    commands))

;; ============================================================================
;; Adaptation of existing functions to work with new system
;; ============================================================================

(defun ai-command-management--get-all-available-command-names ()
  "Get all available command names from registry."
  (mapcar #'ai-structs--get-command-name (ai-command-management--get-all-commands-from-registry)))

(defun ai-command-management--get-command-config-by-type (command &optional default-result-action)
  "Get command configuration by COMMAND from registry using ai-structs accessors."
  (let ((command-struct (ai-command-management--get-command-from-registry command)))
    (if command-struct
        (let ((config (ai-structs--get-command-config command-struct)))
          ;; Add AI command struct to config for context providers
          (plist-put config :ai-command-struct command-struct)
          ;; Apply default action if not specified
          (unless (plist-get config :result-action)
            (when default-result-action
              (setq config (plist-put config :result-action default-result-action))))
          config)
      ;; Fallback logic for unknown commands - create user input command
      (let ((user-cmd (ai-command-management--create-user-input-command command)))
        (let ((config (ai-structs--get-command-config user-cmd)))
          (plist-put config :ai-command-struct user-cmd)
          (when default-result-action
            (setq config (plist-put config :result-action default-result-action)))
          config)))))

(defun ai-command-management--get-command-display-name (command-name)
  "Get display name for COMMAND-NAME from registry."
  (let ((command-struct (ai-command-management--get-command-from-registry command-name)))
    (if command-struct
        (ai-command-management--format-command-display-name command-struct)
      ;; Fallback for legacy system
      (ai-command-management--get-legacy-command-display-name command-name))))

(defun ai-command-management--get-command ()
  "Prompt user to select command using new registry."
  (let* ((all-commands (ai-command-management--get-all-commands-from-registry))
         (command-displays (mapcar #'ai-command-management--format-command-display-name all-commands))
         (command-alist (cl-mapcar (lambda (display command)
                                    (cons display (ai-structs--get-command-canonical-name command)))
                                  command-displays all-commands))
         (user-input (completing-read ai-command-management-command-prompt command-displays nil nil))
         (selected-command (cdr (assoc user-input command-alist))))
    ;; If command not found in alist, return input as is
    (or selected-command user-input)))

;; ============================================================================
;; Legacy compatibility functions
;; ============================================================================

(defun ai-command-management--get-legacy-command-display-name (command-name)
  "Legacy command display name function for old system."
  (let ((indicators nil)
        (configured-p (assoc command-name ai-command-management-commands-config-map))
        (display-name command-name)
        (has-modifiers-in-name (string-match-p "__" command-name)))

    ;; Parse modifiers if command has modifiers in name
    (let* ((parsed (when has-modifiers-in-name (ai-command-management--parse-command-modifiers command-name)))
           (base-name (when parsed (car parsed)))
           (modifier-config (when parsed (cdr parsed))))

      ;; Use clean base name as display name when modifiers are present
      (when (and has-modifiers-in-name base-name)
        (setq display-name base-name))

      ;; For configured commands show [C] and don't process file logic
      (when configured-p
        (push "C" indicators))

      ;; For unconfigured commands check instruction files
      (unless configured-p
        ;; Always use full command name for cache lookup
        (when (ai-prompt-management--get-instruction-from-default-cache command-name)
          (push "D" indicators))
        (when (ai-prompt-management--get-instruction-from-global-cache command-name)
          (push "G" indicators))
        (when (ai-prompt-management--get-instruction-from-local-cache command-name)
          (push "L" indicators))

        ;; Apply modifier indicators only when command has modifiers in name
        (when (and has-modifiers-in-name modifier-config)
          ;; Add behavior modifier indicators
          (when (plist-get modifier-config :user-input)
            (push "U" indicators))  ; Requires user input
          (when (plist-get modifier-config :needs-buffer-context)
            (push "B" indicators))  ; Needs buffer context
          (when (plist-get modifier-config :needs-project-context)
            (push "P" indicators))  ; Needs project context
          (when (plist-get modifier-config :needs-global-context)
            (push "G" indicators))  ; Needs global context

          ;; Add context size indicators
          (when (plist-member modifier-config :preceding-context-size)
            (let ((preceding (plist-get modifier-config :preceding-context-size))
                  (following (plist-get modifier-config :following-context-size)))
              (cond
               ((and (null preceding) (null following))
                (push "F" indicators))  ; Full context
               ((and (eq preceding 5) (eq following 5))
                (push "s" indicators))  ; Small context
               ((and (eq preceding 20) (eq following 20))
                (push "L" indicators))  ; Large context
               (t nil))))

          ;; Add result action indicator
          (when-let ((result-action (plist-get modifier-config :result-action)))
            (let ((action-indicator (cond
                                    ((eq result-action 'show) "S")
                                    ((eq result-action 'eval) "E")
                                    ((eq result-action 'replace) "R")
                                    ((eq result-action 'insert-at-point) "I")
                                    ((eq result-action 'complete) "C")
                                    (t "?"))))
              (push action-indicator indicators))))))

    ;; Format final display name
    (let ((indicator-string (if indicators
                               (format "[%s]" (string-join (reverse indicators) ""))
                             "")))
      (if (string-empty-p indicator-string)
          display-name
        (format "%s %s" indicator-string display-name)))))

;; ============================================================================
;; Original functions (kept for compatibility)
;; ============================================================================

(defun ai-command-management--command-name-to-filename (command-name)
  "Convert COMMAND-NAME to a filesystem-safe filename with extension.
Returns nil if COMMAND-NAME is nil."
  (when command-name
    (concat (replace-regexp-in-string "[[:space:]]" "_" command-name) ai-command-management-instruction-file-extension)))

(defun ai-command-management--has-modifier-pattern-p (filename)
  "Check if FILENAME contains modifier patterns (double underscores).
Returns t if the filename appears to contain modifiers, nil otherwise."
  (let ((base-name (file-name-sans-extension filename)))
    (string-match-p "__" base-name)))

(defun ai-command-management--is-modifier-keyword-p (part)
  "Check if PART is a known modifier keyword.
Returns t if PART matches any known modifier from action, behavior, or context modifiers."
  (or (assoc part ai-command-management-file-command-action-modifiers)
      (assoc part ai-command-management-file-command-behavior-modifiers)
      (assoc part ai-command-management-file-command-context-modifiers)))

(defun ai-command-management--filename-to-command-name (filename)
  "Convert FILENAME back to command name.
Returns the base filename without extension, preserving the full structure."
  (file-name-sans-extension filename))

(defun ai-command-management--get-base-command-name-for-examples (command-name)
  "Extract base command name for finding examples, handling modifiers.
Convert spaces to underscores for file lookup."
  (let* ((parsed (ai-command-management--parse-command-modifiers command-name))
         (base-name (car parsed)))
    ;; Convert spaces back to underscores for file lookup
    (replace-regexp-in-string "[[:space:]]" "_" base-name)))

(defun ai-command-management-get-instructions (command-name)
  "Get instructions for COMMAND-NAME using priority: Local > Global > Default."
  (ai-prompt-management--get-instruction-content command-name))

(defun ai-command-management--get-command-examples (command-name)
  "Get examples for COMMAND-NAME using priority: Local > Global > Default."
  (ai-prompt-management--get-examples-content command-name))

(defun ai-command-management--get-rendered-command-examples (command context)
  "Get examples for COMMAND, render with CONTEXT, and return result."
  (when-let ((examples-content (ai-command-management--get-command-examples command)))
    (ai-prompt-management--render-template examples-content context)))

(defun ai-command-management--get-all-file-based-command-names ()
  "Get all command names from instruction files only (not from config).
Commands are collected from all instruction locations but not from ai-command-management-commands-config-map.
Uses name normalization to prevent duplicates with configured commands."
  (let ((commands-set (make-hash-table :test 'equal))
        (configured-commands (mapcar #'car ai-command-management-commands-config-map))
        ;; Create normalized set of configured commands for comparison
        (normalized-configured-set (make-hash-table :test 'equal))
        (file-commands nil))

    ;; Populate normalized set of configured command names
    (dolist (cmd configured-commands)
      (puthash (ai-prompt-management--normalize-name-for-lookup cmd) t normalized-configured-set))

    ;; Get all available file-based commands from each cache
    (dolist (cache-type '(default global local))
      (let ((names (ai-prompt-management--get-all-instruction-names-from-cache cache-type)))
        (dolist (name names)
          (let* ((command-name (ai-command-management--filename-to-command-name name))
                 (normalized-name (ai-prompt-management--normalize-name-for-lookup command-name)))
            ;; Check against normalized configured commands to avoid duplicates
            (unless (or (gethash normalized-name normalized-configured-set)
                        (gethash command-name commands-set))
              (puthash command-name t commands-set)
              (push command-name file-commands))))))

    ;; Sort and return
    (sort file-commands #'string<)))

(defun ai-command-management--parse-command-modifiers (command-name)
  "Parse modifiers from COMMAND-NAME and return (base-name . config-plist).
Modifiers are separated by '__' and can be anywhere in the command name.
Command parts are joined with spaces. Example: 'user__explain_code__show' -> ('explain code' . (:user-input t :result-action show))"
  (let ((parts (split-string command-name "__"))
        (config '())
        (command-parts '()))

    ;; Separate modifier parts from command parts
    (dolist (part parts)
      (if (ai-command-management--is-modifier-keyword-p part)
          ;; Process modifier
          (cond
           ;; Check action modifiers
           ((assoc part ai-command-management-file-command-action-modifiers)
            (setq config (plist-put config :result-action
                                   (cdr (assoc part ai-command-management-file-command-action-modifiers)))))
           ;; Check behavior modifiers
           ((assoc part ai-command-management-file-command-behavior-modifiers)
            (let ((behavior-config (cdr (assoc part ai-command-management-file-command-behavior-modifiers))))
              (while behavior-config
                (setq config (plist-put config (car behavior-config) (cadr behavior-config)))
                (setq behavior-config (cddr behavior-config)))))
           ;; Check context modifiers
           ((assoc part ai-command-management-file-command-context-modifiers)
            (let ((context-config (cdr (assoc part ai-command-management-file-command-context-modifiers))))
              (while context-config
                (setq config (plist-put config (car context-config) (cadr context-config)))
                (setq context-config (cddr context-config))))))
        ;; Not a modifier - it's part of the command name
        (push part command-parts)))

    ;; Construct base name from command parts (convert underscores to spaces)
    (let ((base-name (if command-parts
                        (mapconcat (lambda (part) (replace-regexp-in-string "_" " " part))
                                  (reverse command-parts) " ")
                      "")))
      (cons base-name config))))

(defun ai-command-management--get-file-command-config (command-name)
  "Get configuration for file-based command with modifier parsing.
Returns a configuration plist with parsed modifiers applied."
  (let* ((parsed (ai-command-management--parse-command-modifiers command-name))
         (base-name (car parsed))
         (modifier-config (cdr parsed))
         (base-config ai-command-management-command-config))

    ;; Merge base configuration with modifier configuration
    ;; Modifier config takes precedence over base config
    (let ((final-config (copy-sequence base-config)))
      (while modifier-config
        (let ((key (car modifier-config))
              (value (cadr modifier-config)))
          (setq final-config (plist-put final-config key value)))
        (setq modifier-config (cddr modifier-config)))
      final-config)))

(defun ai-command-management--get-rendered-command-instructions (command context)
  "Get instructions for COMMAND, render it with CONTEXT, and return the result.
If no instructions are found for COMMAND, returns nil."
  (ai-prompt-management--render-instruction command context))

(defun ai-command-management--get-action-type-object-instructions (action-type context)
  "Get the instructions for ACTION-TYPE rendered with CONTEXT."
  (ai-prompt-management--render-action-type-object-instructions action-type context))

(defun ai-command-management--describe-command-modifiers (command-name)
  "Generate a description of the modifiers for COMMAND-NAME."
  (let* ((parsed (ai-command-management--parse-command-modifiers command-name))
         (base-name (car parsed))
         (modifier-config (cdr parsed)))

    (if (or (string-empty-p base-name) (null modifier-config))
        (format "Command '%s' has no modifiers." command-name)
      (let ((descriptions nil))
        ;; Describe parsed modifiers
        (when (plist-get modifier-config :user-input)
          (push "requires user input" descriptions))
        (when (plist-get modifier-config :needs-buffer-context)
          (push "uses full buffer context" descriptions))
        (when (plist-get modifier-config :needs-project-context)
          (push "uses project context" descriptions))
        (when (plist-get modifier-config :needs-global-context)
          (push "uses global context" descriptions))
        (when-let ((result-action (plist-get modifier-config :result-action)))
          (push (format "result action: %s" (symbol-name result-action)) descriptions))
        (when (plist-member modifier-config :preceding-context-size)
          (let ((preceding (plist-get modifier-config :preceding-context-size))
                (following (plist-get modifier-config :following-context-size)))
            (if (and (null preceding) (null following))
                (push "uses full context size" descriptions)
              (push (format "context size: %s/%s lines"
                           (or preceding "default")
                           (or following "default")) descriptions))))

        (format "Command '%s' (base: '%s') has modifiers: %s"
                command-name
                base-name
                (if descriptions
                    (string-join descriptions ", ")
                  "none"))))))

(defun ai-command-management--get-ordered-command-names ()
  "Get all available command names ordered with configured commands first, then file-based commands."
  (let ((configured-commands (mapcar #'car ai-command-management-commands-config-map))
        (file-based-commands (ai-command-management--get-all-file-based-command-names)))
    (append configured-commands file-based-commands)))

(defun ai-command-management--get-informational-command ()
  "Prompt the user to select an informational command, filtering by :result-action 'show'."
  (let* ((configured-show-commands
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'show))
                   ai-command-management-commands-config-map)))
         ;; For unconfigured commands, we default to 'show', so include all available commands
         (all-commands (ai-command-management--get-ordered-command-names))
         (command-displays (mapcar #'ai-command-management--get-command-display-name all-commands))
         (command-alist (cl-mapcar #'cons command-displays all-commands))
         (selected-display (completing-read ai-command-management-command-prompt command-displays nil nil))
         (selected-command (cdr (assoc selected-display command-alist))))
    ;; If command is not found in alist, return the input as-is
    (or selected-command selected-display)))

(defun ai-command-management--get-executable-command ()
  "Prompt the user to select an executable command, filtering by :result-action 'eval'."
  (let* ((eval-commands
          (mapcar #'car
                  (cl-remove-if-not
                   (lambda (item)
                     (eq (map-elt (cdr item) :result-action) 'eval))
                   ai-command-management-commands-config-map)))
         (command-displays (mapcar #'ai-command-management--get-command-display-name eval-commands))
         (command-alist (cl-mapcar #'cons command-displays eval-commands))
         (selected-display (completing-read ai-command-management-command-prompt command-displays nil nil))
         (selected-command (cdr (assoc selected-display command-alist))))
    ;; If command is not found in alist, return the input as-is
    (or selected-command selected-display)))

(defun ai-command-management--get-all-available-commands ()
  "Get all available commands including those from config and additional actions."
  (ai-command-management--get-ordered-command-names))

(defun ai-command-management--get-command-unrestricted ()
  "Prompt the user to select any available command without restrictions."
  (ai-command-management--get-command))

(defun ai-command-management--get-command-for-editing ()
  "Get command name from user input with completion but allowing arbitrary input.
Shows configured commands first, then file-based commands."
  (let* ((configured-commands (mapcar #'car ai-command-management-commands-config-map))
         ;; Get all available commands but exclude those already in configured
         (all-commands (ai-command-management--get-all-available-command-names))
         (file-only-commands (cl-remove-if (lambda (cmd) (member cmd configured-commands)) all-commands))
         ;; Create ordered list: configured first, then file-only alphabetically sorted
         (ordered-commands (append configured-commands (sort file-only-commands #'string<)))
         (command-displays (mapcar #'ai-command-management--get-command-display-name ordered-commands))
         (command-alist (cl-mapcar #'cons command-displays ordered-commands))
         (input (completing-read "Enter command name: " command-displays nil nil)))
    ;; If input matches a display name, return the actual command name
    (or (cdr (assoc input command-alist))
        ;; Otherwise return the input as-is (for new commands)
        input)))

(defun ai-command-management--get-available-modifiers ()
  "Get all available modifiers categorized by type.
Returns an alist with categories as keys and modifier lists as values."
  `((action . ,(mapcar #'car ai-command-management-file-command-action-modifiers))
    (behavior . ,(mapcar #'car ai-command-management-file-command-behavior-modifiers))
    (context . ,(mapcar #'car ai-command-management-file-command-context-modifiers))))

(defun ai-command-management-describe-command-modifiers ()
  "Interactively describe modifiers for a selected command."
  (interactive)
  (let* ((command (ai-command-management--get-command))
         (description (ai-command-management--describe-command-modifiers command)))
    (ai-logging--message 'info "command-management" "%s" description)))

(defun ai-command-management-create-modified-command ()
  "Interactively create a new file-based command with modifiers."
  (interactive)
  (let* ((base-name (read-string "Base command name: "))
         (available-modifiers (ai-command-management--get-available-modifiers))
         (selected-modifiers '())
         (modifier-string ""))

    ;; Select action modifier
    (when (y-or-n-p "Add result action modifier? ")
      (let ((action (completing-read "Select action: "
                                    (cdr (assoc 'action available-modifiers)))))
        (push action selected-modifiers)))

    ;; Select behavior modifiers
    (when (y-or-n-p "Add behavior modifiers? ")
      (let ((behaviors (completing-read-multiple "Select behaviors (comma-separated): "
                                                 (cdr (assoc 'behavior available-modifiers)))))
        (setq selected-modifiers (append behaviors selected-modifiers))))

    ;; Select context modifier
    (when (y-or-n-p "Add context size modifier? ")
      (let ((context (completing-read "Select context size: "
                                     (cdr (assoc 'context available-modifiers)))))
        (push context selected-modifiers)))

    ;; Construct command name
    (when selected-modifiers
      (setq modifier-string (string-join (reverse selected-modifiers) "__")))

    (let ((full-command-name (if (string-empty-p modifier-string)
                                base-name
                              (format "%s__%s" modifier-string base-name))))
      (ai-logging--message 'info "command-management" "Suggested command name: %s" full-command-name)
      (when (y-or-n-p "Create instruction file for this command? ")
        ;; Check if instructions already exist
        (let* ((existing-instructions (ai-command-management-get-instructions full-command-name))
               (file-exists existing-instructions))
          (when file-exists
            (ai-logging--message 'warn "command-management" "Instructions for command '%s' already exist" full-command-name))
          ;; Create instruction file for the specific command name
          (ai-logging--message 'debug "command-management" "Creating instruction file for command: %s" full-command-name)
          (ai-command-management-edit-command-instructions-for-name full-command-name))))))

(defun ai-command-management--get-available-edit-locations (command-name)
  "Get available editing locations for COMMAND-NAME.
Returns a list of plists with :name, :symbol, :directory, :file-path, and :exists-p keys."
  (let ((locations nil)
        (global-dir (ai-prompt-management--get-global-instructions-directory))
        (local-dir (ai-prompt-management--get-local-instructions-directory)))

    ;; Global location
    (when global-dir
      (let* ((file-path (ai-prompt-management--get-file-path-for-name command-name global-dir))
             (exists-p (and file-path (file-exists-p file-path))))
        (push `(:name "Global"
                :symbol global
                :directory ,global-dir
                :file-path ,file-path
                :exists-p ,exists-p)
              locations)))

    ;; Local location (project-specific)
    (when local-dir
      (let* ((file-path (ai-prompt-management--get-file-path-for-name command-name local-dir))
             (exists-p (and file-path (file-exists-p file-path))))
        (push `(:name "Local"
                :symbol local
                :directory ,local-dir
                :file-path ,file-path
                :exists-p ,exists-p)
              locations)))

    ;; Reverse to get Global first, then Local
    (reverse locations)))

(defun ai-command-management--format-location-display (location-info)
  "Format LOCATION-INFO for display in completing-read.
LOCATION-INFO is a plist with location details."
  (let* ((name (plist-get location-info :name))
         (file-path (plist-get location-info :file-path))
         (exists-p (plist-get location-info :exists-p))
         (status (if exists-p "[EXISTS]" "[NEW]")))
    (format "%s: %s %s" name file-path status)))

(defun ai-command-management--select-edit-location (command-name locations)
  "Select editing location for COMMAND-NAME from LOCATIONS.
Returns the selected location plist."
  (if (= (length locations) 1)
      ;; If only one location available, use it directly
      (car locations)
    ;; Otherwise, let user choose
    (let* ((location-displays (mapcar #'ai-command-management--format-location-display locations))
           (location-alist (cl-mapcar #'cons location-displays locations))
           (selected-display (completing-read "Select location to edit: " location-displays nil t)))
      (cdr (assoc selected-display location-alist)))))

(defun ai-command-management--open-instruction-file (command-name location-info)
  "Open instruction file for COMMAND-NAME at LOCATION-INFO for editing.
LOCATION-INFO is a plist with location details."
  (let* ((directory (plist-get location-info :directory))
         (file-path (plist-get location-info :file-path))
         (exists-p (plist-get location-info :exists-p)))

    (unless file-path
      (user-error "Cannot determine file path for command: %s" command-name))

    ;; Ensure directory exists
    (unless (file-directory-p directory)
      (make-directory directory t))

    ;; Open file for editing
    (find-file file-path)

    ;; If file is new, add template content
    (when (and (not exists-p) (= (point-min) (point-max)))
      (insert (format "# Instructions for command: %s\n\n" command-name)
              "Write your AI instructions here...\n"))))

(defun ai-command-management-edit-command-instructions ()
  "Interactively edit instruction files for commands with improved interface."
  (interactive)
  (let* ((command-name (ai-command-management--get-command-for-editing))
         (available-locations (ai-command-management--get-available-edit-locations command-name)))

    (unless available-locations
      (user-error "No editable locations available. Make sure you're in a project or global config is accessible"))

    (let ((selected-location (ai-command-management--select-edit-location command-name available-locations)))
      (ai-command-management--open-instruction-file command-name selected-location))))

(defun ai-command-management-edit-command-instructions-for-name (command-name)
  "Edit instruction file for specific COMMAND-NAME without user prompt."
  (let ((available-locations (ai-command-management--get-available-edit-locations command-name)))
    (unless available-locations
      (user-error "No editable locations available. Make sure you're in a project or global config is accessible"))
    (let ((selected-location (ai-command-management--select-edit-location command-name available-locations)))
      (ai-command-management--open-instruction-file command-name selected-location))))


(defun ai-command-management-list-commands ()
  "List all registered commands in *messages* buffer without cache update."
  (interactive)
  (let* ((project-key (ai-command-management--get-buffer-project-key))
         (registry (ai-command-management--get-project-registry project-key))
         (commands nil)
         (command-count 0))

    ;; Collect all commands from registry
    (maphash (lambda (_name command)
               (when (ai-command-p command)
                 (push command commands)
                 (setq command-count (1+ command-count))))
             registry)

    ;; Sort commands by priority
    (setq commands (sort commands (lambda (a b)
                                    (< (ai-structs--get-command-priority a)
                                       (ai-structs--get-command-priority b)))))

    ;; Display commands in *messages*
    (message "=== AI Commands Registry for project '%s' ===" project-key)
    (message "Total commands: %d" command-count)
    (message "")

    (if commands
        (dolist (command commands)
          (let* ((name (ai-structs--get-command-name command))
                 (canonical-name (ai-structs--get-command-canonical-name command))
                 (source (ai-structs--get-command-source command))
                 (location (ai-structs--get-command-location command))
                 (priority (ai-structs--get-command-priority command))
                 (result-action (ai-structs--get-result-action command))
                 (needs-input (ai-structs--command-needs-user-input-p command))
                 (needs-buffer (ai-structs--command-needs-buffer-context-p command))
                 (file-path (ai-structs--get-command-file-path command)))

            (message "[%d] %s" priority name)
            (message "  Canonical: %s" canonical-name)
            (message "  Source: %s | Location: %s | Action: %s" source location result-action)
            (when needs-input
              (message "  Requires user input"))
            (when needs-buffer
              (message "  Needs buffer context"))
            (when file-path
              (message "  File: %s" (file-name-nondirectory file-path)))
            (message "")

            ;; Pretty print the full command structure
            (message "  Full structure:")
            (let ((pretty-printed (pp-to-string command)))
              (dolist (line (split-string pretty-printed "\n"))
                (when (not (string-empty-p line))
                  (message "    %s" line))))
            (message "")))
      (message "No commands found in registry"))

    (message "=== End of Commands Registry ===")))


(defun ai-command-management--print-command-summary (command)
  "Print COMMAND as a single consolidated entry with all fields."
  (unless (ai-command-p command)
    (error "COMMAND must be an ai-command struct: %s" command))

  (let* ((name (ai-structs--get-command-name command))
         (canonical-name (ai-structs--get-command-canonical-name command))
         (base-name (ai-structs--get-command-base-name command))
         (instructions (ai-structs--get-command-instructions command))
         (behavior (ai-structs--get-command-behavior command))
         (source (ai-structs--get-command-source command))
         (location (ai-structs--get-command-location command))
         (file-path (ai-structs--get-command-file-path command))
         (priority (ai-structs--get-command-priority command))
         ;; Truncate instructions to first 40 characters
         (truncated-instructions (when instructions
                                   (if (> (length instructions) 40)
                                       (concat (substring instructions 0 40) "...")
                                     instructions)))
         ;; Build behavior string
         (behavior-str (if behavior
                          (format "[Input:%s Action:%s Result:%s Buffer:%s Project:%s Global:%s Pre:%s Post:%s]"
                                  (ai-command-behavior-user-input behavior)
                                  (ai-command-behavior-action-type behavior)
                                  (ai-command-behavior-result-action behavior)
                                  (ai-command-behavior-needs-buffer-context behavior)
                                  (ai-command-behavior-needs-project-context behavior)
                                  (ai-command-behavior-needs-global-context behavior)
                                  (ai-command-behavior-preceding-context-size behavior)
                                  (ai-command-behavior-following-context-size behavior))
                        "nil")))

    (message "Command: %s | Canonical: %s | Base: %s | Source: %s | Location: %s | Priority: %s | File: %s | Behavior: %s | Instructions: %s"
             name
             canonical-name
             base-name
             source
             location
             priority
             (or (when file-path (file-name-nondirectory file-path)) "nil")
             behavior-str
             (or truncated-instructions "nil"))))

(defun ai-command-management-print-command ()
  "Interactively select and print a command summary with truncated instructions."
  (interactive)
  (ai-command-management--ensure-registry-updated)
  (let* ((all-commands (ai-command-management--get-all-commands-from-registry))
         (command-displays (mapcar #'ai-command-management--format-command-display-name all-commands))
         (command-alist (cl-mapcar #'cons command-displays all-commands))
         (selected-display (completing-read "Select command to print: " command-displays nil t))
         (selected-command (cdr (assoc selected-display command-alist))))

    (if selected-command
        (ai-command-management--print-command-summary selected-command)
      (message "No command selected"))))


(defun ai-command-management-update-commands-cache ()
  "Update command caches for all locations and mark all project registries dirty."
  (ai-prompt-management--update-caches)
  ;; Mark all project registries as dirty to force reload
  (clrhash ai-command-management--registries-dirty)
  (maphash (lambda (project-key _registry)
             (puthash project-key t ai-command-management--registries-dirty))
           ai-command-management--project-command-registries))

(defun ai-command-management-update-system-prompts-cache ()
  "Update system prompts cache for all locations."
  (ai-prompt-management--update-caches))

;; ============================================================================
;; New system initialization
;; ============================================================================

;; Initialize modifier system when module loads
(ai-command-management--initialize-modifier-registry)

;; Initialize command providers when module loads (now with file providers enabled)
(ai-command-management--initialize-command-providers)

;; Register hook to update command registries when prompt caches are updated
(ai-prompt-management-add-cache-update-hook #'ai-command-management--on-prompt-cache-update)

(provide 'ai-command-management)

;;; ai-command-management.el ends here
