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

;;; Code:

(require 'cl-lib)
(require 'ai-utils)
(require 'ai-common)
(require 'ai-prompt-management)

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
  '(("full" . (:preceding-context-size nil :following-context-size nil))
    ("small" . (:preceding-context-size 5 :following-context-size 5))
    ("large" . (:preceding-context-size 20 :following-context-size 20)))
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
  `eval` (display and offer to evaluate), `insert-at-point` (insert at cursor).
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
  "Convert FILENAME back to command name intelligently.
For files with modifier patterns, preserve the original structure with __.
For simple files, convert underscores to spaces for backward compatibility."
  (let ((base-name (file-name-sans-extension filename)))
    (if (ai-command-management--has-modifier-pattern-p filename)
        ;; File has modifier patterns - preserve __ structure
        ;; but convert single _ to spaces only in non-modifier parts
        (let ((parts (split-string base-name "__")))
          (mapconcat (lambda (part)
                       (if (ai-command-management--is-modifier-keyword-p part)
                           part  ; Keep modifier keywords unchanged
                         (replace-regexp-in-string "_" " " part)))  ; Convert _ to space in command parts
                     parts "__"))
      ;; Simple file without modifiers - convert all underscores to spaces
      (replace-regexp-in-string "_" " " base-name))))

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
Commands are collected from all instruction locations but not from ai-command-management-commands-config-map."
  (let ((commands-set (make-hash-table :test 'equal))
        (configured-commands (mapcar #'car ai-command-management-commands-config-map))
        (file-commands nil))

    ;; Get all available file-based commands from prompt management
    (let ((all-dirs '()))
      ;; Collect all instruction directories
      (when-let ((default-dir (ai-prompt-management--get-default-instructions-directory)))
        (push default-dir all-dirs))
      (when-let ((global-dir (ai-prompt-management--get-global-instructions-directory)))
        (push global-dir all-dirs))
      (when-let ((local-dir (ai-prompt-management--get-local-instructions-directory)))
        (push local-dir all-dirs))

      ;; Scan each directory for command files
      (dolist (dir all-dirs)
        (when (file-directory-p dir)
          (let ((files (directory-files dir nil (concat "\\." (regexp-quote (substring ai-command-management-instruction-file-extension 1)) "$"))))
            (dolist (file files)
              ;; Skip .examples.md files
              (unless (string-match-p "\\.examples\\." file)
                (let ((command-name (ai-command-management--filename-to-command-name file)))
                  ;; Add to list if not configured and not already seen
                  (unless (or (member command-name configured-commands)
                             (gethash command-name commands-set)))
                    (puthash command-name t commands-set)
                    (push command-name file-commands))))))))

    ;; Sort and return
    (sort file-commands #'string<)))

(defun ai-command-management--get-all-available-command-names ()
  "Get all available command names from all instruction locations plus configured commands.
Commands from ai-command-management-commands-config-map appear first in their original order."
  (let ((configured-commands (mapcar #'car ai-command-management-commands-config-map))
        (file-based-commands (ai-command-management--get-all-file-based-command-names)))
    (append configured-commands file-based-commands)))

(defun ai-command-management--parse-command-modifiers (command-name)
  "Parse modifiers from COMMAND-NAME and return (base-name . config-plist).
Modifiers are separated by '__' and can be prefixes or suffixes.
Example: 'user__explain_code__show' -> ('explain_code' . (:user-input t :result-action show))"
  (let ((parts (split-string command-name "__"))
        (config '())
        (base-name command-name))

    (when (> (length parts) 1)
      ;; Multiple parts found, parse modifiers
      (let ((all-parts parts)
            (identified-modifiers '())
            (remaining-parts '()))

        ;; Identify all modifier parts
        (dolist (part all-parts)
          (cond
           ;; Check action modifiers
           ((assoc part ai-command-management-file-command-action-modifiers)
            (push (cons 'action part) identified-modifiers)
            (setq config (plist-put config :result-action
                                   (cdr (assoc part ai-command-management-file-command-action-modifiers)))))
           ;; Check behavior modifiers
           ((assoc part ai-command-management-file-command-behavior-modifiers)
            (push (cons 'behavior part) identified-modifiers)
            (let ((behavior-config (cdr (assoc part ai-command-management-file-command-behavior-modifiers))))
              (setq config (append config behavior-config))))
           ;; Check context modifiers
           ((assoc part ai-command-management-file-command-context-modifiers)
            (push (cons 'context part) identified-modifiers)
            (let ((context-config (cdr (assoc part ai-command-management-file-command-context-modifiers))))
              (setq config (append config context-config))))
           ;; Not a modifier, part of base name
           (t
            (push part remaining-parts))))

        ;; Construct base name from remaining non-modifier parts
        (setq base-name (if remaining-parts
                           (string-join (reverse remaining-parts) "__")
                         (car all-parts)))))

    (cons base-name config)))

(defun ai-command-management--get-file-command-config (command-name)
  "Get configuration for file-based command with modifier parsing.
Returns a configuration plist with parsed modifiers applied."
  (let* ((parsed (ai-command-management--parse-command-modifiers command-name))
         (base-name (car parsed))
         (modifier-config (cdr parsed))
         (base-config ai-command-management-command-config))

    ;; Merge base configuration with modifier configuration
    ;; Modifier config takes precedence over base config
    (let ((final-config base-config))
      (while modifier-config
        (let ((key (car modifier-config))
              (value (cadr modifier-config)))
          (setq final-config (plist-put final-config key value)))
        (setq modifier-config (cddr modifier-config)))
      final-config)))

(defun ai-command-management--get-command-display-name (command-name)
  "Get display name for COMMAND-NAME with indicators for instruction sources and modifiers.
Shows configured commands [C], instruction sources [D/G/L], and modifier indicators.
For file-based commands with modifiers, displays clean base name with modifier indicators."
  (let ((indicators nil)
        (configured-p (assoc command-name ai-command-management-commands-config-map))
        (display-name command-name))

    ;; Check for instruction files from different sources using prompt management
    (when (ai-prompt-management--get-instruction-content command-name)
      ;; Check each source individually for more specific indicators
      (when-let ((default-dir (ai-prompt-management--get-default-instructions-directory)))
        (when (ai-prompt-management--read-file-content
               (ai-prompt-management--get-file-path-for-name command-name default-dir))
          (push "D" indicators)))
      (when-let ((global-dir (ai-prompt-management--get-global-instructions-directory)))
        (when (ai-prompt-management--read-file-content
               (ai-prompt-management--get-file-path-for-name command-name global-dir))
          (push "G" indicators)))
      (when-let ((local-dir (ai-prompt-management--get-local-instructions-directory)))
        (when (ai-prompt-management--read-file-content
               (ai-prompt-management--get-file-path-for-name command-name local-dir))
          (push "L" indicators))))

    ;; Parse and display modifiers for file-based commands
    (unless configured-p
      (when (ai-command-management-get-instructions command-name)
        (let* ((parsed (ai-command-management--parse-command-modifiers command-name))
               (base-name (car parsed))
               (modifier-config (cdr parsed)))

          ;; Use clean base name if command has modifiers
          (when (and modifier-config (not (string= command-name base-name)))
            (setq display-name base-name))

          ;; Add modifier indicators
          (when (plist-get modifier-config :user-input)
            (push "U" indicators))  ; User input required
          (when (plist-get modifier-config :needs-buffer-context)
            (push "B" indicators))  ; Buffer context needed
          (when (plist-get modifier-config :needs-project-context)
            (push "P" indicators))  ; Project context needed

          ;; Add result action indicator
          (when-let ((result-action (plist-get modifier-config :result-action)))
            (let ((action-indicator (cond
                                    ((eq result-action 'show) "S")
                                    ((eq result-action 'eval) "E")
                                    ((eq result-action 'replace) "R")
                                    ((eq result-action 'insert-at-point) "I")
                                    ((eq result-action 'complete) "C")
                                    (t "?"))))
              (push action-indicator indicators)))

          ;; Add context size indicators
          (when (plist-member modifier-config :preceding-context-size)
            (let ((preceding (plist-get modifier-config :preceding-context-size))
                  (following (plist-get modifier-config :following-context-size)))
              (cond
               ((and (null preceding) (null following))
                (push "F" indicators))  ; Full context
               ((and (numberp preceding) (< preceding 10))
                (push "s" indicators))  ; Small context
               ((and (numberp preceding) (> preceding 15))
                (push "L" indicators))  ; Large context
               (t nil)))))))

    (format "%s%s %s"
            (if configured-p "[C]" "")
            (if indicators
                (format "[%s]" (string-join (reverse indicators) ""))
              "")
            display-name)))

(defun ai-command-management--get-command-config-by-type (command &optional default-result-action)
  "Get command config by COMMAND, applying DEFAULT-RESULT-ACTION for unknown commands.
Now supports modifier parsing for file-based commands with instruction files."
  (if-let (config (cdr (assoc command ai-command-management-commands-config-map)))
      ;; Command found in configuration map
      (append config `(:action ,command))
    (if (string= command "complete")
        ;; Special case for completion
        ai-command-management-completion-config
      ;; Check if this is a file-based command with instructions
      (if (ai-command-management-get-instructions command)
          ;; File-based command - parse modifiers and apply configuration
          (let ((file-config (ai-command-management--get-file-command-config command)))
            ;; Apply default result action if none specified by modifiers
            (unless (plist-get file-config :result-action)
              (when default-result-action
                (setq file-config (plist-put file-config :result-action default-result-action))))
            file-config)
        ;; Regular command without instructions - use base configuration
        (let ((base-config (append ai-command-management-command-config `(:command ,command))))
          (if default-result-action
              (plist-put base-config :result-action default-result-action)
            base-config))))))

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

    (if (string= command-name base-name)
        (format "Command '%s' has no modifiers." command-name)
      (let ((descriptions nil))
        ;; Describe parsed modifiers
        (when (plist-get modifier-config :user-input)
          (push "requires user input" descriptions))
        (when (plist-get modifier-config :needs-buffer-context)
          (push "uses full buffer context" descriptions))
        (when (plist-get modifier-config :needs-project-context)
          (push "uses project context" descriptions))
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

(defun ai-command-management--get-command ()
  "Prompt the user to select the command using `completing-read`."
  (let* ((all-commands (ai-command-management--get-ordered-command-names))
         (command-displays (mapcar #'ai-command-management--get-command-display-name all-commands))
         (command-alist (cl-mapcar #'cons command-displays all-commands))
         (selected-display (completing-read ai-command-management-command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

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
         (selected-display (completing-read ai-command-management-command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

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
         (selected-display (completing-read ai-command-management-command-prompt command-displays))
         (selected-command (cdr (assoc selected-display command-alist))))
    selected-command))

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
    (message "%s" description)))

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
      (message "Suggested command name: %s" full-command-name)
      (when (y-or-n-p "Create instruction file for this command? ")
        ;; Check if instructions already exist
        (let* ((existing-instructions (ai-command-management-get-instructions full-command-name))
               (file-exists existing-instructions))
          (when file-exists
            (message "Warning: Instructions for command '%s' already exist" full-command-name))
          ;; Create instruction file for the specific command name
          (message "Creating instruction file for command: %s" full-command-name)
          (ai-command-management-edit-command-instructions-for-name full-command-name))))))

(defun ai-command-management--get-available-edit-locations (command-name)
  "Get available editing locations for COMMAND-NAME.
Returns a list of plists with :name, :symbol, :directory, :file-path, and :exists-p keys."
  (let ((locations nil))
    ;; Global location
    (when-let ((global-dir (ai-prompt-management--get-global-instructions-directory)))
      (let* ((file-path (ai-prompt-management--get-file-path-for-name command-name global-dir))
             (exists-p (and file-path (file-exists-p file-path))))
        (push `(:name "Global"
                :symbol global
                :directory ,global-dir
                :file-path ,file-path
                :exists-p ,exists-p)
              locations)))

    ;; Local location (project-specific)
    (when-let ((local-dir (ai-prompt-management--get-local-instructions-directory)))
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

(defun ai-command-management-update-commands-cache ()
  "Update command caches for all locations."
  (ai-prompt-management--update-caches))

(defun ai-command-management-update-system-prompts-cache ()
  "Update system prompts cache for all locations."
  (ai-prompt-management--update-caches))

(provide 'ai-command-management)

;;; ai-command-management.el ends here
