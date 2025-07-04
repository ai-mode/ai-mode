;;; ai-model-management.el --- Model management for AI mode -*- lexical-binding: t -*-

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
;; This module provides model management functionality for AI mode,
;; including model registration, selection, configuration, and switching
;; between different AI backends and their respective models.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'ai-utils)

(defvar ai-model-management-providers nil
  "List of functions that provide AI model configurations.
Each function should return a list of model plists.")

(defcustom ai-model-management-execution-model nil
  "The current backend used to execute requests asynchronously."
  :group 'ai-mode)

(defvar ai-model-management-change-hook nil
  "Hook that is run when execution model changes.")

(defcustom ai-model-management-change-execution-backend-prompt "Select execution backend: "
  "Prompt for selecting backend."
  :type 'string
  :group 'ai-mode)

(defun ai-model-management-get-available ()
  "Return a flat list of available AI models retrieved from multiple sources."
  (let ((model-funcs ai-model-management-providers))
    (apply #'append (mapcar #'funcall model-funcs))))

(defun ai-model-management-get-current ()
  "Return the currently selected execution model or set a default if none is selected."
  (or ai-model-management-execution-model
      (let ((default-model (car (ai-model-management-get-available))))
        (ai-model-management-set default-model)
        default-model)))

(defun ai-model-management-set (model)
  "Set the execution model and execute hooks.
MODEL is the model configuration to be set."
  (let ((setup-function (map-elt model :setup-function)))
    (when setup-function
      (funcall setup-function))
    (setq ai-model-management-execution-model model)
    (customize-save-variable 'ai-model-management-execution-model model) ; Save the setting persistently
    (run-hooks 'ai-model-management-change-hook)))

(defun ai-model-management-change (&optional model-name)
  "Change command backend interactively, or use MODEL-NAME if given."
  (interactive)
  (let* ((models (mapcar (lambda (item) `(,(map-elt item :name) ,item)) (ai-model-management-get-available)))
         (value (or model-name
                    (completing-read ai-model-management-change-execution-backend-prompt (mapcar #'car models))))
         (model (ai-model-management--find-model-config-by-name value (ai-model-management-get-available))))

    (message "Setup model: %s" (pp-to-string model))
    (ai-model-management-set model)
    (message "AI mode backend changed to '%s'." value)))

(defun ai-model-management--get-model-key (model key)
  "Retrieve the value for KEY from MODEL."
  (map-elt model key))

(defun ai-model-management--find-model-config-by-name (name elements)
  "Search ELEMENTS for a model configuration matching NAME and return it."
  (cl-find-if (lambda (element)
                (string= (cdr (assoc :name element)) name))
              elements))

(provide 'ai-model-management)

;;; ai-model-management.el ends here
