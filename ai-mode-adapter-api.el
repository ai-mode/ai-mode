;;; ai-mode-adapter-api.el --- Adapter functions for ai-mode with Google Generative AI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alex (https://github.com/lispython)

;; Author: Alex <https://github.com/lispython>
;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai, adapter, api, generative-ai,

;; This file is part of AI Mode.

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
;; AI Mode Adapter API provides a clean abstraction layer between `ai-mode`
;; and external AI APIs. This adapter ensures type-safe and consistent access
;; to AI mode's internal typed structures while maintaining backwards
;; compatibility and extensibility.
;;
;; The adapter addresses the need for external packages to interact with
;; ai-mode's data structures without directly accessing internal fields,
;; following proper encapsulation principles and reducing coupling between
;; ai-mode and its consumers.
;;
;; Main features:
;; - Type-safe access to typed struct metadata
;; - Content extraction with automatic rendering of nested structures
;; - Abstraction layer that shields external code from internal changes
;; - Support for complex data structures including lists and nested content
;; - Error handling for malformed or incomplete structures
;;
;; Usage example:
;;
;;   (let ((struct (ai-common--make-typed-struct "Hello world" 'message 'user)))
;;     (message "Type: %s" (ai-mode-adapter--get-struct-type struct))
;;     (message "Content: %s" (ai-mode-adapter--get-struct-content struct)))
;;
;; The adapter is designed to be extended with additional accessor functions
;; as needed by different AI API integrations while maintaining a consistent
;; interface pattern.

;;; Code:

(require 'ai-common)

(defun ai-mode-adapter--get-struct-type (struct)
  "Get the type from a typed structure STRUCT (plist).
This function provides an abstracted way to access the :type field of
AI mode's internal typed structures, as per user request to avoid
direct field access."
  (plist-get struct :type))

(defun ai-mode-adapter--get-struct-content (struct)
  "Get the content from a typed structure STRUCT (plist).
This function provides an abstracted way to access the :content field of
AI mode's internal typed structures, leveraging
`ai-common--render-struct-to-string` for robust content extraction,
including nested structures."
  (ai-common--render-struct-to-string struct))

(provide 'ai-mode-adapter-api)

;;; ai-mode-adapter-api.el ends here
