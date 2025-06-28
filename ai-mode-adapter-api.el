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

(defun ai-mode-adapter--should-cache-content-p (struct)
  "Determine if the content of typed structure STRUCT should be cached.
Returns t if the structure contains a cacheable flag or if the content
is determined to be suitable for caching based on structure properties,
nil otherwise.

This function checks for explicit caching instructions in the structure
and can be extended to include heuristics for determining cacheable content."
  (let ((cacheable (plist-get struct :cacheable))
        (cache-policy (plist-get struct :cache-policy)))
    (cond
     ;; Explicit cacheable flag
     ((eq cacheable t) t)
     ((eq cacheable nil) nil)
     ;; Cache policy override
     ((eq cache-policy 'always) t)
     ((eq cache-policy 'never) nil)
     ;; Default heuristics based on content type and structure
     (t (ai-mode-adapter--default-cache-heuristic struct)))))

(defun ai-mode-adapter--default-cache-heuristic (struct)
  "Apply default caching heuristics to determine if STRUCT content should be cached.
Returns t if content appears suitable for caching based on type and characteristics."
  (let ((type (ai-mode-adapter--get-struct-type struct))
        (subtype (plist-get struct :subtype))
        (content (plist-get struct :content)))
    (cond
     ;; Cache static instruction content
     ((eq type 'agent-instructions) t)
     ;; Cache file metadata and structured data
     ((eq type 'file-metadata) t)
     ;; Cache memory content for reuse
     ((eq type 'memory-content) t)
     ;; Don't cache user input or dynamic content
     ((eq type 'user-input) nil)
     ;; Cache action context for potential reuse
     ((eq type 'action-context) t)
     ;; For additional context, decide based on subtype
     ((eq type 'additional-context)
      (cond
       ((eq subtype 'project-files) t)
       ((eq subtype 'context-pool) nil)
       (t t))) ; Default to cache for additional context
     ;; Default to not cache for unknown types
     (t nil))))

(provide 'ai-mode-adapter-api)

;;; ai-mode-adapter-api.el ends here
