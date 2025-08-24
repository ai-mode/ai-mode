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
;;     (message "Type: %s" (ai-mode-adapter-api-get-struct-type struct))
;;     (message "Content: %s" (ai-mode-adapter-api-get-struct-content struct)))
;;
;; The adapter is designed to be extended with additional accessor functions
;; as needed by different AI API integrations while maintaining a consistent
;; interface pattern.

;;; Code:

(require 'ai-common)
(require 'ai-network)
(require 'ai-structs)
(require 'cl-lib)

(defun ai-mode-adapter-api-get-struct-type (struct)
  "Get the type from a typed structure STRUCT (plist).
This function provides an abstracted way to access the :type field of
AI mode's internal typed structures, as per user request to avoid
direct field access."
  (plist-get struct :type))

(defun ai-mode-adapter-api-get-struct-content (struct)
  "Get the content from a typed structure STRUCT (plist).
This function provides an abstracted way to access the :content field of
AI mode's internal typed structures, leveraging
`ai-common--render-struct-to-string` for robust content extraction,
including nested structures."
  (ai-common--render-struct-to-string struct))

(defun ai-mode-adapter-api-get-cache-ttl (struct)
  "Get the cache TTL (time-to-live) for typed structure STRUCT.
Returns a string representing the cache duration:
- \"0m\" - no caching needed (highly dynamic content)
- \"5m\" - short-term caching for periodically changing data
- \"1h\" - long-term caching for rarely changing data

The function checks for explicit cache TTL instructions and applies
intelligent heuristics based on content type and characteristics."
  (let ((explicit-ttl (plist-get struct :cache-ttl))
        (cache-policy (plist-get struct :cache-policy)))
    (cond
     ;; Explicit TTL override
     (explicit-ttl explicit-ttl)
     ;; Cache policy override
     ((eq cache-policy 'never) "0m")
     ((eq cache-policy 'short) "5m")
     ((eq cache-policy 'long) "1h")
     ;; Apply intelligent heuristics
     (t (ai-mode-adapter-api-default-cache-ttl-heuristic struct)))))

(defun ai-mode-adapter-api-default-cache-ttl-heuristic (struct)
  "Apply default caching TTL heuristics based on STRUCT content and type.
Returns appropriate cache duration string based on content characteristics."
  (let ((type (ai-mode-adapter-api-get-struct-type struct))
        (subtype (plist-get struct :subtype))
        (content (plist-get struct :content)))
    (cond
     ;; Long-term cache (1h) - static, rarely changing content
     ((eq type 'agent-instructions) "1h")
     ((eq type 'memory) "1h")

     ;; No cache (0m) - dynamic, user-driven content
     ((eq type 'user-input) "0m")
     ((eq type 'action-context) "0m")
     ((eq type 'file-metadata) "0m")

     ;; Short-term cache (5m) - semi-dynamic content
     ((eq type 'session-context) "5m")
     ((eq type 'temporary-data) "5m")

     ((eq type 'project-ai-summary) "1h")
     ((eq type 'project-context) "5m")

     ((eq type 'additional-context) "0m")

     ;; Default cache for unknown types
     (t "0m"))))

(defun ai-mode-adapter-api-ttl-to-seconds (ttl-string)
  "Convert TTL-STRING to seconds.
Supported formats:
- \"0m\" or \"0\" → 0 seconds
- \"5m\" → 300 seconds (5 minutes)
- \"1h\" → 3600 seconds (1 hour)
- \"30s\" → 30 seconds
- \"2d\" → 172800 seconds (2 days)

Returns 0 for invalid or unrecognized formats."
  (cond
   ;; Handle nil or empty string
   ((or (null ttl-string) (string= ttl-string "")) 0)
   ;; Handle plain "0"
   ((string= ttl-string "0") 0)
   ;; Handle time units
   ((string-match "^\\([0-9]+\\)\\([smhd]\\)$" ttl-string)
    (let ((number (string-to-number (match-string 1 ttl-string)))
          (unit (match-string 2 ttl-string)))
      (cond
       ((string= unit "s") number)              ; seconds
       ((string= unit "m") (* number 60))       ; minutes
       ((string= unit "h") (* number 3600))     ; hours
       ((string= unit "d") (* number 86400))    ; days
       (t 0))))
   ;; Fallback for unrecognized format
   (t 0)))

(defun ai-mode-adapter-api-should-cache-content-p (struct)
  "Determine if the content of typed structure STRUCT should be cached.
This is a compatibility function that returns t for any non-zero cache TTL.
For more granular control, use `ai-mode-adapter-api-get-cache-ttl` instead."
  (not (string= (ai-mode-adapter-api-get-cache-ttl struct) "0m")))

(defun ai-mode-adapter-api-prepare-messages (messages)
  "Prepare a list of MESSAGES for AI API consumption.
Currently applies message grouping via `ai-mode-adapter-api-group-adjacent-same-type-structs`
to optimize caching and reduce redundancy. Returns the modified message list.

This is a public interface function that can be extended with additional
message preparation steps as needed."
  (ai-mode-adapter-api-group-adjacent-same-type-structs messages))

(defun ai-mode-adapter-api-group-adjacent-same-type-structs (messages)
  "Group adjacent messages of the same type and group into grouped structures.
Takes a list of MESSAGES (typed structures) and merges consecutive messages
of the same type and same group value into a single grouped structure with child elements.
This enables more efficient caching and rendering of similar content blocks.

The grouping is based on both :type and :group fields. Messages are grouped only if:
1. They have the same :type value
2. They have the same :group value (nil is treated as a valid group value)

The grouped structure will have:
- :type same as the original type
- :content list of child structures
- :grouped t to indicate it's a grouped structure
- Other properties inherited from the first message in the group

Returns a new list with grouped structures replacing adjacent same-type messages."
  (when (null messages)
    (cl-return-from ai-mode-adapter-api-group-adjacent-same-type-structs nil))

  (let ((result '())
        (current-group '())
        (current-type nil)
        (current-group-value nil))

    (dolist (message messages)
      (let ((message-type (ai-mode-adapter-api-get-struct-type message))
            (message-group (plist-get message :group)))
        (cond
         ;; First message or same type and group as current group
         ((or (null current-type)
              (and (eq message-type current-type)
                   (equal message-group current-group-value)))
          (push message current-group)
          (setq current-type message-type)
          (setq current-group-value message-group))

         ;; Different type or group - finalize current group and start new one
         (t
          ;; Finalize current group
          (if (= (length current-group) 1)
              ;; Single message - add as-is
              (push (car current-group) result)
            ;; Multiple messages - create grouped structure
            (let* ((first-message (car (reverse current-group)))
                   (grouped-struct (copy-sequence first-message)))
              ;; Override content and add grouping marker
              (setq grouped-struct (plist-put grouped-struct :content (reverse current-group)))
              (setq grouped-struct (plist-put grouped-struct :grouped t))
              (push grouped-struct result)))

          ;; Start new group
          (setq current-group (list message))
          (setq current-type message-type)
          (setq current-group-value message-group)))))

    ;; Handle the last group
    (when current-group
      (if (= (length current-group) 1)
          ;; Single message - add as-is
          (push (car current-group) result)
        ;; Multiple messages - create grouped structure
        (let* ((first-message (car (reverse current-group)))
               (grouped-struct (copy-sequence first-message)))
          ;; Override content and add grouping marker
          (setq grouped-struct (plist-put grouped-struct :content (reverse current-group)))
          (setq grouped-struct (plist-put grouped-struct :grouped t))
          (push grouped-struct result))))

    (reverse result)))

(defun ai-mode-adapter-api-is-grouped-struct-p (struct)
  "Check if STRUCT is a grouped structure created by grouping function.
Returns t if the structure has the :grouped flag set to t, nil otherwise.
This can be used during rendering to determine if the wrapper container
should be omitted."
  (eq (plist-get struct :grouped) t))

;; Network request aliases
(cl-defun ai-mode-adapter-api-async-request (api-url method body headers callback
                                                     &key (timeout ai-network--default-request-timeout)
                                                     (request-id (ai-common--generate-request-id)))
  "Send an asynchronous HTTP request to API-URL.
This is an alias for `ai-network--async-request` to provide a stable public API.

METHOD specifies the HTTP method.
BODY is the request body.
HEADERS is a list of request headers.
CALLBACK is a function called with the response.
TIMEOUT specifies the request timeout.
REQUEST-ID is an optional unique identifier for the request."
  (ai-network--async-request api-url method body headers callback :timeout timeout :request-id request-id))

(defalias 'ai-mode-adapter-api-request-async 'ai-mode-adapter-api-async-request
  "Alternative alias for `ai-mode-adapter-api-async-request`.")

(defalias 'ai-mode-adapter-api-http-request 'ai-mode-adapter-api-async-request
  "HTTP request alias for `ai-mode-adapter-api-async-request`.")

;; Public API functions for ai-execution-context

(defun ai-mode-adapter-api-get-execution-context-request-id (execution-context)
  "Get request ID from EXECUTION-CONTEXT.
Returns the unique request identifier or nil if context is invalid."
  (ai-structs--get-execution-context-request-id execution-context))

(defun ai-mode-adapter-api-get-execution-context-messages (execution-context)
  "Get messages list from EXECUTION-CONTEXT.
Returns the prepared messages for AI model or nil if context is invalid."
  (ai-structs--get-execution-context-messages execution-context))

(defun ai-mode-adapter-api-get-execution-context-model (execution-context)
  "Get model information from EXECUTION-CONTEXT.
Returns the model configuration or nil if context is invalid."
  (ai-structs--get-execution-context-model execution-context))

(defun ai-mode-adapter-api-get-execution-context-command-config (execution-context)
  "Get command configuration from EXECUTION-CONTEXT.
Returns the command configuration plist or nil if context is invalid."
  (when (ai-execution-context-p execution-context)
    (ai-execution-context-command-config execution-context)))

(defun ai-mode-adapter-api-get-execution-context-buffer-state (execution-context)
  "Get buffer state from EXECUTION-CONTEXT.
Returns the buffer state snapshot or nil if context is invalid."
  (ai-structs--get-execution-context-buffer-state execution-context))

(defun ai-mode-adapter-api-execution-context-p (execution-context)
  "Check if EXECUTION-CONTEXT is a valid execution context.
Returns t if the context is a valid ai-execution-context struct, nil otherwise."
  (ai-execution-context-p execution-context))

(provide 'ai-mode-adapter-api)

;;; ai-mode-adapter-api.el ends here
