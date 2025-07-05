# AI Mode Emacs Lisp Files Overview

*   **`Eask`**: Manages package metadata, dependencies, and source files for `ai-mode` building and installation.
*   **`ai.el`**: Core package file that defines the main `ai` group and provides the foundational namespace for the entire AI mode system.
*   **`ai-chat.el`**: Provides an interactive AI chat interface using `comint-mode`, managing dialogues, session history, and AI response display.
*   **`ai-command-management.el`**: Defines, parses, loads, and dispatches AI commands. Manages instruction file loading, caching, and modifier parsing.
*   **`ai-common.el`**: Contains core AI primitives and general data structures like `typed-struct`. Provides basic operations on these structures, such as creation and update.
*   **`ai-completions.el`**: Implements AI-powered code completion, handling suggestion requests, session management, and applying completions.
*   **`ai-context-management.el`**: Centralizes the collection, structuring, and storage of context for AI requests, including buffer-specific context, region snippets, and assembly of full execution contexts.
*   **`ai-core.el`**: Serves as the central orchestrator and primary location for fundamental AI-mode logic, coordinating calls between different modules.
*   **`ai-debug.el`**: Provides visual tools for debugging and introspecting AI states, including prompt composition, execution context, and model configuration.
*   **`ai-execution.el`**: Manages AI request execution and asynchronous interaction with AI backends, handling success and failure callbacks.
*   **`ai-file-system.el`**: Provides utilities for interacting with the file system, including project root detection, file filtering by ignore patterns, and reading instruction/memory files.
*   **`ai-mode-adapter-api.el`**: Provides a standardized, type-safe API for `ai-mode` to interact with external AI backends, handling message preparation and content extraction.
*   **`ai-mode-indexing.el`**: Manages the project indexing system for AI, encompassing file summary generation, persistence, and versioning.
*   **`ai-mode-line.el`**: Provides mode line utilities and indicators for AI mode, including progress tracking, status displays, and integration with doom-modeline.
*   **`ai-mode.el`**: Defines the `ai-mode` minor mode, manages the overall UI interaction flow, feature integration, and top-level settings.
*   **`ai-model-management.el`**: Responsible for configuring, selecting, and managing available AI models and their providers.
*   **`ai-prompt-management.el`**: Manages the creation, assembly, rendering, and caching of prompts and instructions for AI models.
*   **`ai-response-processors.el`**: Contains functions for processing and handling responses from AI models, including text extraction and formatting.
*   **`ai-structs.el`**: Contains formal definitions of all common data structures (e.g., `typed-struct` and its derived types) used throughout AI-mode.
*   **`ai-utils.el`**: Contains general-purpose utility functions that do not fit into other specific categories, such as logging, HTTP request handling, basic buffer manipulation, and string templating.
*   **`ai-telemetry.el`**: Handles the collection and reporting of telemetry data for usage statistics and performance monitoring.
*   **`ai-network.el`**: Manages network communication and connectivity for AI mode, including HTTP request handling, connection pooling, retry logic, timeout management, and network-related error handling for AI service interactions.
