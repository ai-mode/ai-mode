# AI mode for Emacs

[![LICENSE](https://img.shields.io/github/license/ai-mode/ai-mode)](https://github.com/ai-mode/ai-mode/blob/master/LICENSE)

## Table of Contents

- [Overview](#overview)
  - [Key Features](#key-features)
- [Installation](#installation)
  - [Installation via MELPA](#installation-via-melpa)
  - [Installation via GIT](#installation-via-git)
  - [Configuration](#configuration)
- [Supported Backends](#supported-backends)
- [Usage & Commands](#usage--commands)
  - [Code Completion](#code-completion)
  - [Interactive AI Chat](#interactive-ai-chat)
  - [Code Manipulation](#code-manipulation)
  - [System Instruction Management](#system-instruction-management)
  - [Other Commands](#other-commands)
- [Understanding AI Context](#understanding-ai-context)
- [Customization Options](#customization-options)
- [Related Projects](#related-projects)

## Overview

AI mode for Emacs is a comprehensive package that integrates powerful artificial intelligence capabilities directly into your Emacs workflow. This package transforms Emacs into an AI-enhanced development environment, providing intelligent assistance for coding, writing, and general text manipulation tasks.

### Key Features

-   **Interactive AI Chat**: Directly engage with AI models for real-time problem-solving, code review, and general assistance within Emacs.
-   **Intelligent Code Completion**: Receive context-aware code suggestions and completions, powered by advanced AI models, to significantly enhance coding efficiency.
-   **Code Modification & Refactoring**: Streamline development by leveraging AI to intelligently modify, improve, and refactor existing code.
-   **Code Analysis & Documentation**: Generate comprehensive explanations, documentation, and insightful analysis for code blocks and functions, enhancing readability and maintainability.
-   **Multi-Backend Support**: Seamlessly integrate with various AI providers, including OpenAI, Anthropic, Hugging Face, and Google Generative AI, for diverse model access.
-   **Customizable Commands**: Tailor AI operations to your specific development needs by defining custom commands and integrating them seamlessly into your Emacs setup.

Whether you're debugging complex code, exploring new technologies, or enhancing your documentation, AI mode provides the tools to accelerate your productivity and enhance your Emacs experience.

![ai-mode-completion](https://github.com/ai-mode/ai-mode/blob/master/media/ai-mode-completion.gif "ai-mode-completion")

## Installation

### Installation via MELPA

The easiest and recommended way to install this package is through MELPA. To configure MELPA, add the following lines to your `~/.emacs` file:

```elisp
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Then, run the command:

```elisp
M-x package-install RET ai-mode RET
```

You might want to refresh the package contents if you haven't done so recently by running:

```elisp
M-x package-refresh-contents RET
```

### Installation via GIT

Clone the repository into your Emacs plugins directory:

```bash
$ cd ~/.emacs.d/plugins
$ git clone --recursive https://github.com/ai-mode/ai-mode
```

Then, add the following to your `.emacs` configuration file:

```elisp
(add-to-list 'load-path "~/.emacs.d/plugins/ai-mode")
(require 'ai-mode)
```

### Configuration

AI Mode's functionality is highly customizable. While this package provides the core framework, integration with specific AI models (like OpenAI, Anthropic, etc.) is handled by separate backend packages.

To enable a specific backend, you typically load its package and then call its setup function. This function usually registers the backend with `ai-mode`, making its models available. Refer to the documentation of each backend package for specific instructions on setting API keys and other model-specific parameters.

Here's an example of basic `ai-mode` configuration within `use-package`, including how to enable a backend like `ai-mode-google-genai`:

```elisp
(use-package ai-mode
  :config (progn
             (global-ai-mode)
             ;; Set a global keybinding for code completion
             (global-set-key (kbd "C-<tab>") 'ai-complete-code-at-point)
             ))

;; Example of enabling a specific backend (e.g., ai-mode-google-genai)
;; Ensure you have installed the ai-mode-google-genai package first.
(use-package ai-mode-google-genai
  :after ai-mode
  :config
  (ai-mode-google-genai-setup)) ; This function registers the Google GenAI models
```

## Supported Backends

AI mode integrates with a variety of AI providers through dedicated backend packages. To utilize a specific backend, ensure you have installed its corresponding package and configured any necessary API keys.

*   **[AI Mode OpenAI](https://github.com/ai-mode/ai-mode-openai)**: OpenAI GPT backend for `ai-mode`.
*   **[AI Mode Anthropic](https://github.com/ai-mode/ai-mode-anthropic)**: Anthropic Claude backend for `ai-mode`.
*   **[AI Mode DeepSeek](https://github.com/ai-mode/ai-mode-deepseek)**: DeepSeek backend for `ai-mode`.
*   **[AI Mode Hugging Face](https://github.com/ai-mode/ai-mode-hf)**: Hugging Face models backend for `ai-mode`.
*   **[AI Mode Google Generative AI](https://github.com/ai-mode/ai-mode-google-genai)**: Google Generative AI backend for `ai-mode`.

## Usage & Commands

AI Mode provides a rich set of interactive commands, many of which are accessible through the `ai-command-map` prefix (default: `C-c i`).

### Code Completion

AI Mode offers intelligent code completion at the cursor position. You can use the primary entry point `ai-complete-code-at-point` to initiate a completion session, which can be further refined with context-specific commands.

| Command | Description |
|---|---|
| `ai-complete-code-at-point` | Start code completion at the cursor position. This is the primary entry point for code completion. |
| `ai-completions--complete-at-point-with-limited-context` | Perform completion with a limited context around the cursor. |
| `ai-completions--complete-at-point-with-full-context` | Perform completion using the entire file content as context. |

When a completion session is active, the following interactive commands are available to navigate and manage candidates:

| Binding | Command | Description |
|---|---|---|
| `\e\e\e` | `ai-completions--abort` | Abort the current completion session. |
| `\C-g` | `ai-completions--abort` | Abort the current completion session. |
| `C-<tab>` | `ai-completions--select-next-or-abort` | Show the next candidate completion. If no more candidates, request new ones from the AI. |
| `C-n` | `ai-completions--select-next-or-abort` | Show the next candidate completion. If no more candidates, request new ones from the AI. |
| `<down>` | `ai-completions--select-next-or-abort` | Show the next candidate completion. If no more candidates, request new ones from the AI. |
| `C-p` | `ai-completions--select-prev-or-abort` | Show the previous candidate completion. |
| `<up>` | `ai-completions--select-prev-or-abort` | Show the previous candidate completion. |
| `[return]` | `ai-completions--select-current` | Select and insert the currently displayed candidate completion. |
| `RET` | `ai-completions--select-current` | Select and insert the currently displayed candidate completion. |
| `C-i` | `ai-completions--increase-current-context` | Increase the size of the code context sent to the AI for more relevant suggestions. |
| `C-f` | `ai-completions--maximize-current-context` | Maximize the code context (entire file) sent to the AI for completion. |
| `C-a` | `ai-completions--add-instruction` | Add a temporary, user-provided instruction to the current AI query. |

### Interactive AI Chat

The `ai-chat` package provides an interactive chat interface with AI models, built on Emacs's `comint-mode` for a familiar and extensible experience. It supports asynchronous interaction with various AI backends, allowing you to ask questions, review code, or get general assistance in real-time.

**How to Use:**

1.  **Start a chat:** Invoke `M-x ai-chat` or use the default keybinding `C-c c c`.
2.  **Send messages:** Type your message and press `C-return` to send it to the AI.

**Customization:**

`ai-chat` offers several customizable variables to tailor the chat experience:

*   `ai-chat--prompt`: Customize the prompt string displayed in the chat buffer (default: `AI> `).
*   `ai-chat--buffer-context-size`: Control the number of historical messages included as context for AI responses (default: `5`).
*   `ai-chat--change-backend-prompt`: Modify the prompt used when selecting a different AI backend.

**Keybindings:**

*   `C-return`: Send the current input to the AI.
*   `C-c C-e`: Clear the chat buffer and its history.
*   `C-c +`: Increase the number of historical messages used for context.
*   `C-c -`: Decrease the number of historical messages used for context.
*   `C-c C-b`: Change the active AI backend/model for the chat session.
*   `C-c C-c`: Interrupt the current AI request.

![ai-chat](https://github.com/ai-mode/ai-mode/blob/master/media/ai-chat.gif "ai-chat")

### Code Manipulation

![ai-mode-replace-number](https://github.com/ai-mode/ai-mode/blob/master/media/ai-mode-replace-number.gif "ai-mode-replace-number")
![ai-mode-translation](https://github.com/ai-mode/ai-mode/blob/master/media/ai-mode-translation.gif "ai-mode-translation")
![ai-mode-translation-2](https://github.com/ai-mode/ai-mode/blob/master/media/ai-mode-translation-2.gif "ai-mode-translation-2")

These commands apply AI operations to selected regions or the entire buffer, modifying, improving, or generating code. When invoked, you might be prompted to select a specific *query type* (e.g., "modify", "generate code", "fix", "improve", "optimize", "doc", "explain", "simplify", "spellcheck", "elaborate") from `ai--query-type-config-map`.

| Command | Description |
|---|---|
| `ai-perform` | Apply the chosen AI operation (based on query type) to the selected content or buffer, replacing it with the AI's response. |
| `ai-perform-coordinator` | Decides whether to start a new "replace" operation based on a chosen query type or continue an existing one (e.g., for iterative modification). |
| `ai-optimize-code-region` | Optimize the selected code region and replace it with its optimized version. |
| `ai-explain-code-region` | Explain the selected code region and display the explanation in a dedicated help buffer. |
| `ai-show` | Execute the chosen AI operation and display the AI's response in a special read-only buffer (e.g., for review without immediate insertion). |

### System Instruction Management

These commands manage global and buffer-local system instructions that guide the AI's overall behavior.

| Command | Description |
|---|---|
| `ai-completions--add-system-instructions` | Add *buffer-local* system instructions for code completion. |
| `ai-completions--clear-system-instructions` | Clear all *buffer-local* system instructions for code completion. |

### Other Commands

| Command | Description |
|---|---|
| `ai-chat` | Start an interactive chat session with the AI. |
| `ai--change-execution-backend` | Interactively select and switch the active AI model/backend for current requests. |
| `ai--switch-file-instructions-enabled` | Toggle the loading of project-specific AI instruction files (from `.ai/` directories). |
| `ai-debug` | Display detailed debugging information about the current region and AI execution context. |

## Understanding AI Context

When you interact with AI Mode, a comprehensive context is assembled and sent to the AI model. This context ensures the AI has all the necessary information to provide relevant and accurate responses. The context is built from various sources:

*   **Code-Related Context**:
    *   **Active Region/File Content**: If a region is selected, its content is sent. Otherwise, the entire buffer content may be included.
    *   **Preceding and Following Context**: Lines of code immediately before and after the cursor/region are included to provide surrounding code awareness.
    *   **File Metadata**: Information such as the current file's path, buffer name, and programming language.
*   **Interaction-Related Context**:
    *   **Model Information**: Details about the selected AI model (e.g., name, provider).
    *   **User Input**: Explicit prompts or questions you provide.
    *   **Additional Context Pool**: Content manually added to the temporary context pool by you (e.g., using `ai-common--add-to-context-pool`).
*   **Instructional Context**:
    *   **Global System Instructions**: General instructions that apply to all AI interactions (managed by `ai-common--global-system-prompts`).
    *   **Global Memory**: Persistent information stored in global memory for all sessions (managed by `ai--global-memo-context`).
    *   **Buffer-Bound Prompts**: Instructions specifically tied to the current buffer (managed by `ai-common--buffer-bound-prompts`).
    *   **Action-Specific Instructions**: Prompts and examples loaded from `.ai/` files for specific actions (e.g., "explain", "modify", "complete").
    *   **Action Object Rules**: Instructions on how to interpret and generate structured action objects (`<modify-action-object>`, `<complete-action-object>`, etc.).

This layered approach ensures that AI has a deep understanding of your code, environment, and intentions for each interaction.

## Customization Options

AI Mode provides several important customizable variables to tailor its behavior to your workflow. These can be adjusted using `M-x customize-group RET ai RET` or by setting them directly in your Emacs configuration file (e.g., `init.el`).

| Variable | Description | Default |
|---|---|---|
| `ai-completions--context-size` | Number of lines used as preceding context for code completion when no region is active. | `20` |
| `ai-completions--forwarding-current-context-size` | Number of lines used as following context for code completion when no region is active. | `20` |
| `ai--project-file-instructions-enabled` | If non-nil, enables loading of action instructions from project-specific `.ai/` directories. | `t` |
| `ai--extended-instructions-enabled` | If non-nil, enables loading of extended instructions for AI prompts (e.g., basic, action-specific, examples). | `t` |
| `ai--global-prompts-enabled` | If non-nil, enables the use of global system prompts loaded from the `ai-mode` library directory. | `t` |
| `ai--current-buffer-additional-context` | If non-nil, includes the current buffer's entire content as additional context, especially when a region is active. This can be useful for whole-file understanding, but can increase token usage. | `t` |
| `ai-utils--write-log-buffer` | If non-nil, enables logging of AI requests and responses to `*AI-request-log*` buffer. | `nil` |
| `ai-utils--verbose-log` | If non-nil, enables verbose logging for AI utilities. | `nil` |
| `ai-utils--default-request-timeout` | Default timeout for HTTP requests in seconds. | `60` |
| `ai-keymap-prefix` | The prefix keybinding for AI commands (e.g., `C-c i`). | `"C-c i"` |
| `ai--query-type-config-map` | An association list mapping query types (e.g., "modify", "explain") to their configurations, including options like user input and instructions. This defines the types of AI operations available via `ai-perform` and `ai-show`. | (See definition in `ai-mode.el`) |

## Related Projects

- [Org Babel support](https://github.com/ai-mode/ob-ai)
