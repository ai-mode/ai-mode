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
  - [Buffer-Bound Chat Sessions](#buffer-bound-chat-sessions)
  - [Code Manipulation](#code-manipulation)
  - [System & Session Context Management](#system--session-context-management)
  - [Execution Control](#execution-control)
  - [Extending AI Mode with Custom Commands](#extending-ai-mode-with-custom-commands)
  - [Other Commands](#other-commands)
- [Debugging & Introspection](#debugging--introspection)
- [Understanding AI Context](#understanding-ai-context)
- [Customization Options](#customization-options)
- [File Structure Overview](#file-structure-overview)
- [Related Projects](#related-projects)
- [Legal Notice](#legal-notice)

## Overview

AI mode for Emacs is a comprehensive package that integrates powerful artificial intelligence capabilities directly into your Emacs workflow. This package transforms Emacs into an AI-enhanced development environment, providing intelligent assistance for coding, writing, and general text manipulation tasks. The modular architecture ensures maintainability, testability, and extensibility.

![ai-mode-overview](https://github.com/ai-mode/media/blob/master/01-generate-from-selection.gif "ai-mode-overview")

### Key Features

-   **Interactive AI Chat with Buffer Binding**: Directly engage with AI models for real-time problem-solving, code review, and general assistance. Create dedicated chat sessions bound to specific buffers with rich context integration and smart startup strategies.
-   **Intelligent Code Completion**: Receive interactive, multi-candidate code suggestions and completions. It supports real-time previews, navigation through candidates, dynamic context adjustment, and integration of user instructions.
-   **Code Modification & Refactoring**: Streamline development by leveraging AI to intelligently modify, improve, and refactor existing code.
-   **Code Analysis & Documentation**: Generate comprehensive explanations, documentation, and insightful analysis for code blocks and functions, enhancing readability and maintainability.
-   **Automated Bug Detection & Fixing**: Utilize AI to detect common issues and suggest fixes directly within your buffer.
-   **Multi-Backend Support**: Seamlessly integrate with various AI providers, including OpenAI, Anthropic, Hugging Face, and Google Generative AI, for diverse model access.
-   **Customizable Commands**: Tailor AI operations to your specific development needs by defining custom commands and integrating them seamlessly into your Emacs setup.
-   **Advanced Context Management**: Manage global, buffer-local, and temporary context pools, with extended context capabilities including project-wide indexing, memory files, and comprehensive context enrichment.
-   **Real-time Previews & Progress Indicators**: Get instant visual feedback for completions and track the progress of ongoing AI requests.
-   **Structured Request Auditing**: Detailed logging of AI requests, contexts, and responses for debugging, performance analysis, and security auditing.

Whether you're debugging complex code, exploring new technologies, or enhancing your documentation, AI mode provides the tools to accelerate your productivity and enhance your Emacs experience.

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
M-x package-install RET ai-mode-openai RET # If you plan to use OpenAI API
M-x package-install RET ai-mode-anthropic RET # If you plan to use Anthropic
M-x package-install RET ai-mode-google-genai RET  # If you plan to use Google Generative AI
```

You might want to refresh the package contents if you haven't done so recently by running:

```elisp
M-x package-refresh-contents RET
```

### Installation via GIT

If you prefer to manage packages manually or use a different package manager, clone the `ai-mode` repository and its related backend repositories into a common directory, for example, `~/.emacs.d/plugins/`:

```bash
$ mkdir -p ~/.emacs.d/plugins
$ cd ~/.emacs.d/plugins
$ git clone https://github.com/ai-mode/ai-mode
$ git clone https://github.com/ai-mode/ai-mode-openai # If you plan to use OpenAI API
$ git clone https://github.com/ai-mode/ai-mode-anthropic # If you plan to use Anthropic
$ git clone https://github.com/ai-mode/ai-mode-deepseek # If you plan to use DeepSeek
$ git clone https://github.com/ai-mode/ai-mode-hf # If you plan to use Hugging Face
$ git clone https://github.com/ai-mode/ai-mode-google-genai # If you plan to use Google Generative AI
$ git clone https://github.com/ai-mode/ob-ai # If you plan to use Org Babel integration
```

Then, add the `~/.emacs.d/plugins/` directory to your Emacs `load-path` in your `~/.emacs` or `init.el` file. For `use-package`, you can configure it by adding the following to your `init.el` (or `.emacs`):

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))

;; You can then use use-package as usual for ai-mode and its backends,
;; as they will be discoverable on your load-path.
```

### Configuration

AI Mode's functionality is highly customizable. While this package provides the core framework, integration with specific AI models (like OpenAI, Anthropic, etc.) is handled by separate backend packages.

To enable a specific backend, you typically load its package and then register its model-providing function with `ai-mode`. This makes its models available for use in `ai-mode` and `ai-chat`. Remember to configure any necessary API keys as per the backend's documentation.

Here's an example of a comprehensive `use-package` configuration for `ai-mode`, including how to enable various backends and the `ai-debug` package:

```elisp
(use-package ai-mode
  :defer t
  :config (progn
            (global-ai-mode)
            ;; Set global keybindings for code completion
            (global-set-key (kbd "C-<tab>") 'ai-completions-complete-code-at-point-with-limited-context)
            (global-set-key (kbd "C-M-<tab>") 'ai-completions-complete-at-point-with-full-context)
            ;; Enable completions mode for programming buffers
            (add-hook 'prog-mode-hook #'ai-completions-mode)
            ))

;; Example of enabling specific backends
;; Ensure you have installed the respective packages first (e.g., ai-mode-openai, ai-mode-google-genai).

;; OpenAI Backend
(use-package ai-mode-openai
  :after (ai-mode ai-model-management)
  :config (progn
            (add-to-list 'ai-model-management-providers 'ai-mode-openai--get-models)
            ;; Set your OpenAI API key:
            ;; (setq ai-mode-openai--api-key "YOUR_OPENAI_API_KEY")
            ))

;; Google Generative AI Backend
(use-package ai-mode-google-genai
  :after (ai-mode ai-model-management)
  :config (progn
            (add-to-list 'ai-model-management-providers 'ai-mode-google-genai--get-models)
            ;; Set your Google Generative AI API key:
            ;; (setq ai-mode-google-genai--api-key "YOUR_GOOGLE_GENAI_API_KEY")
            ))

;; Anthropic Backend
(use-package ai-mode-anthropic
  :load-path "~/.emacs.d/plugins/ai-mode-anthropic" ;; Example for local install
  :after (ai-mode ai-model-management)
  :config (progn
            (add-to-list 'ai-model-management-providers 'ai-mode-anthropic--get-models)
            ;; Set your Anthropic API key:
            ;; (setq ai-mode-anthropic--api-key "YOUR_ANTHROPIC_API_KEY")
            ))

;; Other backends (DeepSeek, Hugging Face) can be configured similarly.

;; AI Debug Package
(use-package ai-debug
  :after (ai-mode)
  :config (progn
            ;; Configure debug features, e.g., disable content truncation for full view
            (setq ai-debug-truncate-content nil)
            (setq ai-debug-max-content-length 2000)
            ))
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

AI Mode offers intelligent code completion at the cursor position. You can use the primary entry point `ai-completions-complete-code-at-point` to initiate a completion session, which can be further refined with context-specific commands.

![code-completion](https://github.com/ai-mode/media/blob/master/02-code-completion.gif "code-completion")

| Command | Description |
|---|---|
| `ai-completions-complete-code-at-point` | Start code completion at the cursor position. This is the primary entry point for code completion. |
| `ai-completions-complete-at-point-with-limited-context` | Perform completion with a limited context around the cursor. |
| `ai-completions-complete-at-point-with-full-context` | Perform completion using the entire file content as context. |

When a completion session is active, the following interactive commands are available to navigate and manage candidates:

![code-completion-with-navigation](https://github.com/ai-mode/media/blob/master/03-code-completion-with-navigation.gif "code-completion-with-navigation")

| Binding | Command | Description |
|---|---|---|
| `\e\e\e` | `ai-completions--abort` | Abort the current completion session. |
| `\C-g` | `ai-completions--abort` | Abort the current completion session. |
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

The `ai-chat` package provides a full-featured interactive chat interface with AI models, built on Emacs's `comint-mode` for a familiar and extensible experience. It supports asynchronous interaction with various AI backends, allowing you to ask questions, review code, or get general assistance in real-time.

![ai-chat](https://github.com/ai-mode/media/blob/master/09-ai-mode-chat.gif "ai-chat")

**How to Use:**

1.  **Start a chat:** Invoke `M-x ai-chat` or use the default keybinding `C-c i c c`. This opens a dedicated `*ai-chat*` buffer.
2.  **Send messages:** Type your message at the `AI>` prompt and press `C-return` to send it to the AI.
3.  **Manage Sessions:** Chat sessions can be automatically or manually saved and loaded for continuity.
4.  **Progress Indicators:** Visual indicators are displayed in the mode line to show when an AI request is in progress.

**Customization:**

`ai-chat` offers several customizable variables to tailor the chat experience. These are listed in the [Customization Options](#customization-options) section.

**Keybindings (within the `*ai-chat*` buffer):**

| Binding | Command | Description |
|---|---|---|
| `C-return` | `ai-chat-return` | Send the current input to the AI. |
| `RET` | `ai-chat-insert-new-line` | Insert a new line in the input area without sending the message. |
| `C-c C-e` | `ai-chat-clear-buffer` | Clear the chat buffer and start a new session. |
| `C-c +` | `ai-chat-increase-context-size` | Increase the number of historical messages used for context. |
| `C-c -` | `ai-chat-decrease-context-size` | Decrease the number of historical messages used for context. |
| `C-c C-b` | `ai-chat-change-backend` | Change the active AI backend/model for the chat session. |
| `C-c C-s` | `ai-chat-save-session` | Manually save the current chat session to a file. |
| `C-c C-l` | `ai-chat-load-session` | Load a previously saved chat session from history files. |
| `C-c C-a` | `ai-chat-toggle-auto-save` | Toggle automatic saving of chat sessions. |
| `C-c C-c` | `ai-chat-interrupt` | Interrupt the current AI request. |

### Buffer-Bound Chat Sessions

AI Mode supports advanced buffer-bound chat sessions that create dedicated chat buffers tied to specific source files. This feature enables context-aware conversations with rich startup context and seamless integration between your code and AI discussions.

**Key Features:**

- **Per-Buffer Chat Sessions**: Each buffer can have one or more dedicated chat sessions
- **Multiple Session Support**: Create multiple parallel chat sessions for a single source buffer
- **Rich Startup Context**: Configurable context strategies including buffer content, regions, extended context, and user prompts
- **Context Enrichment**: Send buffer content or selections to bound chats as additional context
- **Persistent Sessions**: Sessions are tracked and can be resumed later

**Buffer-Bound Chat Commands:**

| Command | Keybinding | Description |
|---|---|---|
| `ai-chat-for-buffer` | `C-c i c b` | Open or create a chat bound to the current buffer. With prefix argument, always create new session. |
| `ai-chat-send-selection-to-chat` | `C-c i c s` | Send the active region (or prompt for entire buffer) to the bound chat as context. |
| `ai-chat-send-file-to-chat` | `C-c i c f` | Send the entire current buffer to the bound chat as context. |
| `ai-chat-choose-session` | `C-c i c o` | Choose and open one of the existing chat sessions bound to the current buffer. |

**Startup Context Strategies:**

The `ai-chat-startup-context-strategy` variable controls what context is automatically added when creating new buffer-bound chat sessions:

- `none` - No automatic context
- `ask` - Prompt user to choose strategy (default)
- `buffer` - Add entire buffer content
- `region` - Add selected region (fallback to buffer if no selection)
- `user-prompt` - Add user-provided instructions
- `buffer+prompt` - Combine buffer content with user prompt
- `extended` - Add comprehensive context (context pool, memory, prompts, project context)
- `extended+buffer` - Extended context plus buffer content
- `extended+region` - Extended context plus region
- `extended+prompt` - Extended context plus user prompt
- `full-context` - All available context (buffer + extended + user prompt)

**Session Management:**

Buffer-bound sessions are automatically tracked and can be:
- Reused across Emacs sessions
- Managed independently per source buffer
- Configured for different default behaviors (create new, reuse existing, or ask user)

### Code Manipulation

These commands apply AI operations to selected regions or the entire buffer, modifying, improving, or generating code. When invoked, you might be prompted to select a specific *command* (e.g., "modify", "generate code", "fix", "improve", "optimize", "doc", "explain", "simplify", "spellcheck", "elaborate") from `ai-command-management-commands-config-map`.

![code-generation-from-user-input](https://github.com/ai-mode/media/blob/master/04-generate-code-from-user-input.gif "code-generation-from-user-input")

![code-modification](https://github.com/ai-mode/media/blob/master/05-code-modification.gif "code-modification")

![code-refactoring](https://github.com/ai-mode/media/blob/master/06-code-refactoring.gif "code-refactoring")

![advanced-code-generation](https://github.com/ai-mode/media/blob/master/07-generate-code-from-user-input.gif "advanced-code-generation")

![code-explanation](https://github.com/ai-mode/media/blob/master/08-explain-code.gif "code-explanation")

| Command | Description |
|---|---|
| `ai-perform` | Apply the chosen AI operation (based on command type) to the selected content or buffer. The result action (replace, show, eval, insert-at-point) is determined by the command's configuration. |
| `ai-perform-coordinator` | Decides whether to start a new "replace" operation based on a chosen command type or continue an existing one (e.g., for iterative modification or code generation in place). |
| `ai-show` | Execute the chosen AI operation and display the AI's response in a special read-only buffer (e.g., `*AI response*`). Filters for command types configured for `show` result action. |
| `ai-execute` | Execute the chosen AI operation and display the AI's response in a buffer, then ask for user permission to evaluate the generated Emacs Lisp code. Filters for command types configured for `eval` result action. |
| `ai-core-open-extended-chat` | Open chat for current buffer with comprehensive extended context including project files, memory, prompts, and context pool items. |

### System & Session Context Management

These commands manage global and buffer-local system instructions and temporary context that guide the AI's overall behavior during interactions.

| Command | Description |
|---|---|
| `ai-context-add-to-pool` (C-c i k a) | Manually add a data item (e.g., snippet, user input, file context) to the temporary context pool for the current interaction. |
| `ai-context-add-region` (C-c i k r) | Creates a snippet from the active region and adds it to the temporary context pool. |
| `ai-context-add-file` (C-c i k f) | Adds the entire content of the current file to the temporary context pool. |
| `ai-context-clear-pool` (C-c i k c) | Clear the temporary context pool for the current interaction. |
| `ai-context-remove-from-pool` (C-c i k x) | Remove selected item from context pool via minibuffer selection. |
| `ai-context-switch-project-mode` (C-c i k s) | Interactively switch the project context inclusion mode (disabled, full files, or summary index). |
| `ai-context-add-buffer-prompts` (C-c i k b) | Add content (from region or user input) as buffer-specific instructions, affecting only the current buffer. |
| `ai-context-clear-buffer-prompts` (C-c i k B) | Clear all buffer-specific instructions for the current buffer. |
| `ai-context-add-to-memory` (C-c i k m) | Add content (from region or user input) to the global memory context, accessible across all buffers. |
| `ai-context-clear-memory` (C-c i k M) | Clear all entries from the global memory context. |
| `ai-mode-indexing-update-project-files-summary-index` (C-c i i u) | Generate and update the AI-powered summary index for all project files. |
| `ai-mode-indexing-switch-indexing-strategy` (C-c i i s) | Interactively switch the project indexing strategy (parallel-independent or sequential). |
| `ai-mode-indexing-select-index-version` (C-c i i v) | Interactively select and load a specific historical project index version. |
| `ai-mode-indexing-list-index-versions` (C-c i i l) | List all available project index versions for the current project. |
| `ai-mode-indexing-delete-old-index-versions` (C-c i i d) | Delete old project index versions beyond the configured retention depth. |
| `ai-mode-indexing-toggle-indexing-context` (C-c i i t) | Toggle inclusion of existing context when indexing new files. |
| `ai-mode-indexing-reindex-project-with-context` (C-c i i R) | Reindex the entire project with existing context enabled. |
| `ai-completions--add-instruction` (C-a during completion) | Add a temporary, user-provided instruction to the current AI completion query. |

### Execution Control

These commands provide fine-grained control over how AI requests are executed and how their results are handled.

| Command | Keybinding (default) | Description |
|---|---|---|
| `ai-execution-toggle-caching` | `C-c i e c` | Toggle prompt caching for AI requests. When enabled, AI providers that support it may cache prompts to reduce token usage and improve response times for repetitive queries. |
| `ai-execution-toggle-patch-mode` | `C-c i e p` | Toggle unified patch mode for `replace` actions. When enabled, AI models generate a standard `diff -u` patch, which is then applied to the buffer. When disabled, the AI's response directly replaces the selected content. |
| `ai-request-audit-toggle` (M-x) | `M-x ai-request-audit-toggle` | Toggle the comprehensive request auditing system on/off. When enabled, detailed request/response data is saved to disk for debugging and review. |
| `ai-request-audit-cleanup-project` (M-x) | `M-x ai-request-audit-cleanup-project` | Delete all audit records for the current project. |
| `ai-telemetry-toggle` (M-x) | `M-x ai-telemetry-toggle` | Toggle general telemetry collection (usage statistics, performance metrics) on/off. |
| `ai-telemetry-toggle-log-buffer` (M-x) | `M-x ai-telemetry-toggle-log-buffer` | Toggle logging of AI requests and responses to the `*AI-request-log*` buffer. |
| `ai-telemetry-toggle-prompt-buffer` (M-x) | `M-x ai-telemetry-toggle-prompt-buffer` | Toggle writing the full prompt sent to the AI to the `*AI prompt*` buffer (useful for debugging prompt composition). |
| `ai-usage-toggle` (M-x) | `M-x ai-usage-toggle` | Toggle token usage statistics collection on/off. |

### Extending AI Mode with Custom Commands

AI Mode is highly extensible, allowing you to define your own custom AI commands and integrate them seamlessly into your workflow. This is primarily done by creating instruction files in your `.ai/commands` directory within your project or home directory.

The `ai-command-management-commands-config-map` is an association list that maps a descriptive command name (string) to a property list (`plist`) defining its behavior. Each `plist` can contain the following keys:

-   `:template` (string, optional): A template string for the query. Variables in the template are denoted by `{{:key}}`, corresponding to keywords in the assembled context.
-   `:instructions` (string, optional): Specific instructions for the AI for this command type. These are general guidelines for the AI's behavior when this command is invoked.
-   `:user-input` (boolean, optional): `t` if the command requires additional user input. If `t`, `ai-mode` will prompt the user for text.
-   `:action-type` (string, optional): The high-level action type (e.g., "modify", "complete", "explain") used for structuring the request to the AI. This influences the XML-like container generated for the AI.
-   `:result-action` (symbol): **Crucial for defining how the AI's response is handled.**
    -   `show`: Display the response in a dedicated read-only buffer (e.g., `*AI response*`).
    -   `replace`: Replace the active region (or the entire buffer if no region is active) with the AI's response.
    -   `eval`: Display the response in a buffer and prompt the user to evaluate it as Emacs Lisp code.
    -   `insert-at-point`: Insert the AI's response at the current cursor position.
-   `:needs-buffer-context` (boolean, optional): `t` if the full buffer content is required for context, even if only a region is active. This ensures the AI has a broader understanding.

**File-based Command Modifiers**:
Commands defined in instruction files can use modifiers in their names
to control behavior without code changes. Modifiers are separated by '__':

Examples:
- `user__explain_code__show.md` - requires user input, shows result
- `buffer__refactor_function__replace.md` - uses full buffer context, replaces selection
- `large__smart_completion__complete.md` - uses large context for completion
- `project__analyze_architecture__show.md` - includes project context

Available modifiers:

Action Modifiers (control how results are handled):
- `show` [S] - Display in a buffer
- `eval` [E] - Display and offer to evaluate
- `replace` [R] - Replace selection/buffer
- `insert` [I] - Insert at cursor position
- `complete` [C] - Code completion

Behavior Modifiers (control input and context requirements):
- `user` [U] - Requires user input
- `buffer` [B] - Uses full buffer context
- `project` [P] - Uses project context
- `global` [G] - Uses global context

Context Size Modifiers (control amount of surrounding context):
- `small` [s] - Limited context (5/5 lines)
- `large` [L] - Extended context (20/20 lines)
- `full` [F] - Full context (unlimited)

Display Indicators:
Commands in completion lists show indicators in brackets:
- `[C]` - Configured command (from `ai-command-management-commands-config-map`)
- `[D]` - Has default instructions (from package)
- `[G]` - Has global instructions (from ~/.ai/)
- `[L]` - Has local instructions (from project .ai/)
- `[UBPSLEGRCIF]` - Active modifiers (combinations possible)

Example display: "[C][DG] explain" or "[UBS] user__buffer__explain_code__show"

**Example: Adding a Custom "Translate" Command**

Let's say you want to add a command to translate a selected text to a specific language.

```elisp
(add-to-list 'ai-command-management-commands-config-map
             '("translate to French" . (:instructions "Translate the given text into formal French."
                                        :user-input t ; Optional: if you want user to provide the text or additional context
                                        :result-action replace ; Replace selected text with translation
                                        :action-type "modify"))) ; AI should modify the input text
```

After adding this to your Emacs configuration and restarting or evaluating the Lisp, `ai-perform` (or other general commands) will offer "translate to French" as an option. When selected, the AI will receive instructions to translate the content (either from the active region or user input) and the result will replace the original text.

You can also create a dedicated instruction file for this command in your `.ai/commands` directory (e.g., `~/.emacs.d/.ai/commands/translate_to_French.md`) to provide more detailed, multi-line instructions to the model.

| Command | Description |
|---|---|
| `ai-command-management-edit-command-instructions` (C-c i m e) | Interactively edit instruction files for commands. Prompts to select an existing command or enter a new one, then selects a location (global or local project) to edit/create the instruction file. |
| `ai-command-management-describe-command-modifiers` (C-c i m d) | Interactively describe the modifiers applied to a selected file-based command, explaining their effect on command behavior and context. |
| `ai-command-management-create-modified-command` (C-c i m c) | Interactively create a new file-based command name by composing a base name with various action, behavior, and context modifiers. Offers to create an instruction file. |

### Other Commands

| Command | Keybinding (default) | Description |
|---|---|---|
| `ai-chat` | `C-c i c c` | Start an interactive chat session with the AI. |
| `ai-change-model` | `C-c i c m` | Interactively select and switch the active AI model/backend for current requests. |
| `ai-core-show-audit` | `M-x ai-core-show-audit` | Show a list of all historical AI requests for the current project, including their status, command, and execution time. |

## Debugging & Introspection

AI Mode provides a suite of visual debugging and introspection tools to help you understand the context being sent to AI models, troubleshoot issues, and gain deeper insights into the extension's behavior. These tools are particularly useful for debugging prompts, understanding context construction, and verifying data flow.

### Debugging Commands

These commands, primarily accessed via the `C-c i d` (for `ai-debug-visual`) and `C-c i D` (for `ai-debug-show-sources`) prefixes, open dedicated buffers that use `magit-section-mode` for an interactive, collapsible display of information.

| Command | Keybinding (default) | Description |
|---|---|---|
| `ai-debug-visual` | `C-c i d` | Displays a comprehensive debug view of the *current AI execution context*, including model configuration, buffer context, completion parameters, and the messages being sent to the AI. This is the main entry point for debugging active AI operations. |
| `ai-debug-show-sources` | `C-c i D` | Displays all *available AI context sources*, such as global system prompts, global memory, buffer-bound prompts, and the temporary context pool. Useful for understanding how different parts of the context are assembled. |
| `ai-debug-show-file-commands` | `C-c i f` | Displays all *loaded file-based commands* from default, global, and local sources, including their modifiers and content. |
| `ai-debug-show-system-prompts` | `C-c i s` | Displays all *loaded system prompts* from default, global, and local sources, with their content. |
| `ai-debug-show-raw-structures-interactive` | `C-c i R` | Displays all *typed structures in raw sequential format* as they are prepared for the AI model. Useful for understanding the exact order and structure of context elements. |
| `ai-debug-completion-limited-context` | `C-c i C-d l` | Shows the context specifically for code completion with *limited preceding and following context sizes*. Useful for debugging how completion handles truncated views of your code. |
| `ai-debug-completion-full-context` | `C-c i C-d f` | Shows the context specifically for code completion using the *entire file content as context*. Useful for debugging full-buffer completion strategies. |

### Debug Buffer Interaction

Once a debug buffer (e.g., `*AI Debug Context*`, `*AI Context Sources*`) is open, you can interact with it using `magit-section-mode` keybindings:

| Binding | Command | Description |
|---|---|---|
| `TAB` | `magit-section-toggle` | Expand or collapse the section at point. |
| `C-c t` | `ai-debug-toggle-truncation` | Toggle content truncation within the debug buffer. When enabled, long strings and lists are shortened to improve performance and readability; disable it to see full content. |
| `C-r` | `ai-debug-refresh-buffer` | Refresh the content of the current debug buffer with the latest AI context information. |

## Understanding AI Context

When you interact with AI Mode, a comprehensive context is assembled and sent to the AI model. This context ensures the AI has all the necessary information to provide relevant and accurate responses. The context is built from various sources:

*   **Code-Related Context**:
    *   **Active Region/File Content**: If a region is selected, its content is sent (enclosed in `<selection>` tags). Otherwise, the entire buffer content may be included (enclosed in `<file-context>` tags).
    *   **Preceding and Following Context**: Lines of code immediately before (`<preceding-context>`) and after (`<following-context>`) the cursor/region are included to provide surrounding code awareness.
    *   **File Metadata**: Information such as the current file's path, buffer name, and programming language (`<file-metadata>` within `<additional-context source="file-metadata">`).
*   **Interaction-Related Context**:
    *   **Model Information**: Details about the selected AI model (e.g., name, provider) (`<model>` within `:model-context` in the execution context).
    *   **User Input**: Explicit prompts or questions you provide (`<user-input>` with `source="user-input"`).
    *   **Additional Context Pool**: Content manually added to the temporary context pool by you (e.g., using `ai-context-management--add-to-context-pool`). This is grouped by source (`<additional-context source="context-pool">`).
*   **Instructional Context**:
    *   **Global System Instructions**: General instructions that apply to all AI interactions (managed by `ai-context-management--global-system-prompts`) (`<agent-instructions source="global-system-prompts">`).
    *   **Global Memory**: Persistent information stored in global memory for all sessions (managed by `ai-context-management--global-memo-context`) (`<memory-content source="global-memory">` within `<memory>`).
    *   **Buffer-Bound Prompts**: Instructions specifically tied to the current buffer (managed by `ai-context-management--buffer-bound-prompts`) (`<agent-instructions source="buffer-bound-prompts">`).
    *   **Action-Specific Instructions**: Prompts and examples loaded from `.ai/commands` files for specific actions (e.g., "explain", "modify", "complete") (`<agent-instructions source="command-specific-instructions">`, `<agent-instructions source="command-examples">`).
    *   **Action Object Rules**: Instructions on how to interpret and generate structured action objects (`<modify-action-object-rules>`, `<complete-action-type-object-rules>`, etc., within `<agent-instructions source="action-object-rules">`).
    *   **Result Action Rules**: Instructions on how the AI's response should be formatted and applied (`<agent-instructions source="result-action-format">`).
*   **Project Context**:
    *   **Full Project Files**: All filtered files in the current project (`<file-content>` within `<files>` within `<project-context>`). Enabled by `ai-context-management--project-context-mode` set to `full-project`.
    *   **Project AI Summary Index**: AI-generated summaries of project files from the cached index (`<file-summary>` within `<files>` within `<project-ai-summary>`). Enabled by `ai-context-management--project-context-mode` set to `project-ai-summary`.

This layered approach ensures that AI has a deep understanding of your code, environment, and intentions for each interaction.

## Customization Options

AI Mode provides several important customizable variables to tailor its behavior to your workflow. These can be adjusted using `M-x customize-group RET ai RET` or by setting them directly in your Emacs configuration file (e.g., `init.el`).

| Variable | Description | Default |
|---|---|---|
| **General AI Mode Settings** | | |
| `ai-keymap-prefix` | The prefix keybinding for AI commands (e.g., `C-c i`). | `"C-c i"` |
| `ai-command-management-command-prompt` | Prompt for selecting the command. | `"Command or query: "` |
| `ai-model-management-change-execution-backend-prompt` | Prompt for selecting the AI backend/model. | `"Select execution backend: "` |
| `ai-context-management--user-input-method` | Function to use for collecting user input. | `ai-user-input-ctrl-enter` |
| `ai-mode-line-progress-indicator-enabled` | If non-nil, enables progress indicators in the mode line for general AI requests. | `t` |
| `ai-mode-line-progress-indicator-style` | Style of progress indicator for general AI requests: `spinner`, `dots`, or `message`. | `spinner` |
| `ai-context-management--default-preceding-context-size` | Number of lines to include in the preceding context for most AI operations. | `20` |
| `ai-context-management--default-following-context-size` | Number of lines to include in the following context for most AI operations. | `20` |
| `ai-prompt-management--instruction-watching-enabled` | Enable file watching for instruction files to auto-reload cache. | `t` |
| `ai-context-management--extended-instructions-enabled` | If non-nil, enables loading of extended instructions for AI prompts (e.g., basic, action-specific, examples). | `t` |
| `ai-context-management--current-buffer-additional-context` | If non-nil, includes the current buffer's entire content as additional context, especially when a region is active. This can be useful for whole-file understanding, but can increase token usage. | `t` |
| `ai-command-management-commands-config-map` | An association list mapping AI commands to their configurations. | (See definition in `ai-command-management.el`) |
| `ai-execution--prompt-caching-enabled` | Enable prompt caching for AI requests. | `nil` |
| `ai-execution--replace-action-use-patch` | Use unified patch format for replace actions instead of direct replacement. | `nil` |
| `ai-project-ignore-file-name` | Name of the AI ignore file, typically in the project root. | `".ai-ignore"` |
| `ai-project-global-ignore-patterns` | Global hardcoded ignore patterns that are always applied. | (See definition in `ai-project.el`) |
| `ai-project-global-ignore-files` | List of global ignore files to be processed (e.g., `~/.global-gitignore`). | (See definition in `ai-project.el`) |
| **Project Context & Indexing Settings** | | |
| `ai-context-management--project-context-mode` | Project context inclusion mode: `disabled`, `full-project` (all filtered files), or `project-ai-summary` (AI-generated summaries). | `disabled` |
| `ai-mode-indexing--include-existing-context` | Include context from already indexed files when indexing new files. | `t` |
| `ai-mode-indexing--strategy` | Strategy for file indexing: `parallel-independent` (no context sharing) or `sequential` (with accumulating session context). | `parallel-independent` |
| `ai-mode-indexing--index-retention-depth` | Number of index versions to retain before cleanup. | `5` |
| `ai-mode-indexing--call-timeout` | Timeout in seconds between individual file indexing calls during project summary generation. | `0.1` |
| **Code Completion Specific Settings** | | |
| `ai-completions--current-precending-context-size` | Current number of lines used as context *before* the cursor for AI code completion sessions. | `20` |
| `ai-completions--current-forwarding-context-size` | Current number of lines used as context *after* the cursor for AI code completion sessions. | `20` |
| `ai-completions--context-size-step` | Step size to increase or decrease context size during completion. | `10` |
| `ai-completions--continue-commands` | A list of commands allowed during an active completion session without aborting it. | `(not save-buffer ...)` |
| `ai-completions--abort-commands` | A list of commands that will abort an active completion session. | `(not ai-completions--select-next-or-abort ...)` |
| **AI Chat Specific Settings** | | |
| `ai-chat-prompt` | Prompt text for the AI chat buffer. | `AI> ` |
| `ai-chat-buffer-context-size` | Number of historical messages to display and include as context in AI chat. | `5` |
| `ai-chat-history-directory` | Directory where AI chat session history files are stored. | `~/.ai-chat-history` |
| `ai-chat-auto-save-enabled` | Whether to automatically save chat sessions after each interaction. | `nil` |
| `ai-chat-progress-indicator-enabled` | Enable progress indicator for AI chat requests. | `t` |
| `ai-chat-progress-indicator-style` | Style of progress indicator to use for AI chat requests (`spinner`, `dots`, `message`). | `spinner` |
| `ai-chat-change-backend-prompt` | Prompt for selecting the query backend in AI chat. | `"Select query backend: "` |
| `ai-chat-language-mapping` | Maps external language names to Emacs mode names for syntax highlighting in chat code blocks. | `(("elisp" . "emacs-lisp") ...)` |
| **Buffer-Bound Chat Settings** | | |
| `ai-chat-per-buffer-enabled` | Enable buffer-bound chat sessions. When disabled, use a single global chat buffer. | `t` |
| `ai-chat-allow-multiple-sessions-per-buffer` | Allow multiple parallel chat sessions for a single source buffer. | `t` |
| `ai-chat-bound-buffer-name-format` | Format string for buffer-bound chat buffer name. `%s` is replaced with display name. | `"*ai-chat:%s*"` |
| `ai-chat-startup-context-strategy` | Strategy for startup context when creating buffer-bound chat sessions. | `ask` |
| `ai-chat-send-enrichment-as` | Type of struct to use when enriching bound chat from source buffer. | `additional-context` |
| `ai-chat-default-behavior-for-buffer` | Default behavior when opening chat for buffer (create-new, reuse-existing, ask-user). | `create-new` |
| **Logging & Debugging Settings** | | |
| `ai-telemetry-enabled` | Enable telemetry collection for AI mode. | `t` |
| `ai-telemetry-write-log-buffer` | If non-nil, enables logging of AI requests and responses to `*AI-request-log*` buffer. | `nil` |
| `ai-logging-verbose-level` | Logging level for AI mode. Possible values: `silent`, `error`, `warn`, `info`, `debug`, `trace`. | `info` |
| `ai-telemetry-write-to-prompt-buffer` | If non-nil, writes the full prompt sent to the AI to `*AI prompt*` buffer. | `nil` |
| `ai-network--default-request-timeout` | Default timeout for HTTP requests in seconds. | `60` |
| `ai-debug-truncate-content` | Whether to truncate long content in debug buffers for performance. | `t` |
| `ai-debug-max-content-length` | Maximum length of content to display when truncation is enabled in debug buffers. | `2000` |
| `ai-debug-max-recursion-depth` | Maximum recursion depth for nested content display in debug buffers. | `2` |
| `ai-debug-max-list-items` | Maximum number of list items to display in debug buffers. | `100` |
| `ai-debug-max-plist-lines` | Maximum number of property list lines to display in debug buffers. | `40` |
| `ai-usage-enabled` | Enable usage statistics collection for AI mode. | `t` |
| `ai-request-audit-enabled` | Enable structured request auditing for AI mode. | `t` |
| `ai-request-audit-max-records` | Maximum number of audit records to keep per project. | `100` |
| `ai-request-audit-cleanup-old-records` | Automatically cleanup old audit records when `max-records` is exceeded. | `t` |

## File Structure Overview

The `ai-mode` package is composed of several `.el` files, each responsible for a specific set of functionalities. This modular design enhances maintainability and allows for independent development and testing of features.

| File | Description |
|---|---|
| `Eask` | Manages package metadata, dependencies, and source files for `ai-mode` building and installation. |
| `ai.el` | The core package file. It defines the main `ai` group and provides the foundational namespace for the entire AI mode system. |
| `ai-chat.el` | Implements the interactive AI chat interface with buffer-bound sessions, startup context strategies, session management, and rich context integration. |
| `ai-command-management.el` | Defines, parses, loads, and dispatches AI commands. Manages instruction file loading, caching, and modifier parsing. |
| `ai-common.el` | Contains core AI primitives and general data structures like `typed-struct`. Provides basic operations on these structures, such as creation and update. |
| `ai-completions.el` | Implements AI-powered code completion, managing suggestion requests, session management, and applying completions. |
| `ai-context-management.el` | Centralizes the collection, structuring, and storage of context for AI requests, including extended context capabilities and comprehensive context assembly functions. |
| `ai-core.el` | Serves as the central orchestrator and primary location for fundamental AI-mode logic, coordinating calls between different modules. Includes extended chat functionality. |
| `ai-debug.el` | Provides visual tools for debugging and introspecting AI states, including prompt composition, execution context, and model configuration. |
| `ai-execution.el` | Manages AI request execution and asynchronous interaction with AI backends, handling success and failure callbacks. |
| `ai-logging.el` | Provides logging utilities for AI mode, including message logging, multi-level verbose control, and structured request/response logging. |
| `ai-mode-adapter-api.el` | Provides a standardized, type-safe API for `ai-mode` to interact with external AI backends, handling message preparation and content extraction. |
| `ai-mode-indexing.el` | Manages the project indexing system for AI, encompassing file summary generation, persistence, and versioning. |
| `ai-mode-line.el` | Provides mode line utilities and indicators for AI mode, including progress tracking, status displays, and integration with doom-modeline. |
| `ai-mode.el` | Defines the `ai-mode` minor mode with enhanced keybindings for buffer-bound chat, manages the overall UI interaction flow, feature integration, and top-level settings. |
| `ai-model-management.el` | Responsible for configuring, selecting, and managing available AI models and their providers. |
| `ai-network.el` | Manages network communication and connectivity for AI mode, including HTTP request handling, connection pooling, retry logic, timeout management, and network-related error handling for AI service interactions. |
| `ai-progress.el` | Manages progress tracking business logic for long-running AI operations, including progress state management, cancellation support, and data structures for tracking AI request progress. |
| `ai-project.el` | Contains project management utilities for AI mode, including project root detection, file filtering by ignore patterns, and creating project-related data structures. |
| `ai-prompt-management.el` | Manages the creation, assembly, rendering, and caching of prompts and instructions for AI models. |
| `ai-request-audit.el` | Provides a structured request auditing system for AI mode, including structured storage of request/response data, unique request identification and tracking, and project-based audit organization. |
| `ai-response-processors.el` | Contains functions for processing and handling responses from AI models, including text extraction, formatting, display, and callback creation for various response insertion and replacement strategies. |
| `ai-structs.el` | Contains formal definitions of all common data structures (e.g., `typed-struct` and its derived types) used throughout AI-mode. This file is primarily for data structure definitions. |
| `ai-telemetry.el` | Handles the collection and reporting of telemetry data for usage statistics and performance monitoring. |
| `ai-usage.el` | Handles usage statistics and token counting for AI mode, including token usage tracking and reporting, and session statistics management. |
| `ai-user-input.el` | Provides user input utilities for AI mode, including various methods for collecting multiline input from users. |
| `ai-utils.el` | Contains general-purpose utility functions that do not fit into other specific categories, such as logging, HTTP request handling, basic buffer manipulation, and string templating. |

## Related Projects

- [Org Babel support](https://github.com/ai-mode/ob-ai)

## Legal Notice

This project is an independent open-source initiative and is not affiliated with, endorsed by, or sponsored by Anthropic, PBC, OpenAI, Inc., DeepSeek, Hugging Face, Inc., or Google LLC.

Claude is a trademark of Anthropic, PBC. OpenAI, GPT, and ChatGPT are trademarks or registered trademarks of OpenAI, Inc. DeepSeek is a trademark of DeepSeek. Hugging Face and the Hugging Face logo are trademarks or registered trademarks of Hugging Face, Inc. Google and Gemini are trademarks of Google LLC. All other trademarks mentioned in this documentation are the property of their respective owners.

The use of AI APIs (such as Anthropic's, OpenAI's, Google's, DeepSeek's, and Hugging Face's) is subject to their respective terms of service and usage policies. Users are responsible for ensuring their usage complies with all applicable terms and regulations.
