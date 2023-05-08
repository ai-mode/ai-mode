# AI mode for Emacs

[![LICENSE](https://img.shields.io/github/license/ai-mode/ai-mode)](https://github.com/ai-mode/ai-mode/blob/master/LICENSE)


#  Overview

This package provides support for AI in Emacs in several ways.

- Interaction with AI in interactive chat mode.
- Using AI for code and text completion in AI completion mode.
- Using AI to modify code.
- Using AI to explain code.
- Extension for org babel


# Installation


### Installation via MELPA

It's easiest/recommended to install from MELPA. Here's a minimal MELPA configuration for your ~/.emacs:

```elisp
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Afterwards, M-x package-install RET ai-mode RET (you might want to M-x package-refresh-contents RET beforehand if you haven't done so recently).


### Installation via GIT

Clone this repository somewhere

```bash
$ cd ~/.emacs.d/plugins
$ git clone --recursive https://github.com/ai-mode/ai-mode
```

Add the following in your .emacs file:

```elisp
(add-to-list 'load-path
              "~/.emacs.d/plugins/ai-mode")
(require 'ai-mode)
```


### Configuration

To use OpenAI backends, you need to obtain an authorization key. You can get the key on the [page](https://platform.openai.com/account/api-keys).

There are several ways to assign your key to the variable `ai-openai--api-key`.

1. Put `(setq ai-openai--api-key "you key")` in the configuration file.
2. Use the command M-x `set-variable ai-openai--api-key`.
3. Use the command M-x `customize-variable ai-openai--api-key`.


```elisp
(use-package ai-mode
  :config (progn
                (global-ai-mode)
                (global-set-key (kbd "C-<tab>") 'ai-complete-code-at-point)
                (setq ai--query-type-map '(("translate-into-english" . "Translate into english: %s")
                                       ("fix" . "Here is a bug in the following function, please help me fix it: %s")
                                       ("improve" . "Improve and extend the following code: %s")
                                       ("explain" . "Explain following code: %s")
                                       ("optimize" . "Optimize the following code: %s")
                                       ("spellcheck" . "Spellcheck this text: %s")
                                       ("elaborate" . "Elaborate on this text: %s")
                                       ("document" . "Please add the documentation for the following code: %s")
                                       ("refactor" . "Refactor the following code: %s")))))
```

# Features

Extension capabilities can be divided into 4 areas.

## ChatAI

Interactive mode for interacting with AI. In this mode, you can ask AI questions in an interactive way and receive answers. After typing a message, you can send it using the `C-return>` key combination.

[Place the video here.]

### List of available commands:

- `ai-chat-change-backend` - changing the AI backend used for processing requests
- `ai--chat--return` - sends a request to the AI backend
- `ai--chat--interrupt` - interrupts the request processing
- `ai--chat--clear-buffer` - clears the interaction history
- `ai--chat--increase-context-size` - increases the number of messages in the context. By default, it is 1. The more context, the more accurate the response, but the higher the cost of the request.
- `ai--chat--decrease-context-size` - decreases the number of messages in the context.

### Customization of chat mode.

- `(setq ai-chat--prompt "SuperAI>")` - changing the greeting
- `(setq ai-chat--buffer-context-size 10)` - changing the context size

If you want to write your own backend, take a look at the source code of the `ai-openai--chat-async-send-context` function.

Adding a customized backend:

```elisp
(add-to-list 'ai-chat--query-backends '("SuperAI backend" . super-chat-ai-backend))
```


## Code completion

[Place the video here.]

To invoke completion, you need to call the command `ai-complete-code-at-point`. For convenience, you can assign a keyboard shortcut to the command, for example `C-<tab>`.

List of contextual commands in autocomplete mode after preview display.

| binding    | command                                    | Explanation              |
|------------|--------------------------------------------|--------------------------|
| `\e\e\e`   | `ai--completion-abort`                     | Disable autocomplete     |
| `\C-g`     | `ai--completion-abort`                     | Disable autocomplete     |
| `C-<tab>`  | `ai--completion-select-next-or-abort`      | Show the next candidate  |
| `C-n`      | `ai--completion-select-next-or-abort`      | Show the next candidate  |
| `<down>`   | `ai--completion-select-next-or-abort`      | Show the next candidate  |
| `C-p`      | `ai--completion-select-prev-or-abort`      | Show the prev candidate  |
| `<up>`     | `ai--completion-select-prev-or-abort`      | Show the prev candidate  |
| `[return]` | `ai--completion-select-current`            | Select current candidate |
| `RET`      | `ai--completion-select-current`            | Select current candidate |
| C-i        | `ai-completions--increase-current-context` | Increase context         |




### Customization of code completion mode

- `(setq ai-chat--prompt "SuperAI>")` - changing the greeting
- `(setq ai-completion--context-size 10)` - changing the context size

If you want to write your own backend, take a look at the source code of the `ai-openai--completions-completion-backend` function.

Adding a customized backend:

```elisp
(add-to-list 'ai--completion-backends '("SuperAI completion backend" . super-ai-completion-backend))
```


## Code manipulation

[Place the video here.]

This mode includes several commands that allow modifying, adding to, and changing the code and text.

### List of available commands:

- `ai-perform` - takes the content of the selected region, applies a command from the `ai--query-type-map` list to it, or uses the entered text as the command, and replaces the region with the resulting response.
- `ai-insert-doc-marker` - places a documentation marker
- `ai-document-code-region` - generates documentation for the selected region and replaces the marker with the resulting documentation
- `ai-show` - displays the AI response in a new buffer
- `ai-explain-code-region` - explains the selected code fragment in a special help buffer


### Customization of code manipulation mode

- `(setq ai--doc-tag "{{__ai_doc__}}")` - changing the marker for documentation placement
- `(setq ai--change-backend-prompt "Select query backend:")` - changing the greeting for changing the backend
- `(setq ai--query-type-prompt "Type of Query:")` - changing the greeting for selecting the query type
- `(setq ai-keymap-prefix "C-c i")` - changing the prefix for keymap

If you want to write your own backend, take a look at the source code of the ` ai-openai--chat-async-send-query` function.

Adding a customized backend:

```elisp
(add-to-list 'ai-async-query-backends '("SuperAI completion backend" . super-ai-backend))
```

## Org babel support

The org babel extension allows executing blocks using an AI engine. This functionality is extracted into a separate repository [ob-ai](https://github.com/ai-mode/ob-ai). You can learn more about this feature there.


## Available commands

`ai-change-query-backend` - allows you to change the current backend to another one from the list `ai-async-query-backends`.


## Related projects

- [Codeium backend](https://github.com/ai-mode/ai-mode-codeium)
- [org babel support](https://github.com/ai-mode/ob-ai)



## Alternatives

- [https://github.com/stuhlmueller/gpt.el](GPT.el)
- [https://github.com/xenodium/chatgpt-shell](ChatGPT-shell)
- [https://github.com/Exafunction/codeium.el](Codeium)
