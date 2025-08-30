# verse-mode

An Emacs **major mode** for [Verse](https://dev.epicgames.com/documentation/en-us/fortnite/verse-language-reference), the language used in **UEFN** (Unreal Editor for Fortnite).

## Features
- Syntax highlighting (keywords, types, modules, attributes)
- Comments: `#` line comments and `<# ... #>` block comments (via `syntax-propertize`)
- Simple indentation heuristic
- Imenu for functions, classes, and modules
- `auto-mode-alist` for `*.verse` files
- Minimal test + CI + MELPA recipe

> Note: Verse docs: quick reference and comment syntax are on Epic's site. See links inline in the source.

## Install locally
```elisp
(add-to-list 'load-path "/path/to/verse-mode")
(require 'verse-mode)
```

## Dev workflow
```bash
cask install
make lint
make compile
make test
```

## Roadmap
- Better indentation (parse `:=`, blocks, and braces)
- String interpolation font-lock inside `"{...}"`
- Eldoc & xref using UEFN LSP if Epic exposes it
- Snippets (`yasnippet`) for common patterns

PRs welcome!
