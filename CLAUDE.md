# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs major mode for the Verse programming language used in Unreal Editor for Fortnite (UEFN). The project provides syntax highlighting, comment handling, indentation, and basic navigation support for `.verse` files.

## Development Commands

### Setup and Dependencies
```bash
cask install        # Install development dependencies
```

### Development Workflow
```bash
make deps          # Install dependencies (equivalent to cask install)
make lint          # Run package-lint on verse-mode.el
make compile       # Byte-compile the Emacs Lisp file
make test          # Run tests using ert-runner
make clean         # Remove compiled files and .cask directory
```

## Architecture

### Core Files
- `verse-mode.el` - Main implementation containing:
  - Syntax table with comment handling (`#` line comments, `<# ... #>` block comments)
  - Font-lock keywords for Verse language constructs
  - Simple indentation logic based on `:` and `{` delimiters
  - Imenu support for functions, classes, and modules
  - Mode definition and auto-mode-alist registration

- `tests/verse-mode-test.el` - Basic smoke test ensuring mode activation

### Language Features Implemented
- Keywords: `if`, `else`, `for`, `loop`, `var`, `set`, `module`, `class`, etc.
- Types: `logic`, `int`, `float`, `string`, `array`, `map`, etc.
- Syntax highlighting for function definitions, module paths, and attributes
- Block comment support via `syntax-propertize-function`

### Build System
Uses Cask for dependency management and Make for build tasks. The project includes package-lint for code quality and ert-runner for testing.

## Development Notes

The indentation system (`verse-indent-line`) uses a simple heuristic:
- Increases indentation after lines ending with `:` or `{`
- Decreases indentation for lines starting with `}` or `else`

Font-lock patterns match:
- Function definitions: `FunctionName(args) : type =`
- Module imports: `using { /Path/Module }`
- Attributes: `<public>`, `<localizes>`