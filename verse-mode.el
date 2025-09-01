;;; verse-mode.el --- Major mode for Epic's Verse language (UEFN) -*- lexical-binding: t; -*-
;; Author: Steven Ness <sness@sness.net>
;; Maintainer: Steven Ness <sness@sness.net>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools
;; URL: https://github.com/sness23/verse-mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; verse-mode provides comprehensive editing support for the Verse
;; programming language used in Unreal Editor for Fortnite (UEFN):
;; syntax highlighting, comment handling (single-line `#` and block
;; `<# ... #>`), indentation, Imenu, and `auto-mode-alist` for
;; `.verse` files.
;;
;; Verse is a functional logic programming language developed by Epic
;; Games that supports:
;; - First-class type system with effect system
;; - Mutable state and I/O effects
;; - Transactional memory
;; - Classes, structs, and inheritance
;; - Expression-based syntax
;;

;;; Code:

(require 'imenu)
(require 'rx)

(defgroup verse nil
  "Major mode for the Verse language."
  :group 'languages
  :prefix "verse-")

(defcustom verse-indent-offset 4
  "Number of spaces for each indentation level."
  :type 'integer
  :group 'verse)

;; ----------------------------------------------------------------------------
;; Syntax table
;; ----------------------------------------------------------------------------
(defvar verse-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Single-line comments start with '#'
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; Strings: double quotes
    (modify-syntax-entry ?\" "\"" st)

    ;; Word constituents: underscore
    (modify-syntax-entry ?_ "w" st)

    ;; Braces, parens, brackets
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ;; Assignment and comparison operators
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)

    st)
  "Syntax table for `verse-mode'.")

;; Block comments `<# ... #>` (with nesting allowed in Verse docs).
;; We mark <# as comment-start and #> as comment-end via syntax-propertize.
(defun verse--syntax-propertize (start end)
  "Apply syntax properties for Verse block comments between START and END."
  (goto-char start)
  (remove-text-properties start end '(syntax-table))
  (funcall
   (syntax-propertize-rules
    ;; Open delimiter: `<#`
    ((rx "<#") (0 "<"))
    ;; Close delimiter: `#>`
    ((rx "#>") (0 ">")))
   start end))

;; ----------------------------------------------------------------------------
;; Font-lock (keywords and syntax highlighting)
;; ----------------------------------------------------------------------------
(defconst verse--keywords
  '("if" "else" "for" "loop" "break" "continue" "return" "block"
    "var" "set" "module" "class" "struct" "where" "subtype" "enum"
    "sync" "branch" "await" "concurrent" "try" "catch" "throw"
    "using" "import" "spawn" "race" "select" "defer" "do"
    "true" "false" "nil" "mut" "ref" "out" "in" "option"))

(defconst verse--types
  '("logic" "int" "float" "string" "message" "locale" "rational" "any" "void"
    "array" "map" "tuple" "option" "type" "comparable" "persistable"
    "char" "byte" "[]" "weak_map" "event" "asset" "creative_device"
    "player" "agent" "fort_character" "game" "creative_prop"))

(defconst verse--specifiers
  '("public" "private" "protected" "override" "abstract" "final" "native"
    "localizes" "editable" "epic_internal" "experimental" "deprecated"
    "transacts" "decides" "no_rollback" "suspends" "varies" "computes"
    "converges" "query" "native_callable"))

(defconst verse-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt verse--keywords 'symbols) . font-lock-keyword-face)
    ;; Types
    (,(regexp-opt verse--types 'symbols) . font-lock-type-face)
    ;; Specifiers in angle brackets like <public>, <localizes>
    (,(concat "<\\(" (regexp-opt verse--specifiers) "\\)>") 1 font-lock-builtin-face)
    ;; Assignment operator :=
    (,(rx ":=") . font-lock-keyword-face)
    ;; Function definitions: FunctionName(args) : type =
    (,(rx line-start (* space)
          (group (+ (or word ?_ ?.))) (* space)
          "(") 1 font-lock-function-name-face)
    ;; Class/struct/module definitions: Name := class/struct/module
    (,(rx line-start (* space)
          (group (+ (or word ?_ ?.))) (* space)
          ":=" (* space)
          (or "class" "struct" "module" "enum")) 1 font-lock-type-face)
    ;; Module paths inside using { /Org/Module }
    (,(rx "using" (+ space) "{" (+ space) (group (+ (any ?/ ?_ ?. word))) (+ space) "}")
     1 font-lock-constant-face)
    ;; String interpolation {expression} within strings
    (,(rx "\"" (* (or (not (any ?\\ ?\")) (seq ?\\ any)))
          "{" (group (* (not (any ?}))))  "}"
          (* (or (not (any ?\\ ?\")) (seq ?\\ any))) "\"") 1 font-lock-variable-name-face)
    ;; Numbers (integers and floats)
    (,(rx (or line-start (not (any word ?_)))
          (group (+ digit) (? "." (+ digit)))
          (or line-end (not (any word ?_)))) 1 font-lock-constant-face)
    ;; Attributes with @ syntax like @editable
    (,(rx "@" (group (+ (or word ?_)))) 1 font-lock-preprocessor-face)
    ;; Generic attributes/specifiers like <public>, <localizes> (fallback)
    (,(rx "<" (group (+ (or word ?_))) ">") 1 font-lock-preprocessor-face)
    ;; Variable names in var declarations
    (,(rx "var" (+ space) (group (+ (or word ?_))) (* space) ":") 1 font-lock-variable-name-face)))

;; ----------------------------------------------------------------------------
;; Indentation
;; ----------------------------------------------------------------------------
(defun verse--current-line-indentation ()
  "Get the indentation of the current line."
  (save-excursion
    (beginning-of-line)
    (current-indentation)))

(defun verse--previous-line-indentation ()
  "Get the indentation of the previous non-empty line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[ \t]*$"))
      (forward-line -1))
    (current-indentation)))

(defun verse--line-ends-with-opener-p ()
  "Check if the previous line ends with an opener like : or {."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t")
    (skip-chars-backward "#" (line-beginning-position))  ; Skip comments
    (skip-chars-backward " \t")
    (memq (char-before) '(?: ?{ ?=))))

(defun verse--line-starts-with-closer-p ()
  "Check if the current line starts with a closer like } or else."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (or (looking-at-p "}")
        (looking-at-p "else\\b"))))

(defun verse-indent-line ()
  "Indent current line according to Verse indentation rules."
  (interactive)
  (let* ((prev-indent (verse--previous-line-indentation))
         (indent prev-indent))
    
    ;; Increase indent after lines ending with : or { or =
    (when (verse--line-ends-with-opener-p)
      (setq indent (+ prev-indent verse-indent-offset)))
    
    ;; Decrease indent for lines starting with } or else
    (when (verse--line-starts-with-closer-p)
      (setq indent (max 0 (- indent verse-indent-offset))))
    
    (indent-line-to (max 0 indent))))

;; ----------------------------------------------------------------------------
;; Imenu
;; ----------------------------------------------------------------------------
(defvar verse-imenu-generic-expression
  `(("Functions" ,(rx line-start (* space)
                     (group (+ (or word ?_ ?.))) (* space) "(") 1)
    ("Classes" ,(rx line-start (* space)
                   (group (+ (or word ?_ ?.))) (* space) ":=" (* space) "class") 1)
    ("Modules" ,(rx line-start (* space)
                   (group (+ (or word ?_ ?.))) (* space) ":=" (* space) "module") 1)
    ("Structs" ,(rx line-start (* space)
                   (group (+ (or word ?_ ?.))) (* space) ":=" (* space) "struct") 1)
    ("Enums" ,(rx line-start (* space)
                 (group (+ (or word ?_ ?.))) (* space) ":=" (* space) "enum") 1))
  "Imenu expressions for `verse-mode'.")

;; ----------------------------------------------------------------------------
;; Mode definition
;; ----------------------------------------------------------------------------
;;;###autoload
(define-derived-mode verse-mode prog-mode "Verse"
  "Major mode for editing Verse (UEFN) source files.

Verse is a functional logic programming language developed by Epic Games
for use in Unreal Editor for Fortnite (UEFN).  This mode provides:

- Comprehensive syntax highlighting for keywords, types, and specifiers
- Support for single-line (#) and block (<# ... #>) comments
- Smart indentation based on code structure
- Imenu support for navigation
- Auto-mode-alist integration for .verse files

\\{verse-mode-map}"
  :syntax-table verse-mode-syntax-table
  (setq-local font-lock-defaults '(verse-font-lock-keywords))
  (setq-local syntax-propertize-function #'verse--syntax-propertize)
  (setq-local indent-line-function #'verse-indent-line)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local imenu-generic-expression verse-imenu-generic-expression)
  
  ;; Electric indentation
  (setq-local electric-indent-chars '(?{ ?} ?: ?=)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.verse\\'" . verse-mode))

(provide 'verse-mode)
;;; verse-mode.el ends here
