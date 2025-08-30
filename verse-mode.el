;;; verse-mode.el --- Major mode for Epic's Verse language (UEFN) -*- lexical-binding: t; -*-
;; Author: Your Name <you@example.com>
;; Maintainer: Your Name <you@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools
;; URL: https://github.com/yourname/verse-mode
;; License: GPL-3.0-or-later

;;; Commentary:

;; verse-mode provides basic editing support for the Verse programming language used
;; in Unreal Editor for Fortnite (UEFN): syntax highlighting, comment handling
;; (single-line `#` and block `<# ... #>`), simple indentation, Imenu, and
;; `auto-mode-alist` for `.verse` files.
;;
;; Docs:
;; - Verse quick reference (Epic): https://dev.epicgames.com/documentation/en-us/fortnite/verse-language-quick-reference
;; - Comments in Verse: https://dev.epicgames.com/documentation/en-us/fortnite/comments-in-verse
;; - Modules and paths: https://dev.epicgames.com/documentation/en-us/fortnite/modules-and-paths-in-verse
;;
;; This is a minimal starter mode intended to get you productive quickly; PRs welcome!

;;; Code:

(require 'imenu)
(require 'rx)

(defgroup verse-mode nil
  "Major mode for the Verse language."
  :group 'languages
  :prefix "verse-")

(defcustom verse-indent-offset 4
  "Number of spaces for each indentation level."
  :type 'integer
  :group 'verse-mode)

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
    ((rx \"<#\") (0 \"<\"))
    ;; Close delimiter: `#>`
    ((rx \"#>\") (0 \">\")))
   start end))

;; ----------------------------------------------------------------------------
;; Font-lock (keywords)
;; ----------------------------------------------------------------------------
(defconst verse--keywords
  '(\"if\" \"else\" \"for\" \"loop\" \"break\" \"continue\" \"return\" \"block\"
    \"var\" \"set\" \"module\" \"class\" \"struct\" \"where\" \"subtype\"
    \"sync\" \"branch\" \"await\" \"concurrent\" \"try\" \"catch\" \"throw\"))

(defconst verse--types
  '(\"logic\" \"int\" \"float\" \"string\" \"message\" \"locale\" \"rational\" \"any\" \"void\"
    \"array\" \"map\" \"tuple\" \"option\"))

(defconst verse-font-lock-keywords
  `(
    ;; Keywords
    (,(regexp-opt verse--keywords 'symbols) . font-lock-keyword-face)
    ;; Types
    (,(regexp-opt verse--types 'symbols) . font-lock-type-face)
    ;; Function names:  Foo(args) : type =
    (,(rx line-start (* space)
          (group (+ (or word ?_ ?.))) (* space)
          \"(\") 1 font-lock-function-name-face)
    ;; Module paths inside using {{ /Org/Module }}
    (,(rx \"using\" (+ space) \"{\" (+ space) (group (+ (any ?/ ?_ ?. word))) (+ space) \"}\")
     1 font-lock-constant-face)
    ;; Attributes/specifiers like <public>, <localizes>
    (,(rx \"<\" (group (+ (or word ?_))) \">\") 1 font-lock-preprocessor-face)
    ))

;; ----------------------------------------------------------------------------
;; Indentation
;; ----------------------------------------------------------------------------
(defun verse--line-indentation ()
  \"Compute indentation for current line.\"
  (save-excursion
    (beginning-of-line)
    (let ((indent 0)
          (not-indented t))
      (while not-indented
        (setq not-indented nil))
      indent)))

(defun verse-indent-line ()
  \"Indent current line according to Verse simple rules.\"
  (interactive)
  (let* ((indent 0))
    ;; naive heuristic: increase after lines ending with ':' or '{'
    (save-excursion
      (forward-line -1)
      (when (looking-at \".*[:{][ \t]*\\(#.*\\)?$\")
        (setq indent (+ (current-indentation) verse-indent-offset))))
    ;; decrease if current line starts with '}' or 'else'
    (save-excursion
      (beginning-of-line)
      (when (looking-at \"[ \t]*\\(}\\|else\\)\\b\")
        (setq indent (max 0 (- indent verse-indent-offset)))))
    (indent-line-to (max 0 indent))))

;; ----------------------------------------------------------------------------
;; Imenu
;; ----------------------------------------------------------------------------
(defvar verse-imenu-generic-expression
  `((\"Functions\" ,(rx line-start (* space)
                      (group (+ (or word ?_ ?.))) (* space) \"(\") 1)
    (\"Classes\" ,(rx line-start (* space)
                    (group (+ (or word ?_ ?.))) (* space) \":=\" (* space) \"class\") 1)
    (\"Modules\" ,(rx line-start (* space)
                    (group (+ (or word ?_ ?.))) (* space) \":=\" (* space) \"module\") 1))
  \"Imenu expressions for `verse-mode'.\")

;; ----------------------------------------------------------------------------
;; Mode definition
;; ----------------------------------------------------------------------------
;;;###autoload
(define-derived-mode verse-mode prog-mode \"Verse\"
  \"Major mode for editing Verse (UEFN) source files.\"
  :syntax-table verse-mode-syntax-table
  (setq-local font-lock-defaults '(verse-font-lock-keywords))
  (setq-local syntax-propertize-function #'verse--syntax-propertize)
  (setq-local indent-line-function #'verse-indent-line)
  (setq-local comment-start \"# \")
  (setq-local comment-end \"\")
  (setq-local imenu-generic-expression verse-imenu-generic-expression))

;;;###autoload
(add-to-list 'auto-mode-alist '(\"\\.verse\\'\" . verse-mode))

(provide 'verse-mode)
;;; verse-mode.el ends here
