;;; lawvere-mode.el --- Language support for the Lawvere programming language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 James Henri Haydon
;;
;; Author: James Henri Haydon <https://github.com/jameshaydon>
;; Maintainer: James Henri Haydon <james.haydon@gmail.com>
;; Created: February 04, 2021
;; Modified: February 04, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/james/lawvere-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Language support for the Lawvere programming language
;;
;;; Code:

(eval-when-compile
  (require 'dash)
  (require 's))

(defvar lawvere-keywords nil
  "Lawvere language keywords.")
(defvar lawvere-builtins nil
  "Lawvere language keywords.")

(defun kw-alone (name)
  "A keyword eith name NAME, on its own."
  (concat "\\_<" name "\\_>"))

(setq lawvere-keywords
      (-map #'kw-alone
            '("ob"
              "ar"
              "interp"
              "sketch"
              "over"
              "handling"
              "curry"
              "summing"
              "if"
              "then"
              "else"
              "category"
              "effect_category"
              "effect"
              "interpret"
              "in")))

(setq lawvere-builtins
      (-map #'kw-alone
            '("identity"
              "app"
              "incr"
              "abs"
              "show"
              "plus"
              "minus"
              "mult"
              "equal"
              "less_than"
              "less_than_equal"
              "greater_than"
              "greater_than_equal"
              "const"
              "SumOb"
              "sumInj"
              "sumUni"
              "side")))

(defvar lawvere-font-lock
      `(("//.*$" . font-lock-comment-face)
        ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
        (,(s-join "\\|" lawvere-keywords) . font-lock-keyword-face)
        (,(s-join "\\|" lawvere-builtins) . font-lock-builtin-face)
        ("[a-z][A-Za-z0-9]*\\." . font-lock-constant-face)
        ("\\<\\*?\\[?[A-Z][A-Za-z]*\\]?\\>" . font-lock-type-face)
        ("\\w\\(#\\w+\\)" . (1 font-lock-builtin-face))))


;;;###autoload
(define-derived-mode lawvere-mode prog-mode "Lawvere"
  (setq-local font-lock-defaults '(lawvere-font-lock t nil nil))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "// ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.law\\'" . lawvere-mode))

(defun lawvere--disable-highlight-numbers ()
  "The highlight-numbers package interferes with our syntax rules.
Disable it, if it's available."
  (if (featurep 'highlight-numbers)
      (highlight-numbers-mode -1)))

(add-hook 'lawvere-mode-hook 'lawvere--disable-highlight-numbers)

(provide 'lawvere-mode)
;;; lawvere-mode.el ends here
