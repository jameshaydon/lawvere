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
              "freyd"
              "summing"
              "side")))

(setq lawvere-builtins
      (-map #'kw-alone
            '("const"
              "curry"
              "i")))

(defvar lawvere-font-lock
      `(("//.*$" . font-lock-comment-face)
        ("\"\\(\\(\\\\\"\\)\\|[^\"]\\)*\"" . font-lock-string-face)
        (,(s-join "\\|" lawvere-keywords) . font-lock-keyword-face)
        (,(s-join "\\|" lawvere-builtins) . font-lock-builtin-face)
        ("\\<\\*?\\[?[A-Z][a-z]*\\]?\\>" . font-lock-type-face)
        ("\\w\\(#\\w+\\)" . (1 font-lock-builtin-face))))


;;;###autoload
(define-derived-mode lawvere-mode prog-mode "Lawvere"
  (setq-local font-lock-defaults '(lawvere-font-lock t nil nil))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "// ")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.law\\'" . lawvere-mode))

(defun art--disable-highlight-numbers ()
  "The highlight-numbers package interferes with our syntax rules.
Disable it, if it's available."
  (if (featurep 'highlight-numbers)
      (highlight-numbers-mode -1)))

(add-hook 'art-mode-hook 'art--disable-highlight-numbers)

(provide 'lawvere-mode)
;;; lawvere-mode.el ends here
