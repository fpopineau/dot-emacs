;;; pseudocode.el --- Pseudocode major mode

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: StefanMonnier
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If needed: http://emacswiki.org/emacs/ModeTutorial

;;

;;; Code:

(require 'pascal)

(defvar pseudocode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `pseudocode-mode'.")

(defvar pseudocode-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# ". 1" st)
    (modify-syntax-entry ?c ". 2" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `pseudocode-mode'.")

(defvar pseudocode-font-lock-keywords
  '(("\\<\\(fun\\|function\\|end\\|for\\|to\\|downto\\|while\\|if\\|else\\|return\\|and\\|or\\|not\\)\\>" (1 font-lock-function-name-face)))
  "Keyword highlighting pseudocodeication for `pseudocode-mode'.")

(setq pseudocode-font-lock-keywords pascal-font-lock-keywords)

(defvar pseudocode-imenu-generic-expression
  "")

(defvar pseudocode-outline-regexp
  "")

;;;###autoload
(define-derived-mode pseudocode-mode fundamental-mode "Pseudocode"
  "A major mode for editing Pseudocode files."
  :syntax-table pseudocode-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
       '(pseudocode-font-lock-keywords))
  (setq-local indent-line-function 'pseudocode-indent-line)
  (setq-local imenu-generic-expression
       pseudocode-imenu-generic-expression)
  (setq-local outline-regexp pseudocode-outline-regexp))


;;; Indentation

;; If needed: http://www.linta.de/~aehlig/techblog/2012-05-10.html

(defvar pseudocode-indent 4
  "Standard indentation for pseudo code")

(defvar pseudocode-emptyline
  "^\\s-*$"
  "Regular expression matching an empty line")

(defvar pseudocode-opening
  "^\\s-*\\(fun\\|if\\|else\\)"
  "Regular expression matching lines starting an indentation level in pseudo code")

(defvar pseudocode-closing
  "^\\s-*\\(nuf\\|fi\\|else\\|end\\)"
  "Regular expression matching lines ending an indentation level in pseudo code")

(defun pseudocode-indent-line ()
  "Indent current line according to pseudo code style"
  (interactive)
  (save-excursion
    (indent-line-to
     (max
      0
      (catch 'indent
        (save-excursion
          (beginning-of-line)
          (if (bobp) (throw 'indent 0))
          (let ((outdent (if (looking-at pseudocode-closing)
                             (- pseudocode-indent)
                           0)))
            (forward-line -1)
            (while (looking-at pseudocode-emptyline)
              (if (bobp) (throw 'indent 0))
              (forward-line -1))
            (if (looking-at pseudocode-opening)
                (throw 'indent (+ (current-indentation) pseudocode-indent outdent)))
            (throw 'indent (+ (current-indentation) outdent)))))))))



(defvar pseudocode-mode-hook nil)

(add-hook 'pseudocode-mode-hook
          (lambda () (setq prettify-symbols-alist
                           '(("lambda" . 955)
                             ("<-" . ?←) ; 8592
                             ("<=" . 8656)
                             ("->" . 8594)
                             ("=>" . 8658)
                             ("!=" . ?≠)
                             (".leq." . ?≤)
                             (".geq." . ?≥)
                             ("_1" . ?₁)
                             ("_2" . ?₂)
                             ("_3" . ?₃)
                             (".cap." . ?⋂)
                             (".cup." . ?⋃)
                             (".and." . ?∧)
                             (".or." . ?∨)
                             (".not." . ?¬)))))

(push '("\\.pseudocode$" . pseudocode-mode) auto-mode-alist)

(provide 'fp-pseudocode)
;;; pseudocode.el ends here
