;;; specif.el --- Specif major mode

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

;;

;;; Code:

(defvar specif-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `specif-mode'.")

(defvar specif-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `specif-mode'.")

(defvar specif-font-lock-keywords
  '(("\\<\\(Spec\\|EndSpec\\|Operations\\|Preconditions\\|Axioms\\)\\>" (1 font-lock-function-name-face)))
  "Keyword highlighting specification for `specif-mode'.")

(defvar specif-imenu-generic-expression
  "")

(defvar specif-outline-regexp
  "")

;;;###autoload
(define-derived-mode specif-mode fundamental-mode "Specif"
  "A major mode for editing Specif files."
  :syntax-table specif-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
       '(specif-font-lock-keywords))
  (setq-local indent-line-function 'specif-indent-line)
  (setq-local imenu-generic-expression
       specif-imenu-generic-expression)
  (setq-local outline-regexp specif-outline-regexp))


;;; Indentation

(defun specif-indent-line ()
  "Indent current line of Specif code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (specif-calculate-indentation) 0)
           (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun specif-calculate-indentation ()
  "Return the column to which the current line should be indented."
  0)

(defvar specif-mode-hook nil)

(defun fp-specif-replace-symbols ()
  (mapcar
   #'(lambda (pair)
       (save-mark-and-excursion
        (save-restriction
          (when (region-active-p)
            (narrow-to-region (region-beginning)
                              (region-end)))
          (goto-char (point-min))
          (while (re-search-forward (concat "[^\\w]\\(" (regexp-quote (car pair)) "\\)[^\\w]") (point-max)  t)
            (replace-match (char-to-string (cdr pair)) t t nil 1))
          (widen))))

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
     (".not." . ?¬)
     (".in." . #x2208 )
     (".notin." . #x2209)
     (".infinite." . #x221E)
     (".emptyset." . #x2205 ))))

(push '("\\.specif$" . specif-mode) auto-mode-alist)

(provide 'fp-specif)
;;; specif.el ends here
