;;; fp-pre.el --- Emacs Prelude: Fabrice Popineau preload configuration.
;;
;; Copyright © 2014 Fabrice Popineau
;;
;; Author: Fabrice Popineau <fabrice.popineau@gmail.com>
;; URL: https://github.com/fpopineau/.emacs.d
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))

;; this section is good for OS X only
;; TODO add sensible defaults for Linux/Windows
(setq TeX-view-program-list
      '(("DVI Viewer" "open %o")
        ("PDF Viewer" "open %o")
        ("HTML Viewer" "open %o")))

(defun fp-config-latex-mode-defaults ()
  (turn-on-auto-fill)
  (abbrev-mode +1))

(setq fp-config-latex-mode-hook 'fp-config-latex-mode-defaults)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (run-hooks 'fp-config-latex-mode-hook)))

;;; fp-config-latex.el ends here
;;; AUC-TeX
;;; auctex
;; (add-to-list 'load-path
;;   (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../elpa/auctex-11.86/")))
(require 'latex)
(require 'tex-site)
(require 'tex-mik)

;; (autoload 'TeX-load-hack
;;   (expand-file-name "../tex-site.el" (file-name-directory load-file-name)))
;; (TeX-load-hack)

(put 'LaTeX-command 'safe-local-variable 't)

;; (add-to-list 'load-path
;;  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../elpa/sumatra-forward-2008.10.8/")))
(require 'sumatra-forward)

;; Set the default PDF reader to SumatraPDF. The executable should be in PATH

(setcdr (assoc "^pdf$" TeX-output-view-style)
        '("." "SumatraPDF.exe -reuse-instance %o"))

(setq TeX-engine 'luatex)
(setq TeX-view-program-list '(("SumatraPDF" "SumatraPDF.exe -reuse-instance %o")))
(setq TeX-view-program-selection '((output-pdf "SumatraPDF")
                                   (output-dvi "Yap")
                                   (output-html "start"))
      )
(setcdr (assoc "LaTeX" TeX-command-list)
        '("%l -file-line-error %(mode) \"%t\"" TeX-run-TeX nil
          (latex-mode doctex-mode)
          :help "Run LaTeX"))

(setq TeX-command-list
      '(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
        (plain-tex-mode ams-tex-mode texinfo-mode)
        :help "Run plain TeX")
        ("MyTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
         (latex-mode doctex-mode)
         :help "Run LaTeX")
        ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
         (latex-mode doctex-mode)
         :help "Run LaTeX")
        ("Makeinfo" "makeinfo %t" TeX-run-compile nil
         (texinfo-mode)
         :help "Run Makeinfo with Info output")
        ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
         (texinfo-mode)
         :help "Run Makeinfo with HTML output")
        ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
         (ams-tex-mode)
         :help "Run AMSTeX")
        ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
         (context-mode)
         :help "Run ConTeXt once")
        ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
         (context-mode)
         :help "Run ConTeXt until completion")
        ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
        ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
        ("Print" "%p" TeX-run-command t t :help "Print the file")
        ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
        ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
        ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
        ("Check" "lacheck %s" TeX-run-compile nil
         (latex-mode)
         :help "Check LaTeX file for correctness")
        ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
        ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
        ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
        ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))

;; Some helpful settings
(setq TeX-auto-untabify t)
; (setq TeX-electric-escape t)
(setq TeX-save-query nil) ;;autosave before compiling
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master t)
(setq-default TeX-PDF-mode t)
(setq-default TeX-engine 'luatex)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-newline-function 'newline-and-indent)
(setq TeX-show-compilation nil)
(setq TeX-auto-global (expand-file-name
          "auctex-auto-generated-info/" temporary-file-directory))
(setq TeX-auto-local (expand-file-name
          "auctex-auto-generated-info/" temporary-file-directory))
;(defun my-tex-mode-hook ()
;  (local-set-key (kbd "") (kbd "C-c C-c C-j")))
(add-hook 'TeX-mode-hook
          #'(lambda ()
              (TeX-source-correlate-mode)
              (local-set-key (kbd "<f5>") (kbd "C-u C-c C-c"))
              (local-set-key (kbd "<f6>") #'TeX-next-error)))

(add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
(add-to-list 'LaTeX-verbatim-environments "comment")

;; preview
;(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
;    (autoload 'LaTeX-preview-setup "preview")

;; how to call gs for conversion from EPS
;(setq preview-gs-command
;      (cond (running-ms-windows
;            "C:/Program Files/gs/gs8.64/bin/gswin32c.exe")
;           (t
;            "/usr/bin/gs")))
;(my-file-executable-p preview-gs-command)

;; scale factor for included previews
;(setq preview-scale-function 1.2)

;; Start RefTeX
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(provide 'fp-tex)
