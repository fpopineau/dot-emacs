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
        ("PDF Viewer" "c:/Local/SumatraPDF/SumatraPDF.exe -reuse-instance %o")
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
        '("." "c:/Local/SumatraPDF/SumatraPDF.exe -reuse-instance %o"))

(setq TeX-engine 'luatex)

(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-list
      '(("SumatraPDF" ("\"c:/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                        (mode-io-correlate " -forward-search %b %n ") " %o"))))

(eval-after-load 'tex
  '(progn
     (assq-delete-all 'output-pdf TeX-view-program-selection)
     (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

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

(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
                 (TeX-command-expand "latexmk -pvc -lualatex %t" 'TeX-master-file)
                 master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (minibuffer-message "latexmk done"))))

;; TODO: chose some nice key
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-0") #'run-latexmk))
          )

;; TODO: solve the problem of AUC-TeX wrongly parsing headers printed
;; by Web2C.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LATEXMK START-PROCESS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/q/18705774/2112489
;; https://github.com/lawlist/emacs-latexmk

(defvar tex-file nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'tex-file)

(defvar pdf-file nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'pdf-file)

(defvar base-file nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'base-file)

(defvar w32-tex-file nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'w32-tex-file)

(defvar w32-pdf-file nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'w32-pdf-file)

(defvar line nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'line)

(defvar sumatra nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'sumatra)

(defvar skim nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'skim)

(defvar w32-document nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'w32-document)

(defvar tex-output nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'tex-output)

(defvar latexmk nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'latexmk)

(defvar latexmkrc nil
 "Local variable for latexmk function.")
(make-variable-buffer-local 'latexmkrc)

(defun xp-latexmk ()
  ".latexmkrc contains the following entries:
  $pdflatex = 'pdflatex -file-line-error -synctex=1 %O %S';
  $pdf_mode = 1;
  $recorder = 0;
  $clean_ext = 'synctex.gz synctex.gz(busy) aux fdb_latexmk log';"
  (interactive)
  (setq tex-file (buffer-file-name))
  (setq base-file (car (split-string (buffer-file-name) "\\.tex")))
  (setq w32-tex-file (concat "\"" (buffer-file-name) "\""))
  (setq w32-pdf-file (concat "\"" base-file ".pdf" "\""))
  (setq line (format "%d" (line-number-at-pos)))
  (setq sumatra "c:/Program Files/SumatraPDF/SumatraPDF.exe")
  (setq w32-document
        (concat "-forward-search " w32-tex-file " " line " " w32-pdf-file))
  (setq tex-output (concat "*" (file-name-nondirectory buffer-file-name) "*") )
  (setq latexmk "c:/texlive/2013/bin/win32/latexmk.exe")
  (setq latexmkrc "y:/.0.emacs/.latexmkrc-nt")
  (if (buffer-modified-p)
    (save-buffer))
  (delete-other-windows)
  (set-window-buffer (split-window-horizontally) (get-buffer-create tex-output))
  (with-current-buffer tex-output (erase-buffer))
  (start-process "tskill" nil "c:/WINDOWS/system32/tskill.exe" "SumatraPDF")
  (set-process-sentinel
    (start-process "deep-clean" nil latexmk "-C" "-r" latexmkrc tex-file)
    (lambda (p e) (when (= 0 (process-exit-status p))
      (set-process-sentinel
        (start-process "compile" tex-output latexmk "-r" latexmkrc tex-file)
        (lambda (p e) (when (= 0 (process-exit-status p))
          (if (get-buffer-process (get-buffer tex-output))
            (process-kill-without-query (get-buffer-process
              (get-buffer tex-output))))
          (kill-buffer tex-output)
          (delete-other-windows)
          (switch-to-buffer (get-file-buffer tex-file))
          ;; (start-process "displayline" nil sumatra
          ;;   "-forward-search" tex-file line pdf-file)
          (w32-shell-execute "open" sumatra w32-document)
          (sit-for 1)
          (start-process "clean" nil latexmk "-c" "-r" latexmkrc tex-file))))))))

;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line
;; option -g opens Skim in the background
;; option -o open Skim in the foreground with full application focus.
(defun latexmk ()
  ".latexmkrc contains the following entries (WITHOUT the four backslashes):
  $pdflatex = 'pdflatex -file-line-error -synctex=1 %O %S && (cp \"%D\" \"%R.pdf\")';
  $pdf_mode = 1;
  $out_dir = '/tmp';"
(interactive)
  (setq tex-file buffer-file-name)
  (setq pdf-file (concat "/tmp/"
    (car (split-string (file-name-nondirectory buffer-file-name) "\\.tex")) ".pdf"))
  (setq line (format "%d" (line-number-at-pos)))
  (setq skim "/Applications/Skim.app/Contents/SharedSupport/displayline")
  (setq tex-output (concat "*" (file-name-nondirectory buffer-file-name) "*") )
  (setq latexmk "/usr/local/texlive/2012/texmf-dist/scripts/latexmk/latexmk.pl")
  (setq latexmkrc "/Users/HOME/.0.data/.0.emacs/.latexmkrc")
  (setq exit-skim-script "tell application \"skim\" to quit")
  (if (buffer-modified-p)
    (save-buffer))
  (delete-other-windows)
  (set-window-buffer (split-window-horizontally) (get-buffer-create tex-output))
  (with-current-buffer tex-output (erase-buffer))
  (set-process-sentinel
    (start-process "exit-skim" nil "osascript" "-e" exit-skim-script)
    (lambda (p e) (when (= 0 (process-exit-status p))
      (set-process-sentinel
        (start-process "deep-clean" nil latexmk "-C" "-r" latexmkrc tex-file)
        (lambda (p e) (when (= 0 (process-exit-status p))
          (set-process-sentinel
            (start-process "compile" tex-output latexmk "-r" latexmkrc tex-file)
            (lambda (p e) (when (= 0 (process-exit-status p))
              (set-process-sentinel
                ;; (start-process "displayline" nil skim "-b" line pdf-file tex-file)
                (start-process "displayline" nil skim line pdf-file tex-file)
                  (lambda (p e) (when (= 0 (process-exit-status p))
                    (switch-to-buffer (get-file-buffer tex-file))
                    (if (get-buffer-process (get-buffer tex-output))
                      (process-kill-without-query (get-buffer-process
                        (get-buffer tex-output))))
                    (kill-buffer tex-output)
                    (delete-other-windows))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start RefTeX
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(provide 'fp-tex)
