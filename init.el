;;; init.el --- FP Emacs initialization file
;;
;; Copyright © 2016 Fabrice Popineau
;;
;; Author: Fabrice Popineau <fabrice.popineau@gmail.com>
;; URL: https://github.com/fpopineau/emacs-init
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

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

;; * Startup

(require 'edebug)

;; ** Environment

(when (eq system-type 'windows-nt)
;;  (setenv "TEMP" "c:/Temp/")
;;  (setenv "temp" "c:/Temp/")
;;  (setenv "TMP" "c:/Temp/")
;;  (setenv "TMPDIR" "c:/Temp/")
  (setenv "HOME" "c:/Home/")
  (setenv "SHELL" (expand-file-name "cmdproxy.exe" exec-directory))
;; PATH=/MingW64/bin:/c/Local/Miniconda3:/c/Local/Miniconda3/Scripts:/c/Local/Miniconda3/Library/bin:/usr/bin:/c/Local/TeXLive/bin/win32:/c/Local/CCL/current:/c/Local/SBCL/current:/c/Local/SumatraPDF:/c/Local/SwiProlog/bin:/c/Local/BibTeX2HTML:/c/Local/CUDA/bin:/c/Windows/System32:/c/Windows:/c/Windows/System32/Wbem

  (setenv "PATH" (concat
                  "C:\\Local\\Anaconda3"
                  ";C:\\Local\\Anaconda3\\Scripts"
                  ";C:\\Local\\Anaconda3\\Library\\bin"
                  ";C:\\Local\\MSys64\\MingW64\\bin"
                  ";C:\\Local\\MSys64\\usr\\bin"
                  ";C:\\Local\\TeXLive\\bin\\win32"
                  ";C:\\Local\\Putty"
                  ";C:\\Local\\CCL\\current"
                  ";C:\\Local\\SBCL\\current"
                  ";C:\\Local\\SumatraPDF"
                  ";C:\\Local\\SwiProlog\\bin"
                  ";C:\\Local\\BibTeX2HTML"
                  ";C:\\Local\\CUDA\\bin"
                  ";C:\\Local\\Pandoc"
                  ";C:\\Windows\\System32"
                  ";C:\\Windows"
                  ";C:\\Windows\\System32\\Wbem"
                         ;; ";C:\\Windows\\System32\\WindowsPowerShell\\v1.0"
                         ;; ";C:\\Local\\Emacs\\bin"
                         ;;                         ";c:\\Source\\Gnu\\Orig\\w3m-0.5.3"
                         ;;                         ";C:\\Local\\Ruby193\\bin"
                         ;;                         ";C:\\Local\\Bazaar"
                         ;;                         ";C:\\Local\\Git\\bin"
                         ;;                         ";C:\\Local\\StrawBerry\\perl\\bin"
                         ;; ";C:\\Local\\ImageMagick-6.9.2-Q16\\"
                         ;; ";C:\\Local\\GhostScript\\GS9.15\\bin\\"
                         ;;                         ";C:\\Local\\Subversion\\bin"
                         ;; ";C:\\Local\\OpenSSL-Win64"
                         ;; ";c:\\Local\\OpenBLAS\\0.2.15\\bin"
                         ;; ";c:\\Local\\FreeGLUT\\bin\\x64"
                         ;; ";C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\bin\\amd64"
                         ))
  (setq exec-path (split-string (getenv "PATH") ";"))

  (setenv "PYTHONPATH" "c:/Home/.python")
  (setenv "TEXMFLOCAL" "$TEXMFROOT/texmf-local")
  (setenv "SBCL_HOME" "c:/Local/SBCL/current/")
  
  (setenv "OPENBLAS_NUM_THREADS" "4")
  
  ;; For TeXDocTk
  (setenv "PDFVIEWER" "c:/Local/SumatraPDF/Sumatrapdf.exe"))

(when (eq system-type 'gnu/linux)
  (require 'dbus)
  (dbus-init-bus :system))

;; ** Init dynamic libraries

(dolist (type '(xpm jpeg png tiff gif imagemagick svg))
  (init-image-library type))


;; ** Who am I?

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar fp-config-dir (file-name-directory (or load-file-name user-emacs-directory))
  "The root dir of the Emacs FP configuration.")

(defvar fp-config-savefile-dir (expand-file-name "savefile" fp-config-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p fp-config-savefile-dir)
  (make-directory fp-config-savefile-dir))

;; ** setup packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" fp-config-dir))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(use-package diminish                ;; if you use :diminish
  :ensure t)
(use-package bind-key                ;; if you use any :bind variant
  :ensure t)


(setq emacs-debug nil)
(setq debug-on-error t)

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)

(if (eq system-type 'windows-nt)
    ;; For Windows
    (setq default-process-coding-system '(cp1252-dos . cp1252-unix))
  ;; For Unix
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

;; Add coding systems for process that may require specific settings
(add-to-list 'process-coding-system-alist '("[pP][aA][nN][dD][oO][cC]" utf-8 . utf-8))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

;; FIXME
(setq enable-local-variables :all)

;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; It seems that Windows 8.1 has made it very difficult to catch LWin and RWin keys
;; from an application. It seems the following AutoHotkey_L script
;; #IfWinActive ahk_exe emacs.exe
;;   CapsLock::LCtrl
;;   <+CapsLock::CapsLock
;;   LCtrl::AppsKey
;;   LWin::LWin
;; #IfWinActive
;; does what is needed to get hyper and super keys.
;; Make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.

(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil
	w32-pass-rwindow-to-system nil
	w32-pass-apps-to-system nil
	w32-lwindow-modifier 'hyper ; Left Windows key
	w32-rwindow-modifier 'hyper ; Right Windows key
	w32-apps-modifier 'super) ; Menu keyemacs lisp
  
  (w32-register-hot-key [s-])
  (w32-register-hot-key [h-])
  
  ;; Win32 !
  ;; As of Emacs 24.4, unicode filenames under NT should be honoured
  ;; (set-file-name-coding-system 'latin-1)
  ;; MS Windows clipboard is UTF-16LE
  (set-clipboard-coding-system 'utf-16le-dos)
  
  ;; Fix a problem where it takes forever to read
  ;; the output of a process.
  (setq  w32-pipe-read-delay 0)
  )

;; * Editor

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; Kill ring
(setq save-interprogram-paste-before-kill t)

;; * Backups

;; (setq backup-by-copying t)
(setq backup-by-copying-when-linked t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" fp-config-savefile-dir))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)



;; * UI
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; menu-bar is still useful sometimes
(menu-bar-mode +1)

                                        ; wrap lines
(global-visual-line-mode)
(diminish 'visual-line-mode)

(setq-default indicate-empty-lines t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
;; disable scrollbars for windows
(scroll-bar-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; (when (eq system-type 'windows-nt)
;;   (add-to-list 'default-frame-alist `(font . "Consolas-10")))

(set-fringe-mode '(8 . 0))

;; * Keybindings

(use-package cua-base
  :config
  (setq cua-enable-cua-keys nil)
  (cua-mode +1)
)
;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
    (global-set-key (kbd "C-x p") 'proced))

;; * Fonts

(when (eq system-type 'windows-nt)
  (set-face-font 'default "Consolas-11")
  (set-face-font 'variable-pitch "Segoe UI-11")
  (set-face-font 'fixed-pitch "Consolas-11")
  (custom-set-variables '(line-spacing 0.1))
  )

(when (eq system-type 'gnu/linux)
  (set-face-font 'default "Consolas-17")
  (set-face-font 'variable-pitch "Segoe UI-17")
  (set-face-font 'fixed-pitch "Consolas-17")
  (custom-set-variables '(line-spacing 0.2))
  )

;; ;; convert symbols like greek letter into its unicode character
(global-prettify-symbols-mode)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Segoe UI Emoji Normal" nil 'prepend))

(setq inhibit-compacting-font-caches t)


;; * Setup local packages
;; Fix load-path for those packages we mirror
(require 'cl)

(defvar *fp-config-packages-supplied-regexp*
  "\\(lisp/org\\|elpa/slime-[0-9\.]+\\|elpa/org-mode-[0-9\.]+\\|elpa/org-ref-[0-9\.]+\\|elpa/org-reveal-[0-9\.]+\\|elpa/undo-tree-[0-9\.]+\\|elpa/ivy-[0-9\.]+\\|elpa/counsel-[0-9\.]+\\|elpa/swiper-[0-9\.]+\\)")

(defvar *fp-local-packages-directory* (expand-file-name "local" user-emacs-directory))

;; Remove packages maintained locally from load-path
(setq load-path
      (remove-if '(lambda (s) (string-match *fp-config-packages-supplied-regexp* s) ) load-path))

;; Add the local packages to load-path
(mapcar #'(lambda (package)
            (add-to-list 'load-path (expand-file-name package *fp-local-packages-directory*)))
	'(
          ;; "magit/lisp"
	  "el-misc"
          "font-lock-plus"
          "org-mode/lisp"
          "org-board"
	  "org-html-themes"
          "org-protocol-capture-html"
          "org-ref"
          "org-reveal"
          "ox-ipynb"
          ;; "pdf-tools/pdf-tools-0.70"
	  "ppd-sr-speedbar"
          "spaceline"
          ;; "sly"
          "slime"
          ;; "swiper"
          "sumatra-forward"
          ;; "tablist"
          "undo-tree"
          ))

;; http://emacs.stackexchange.com/a/26513/115
(defun modi/package-dependency-check-ignore (orig-ret)
  "Remove the `black listed packages' from ORIG-RET.
Packages listed in the let-bound `pkg-black-list' will not be auto-installed
even if they are found as dependencies.
It is known that this advice is not effective when installed packages
asynchronously using `paradox'. Below is effective on synchronous
package installations."
  (let ((pkg-black-list '(org slime swiper undo-tree org-ref org-reveal))
        new-ret
        pkg-name)
    (dolist (pkg-struct orig-ret)
      (setq pkg-name (package-desc-name pkg-struct))
      (if (member pkg-name pkg-black-list)
          (message (concat "Package `%s' will not be installed. "
                           "See `modi/package-dependency-check-ignore'.")
                   pkg-name)
        ;; (message "Package to be installed: %s" pkg-name)
        (push pkg-struct new-ret)))
    new-ret))
(advice-add 'package-compute-transaction :filter-return #'modi/package-dependency-check-ignore)


;; * Fixes
;; ** Remove ad-handle message

(setq ad-redefinition-action 'accept)
;; ** minibuffer

;; Minibuffer window expands vertically as necessary to hold the text that you put in the minibuffer

(setq resize-mini-windows t) ;; was grow-only

;; ** Emacs Lisp Mode

;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)

(defun lisp-indent-function-kw (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(eval-after-load "lisp-mode"
  (lambda ()
    (setq lisp-indent-function 'lisp-indent-function-kw)))


;; ** Doc View

(setq doc-view-odf->pdf-converter-program "c:/Program Files/LibreOffice 5/program/soffice.exe")
(setq doc-view-odf->pdf-converter-function #'doc-view-odf->pdf-converter-soffice)

;; * Utilities

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))



;; * ELPA Packages
;; ** custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; ** abbrev
(use-package abbrev
  :ensure nil
  :pin manual
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" fp-config-savefile-dir))
  ;; abbrev config
  (add-hook 'text-mode-hook 'abbrev-mode)

  )

;; ** ace-window
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?q ?s ?d ?f ?j ?k ?l ?m))
  (ace-window-display-mode)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  :bind (("C-o " . ace-window)
         ("s-w" . ace-window)))


;; ** ag
(use-package ag
  :ensure t)

;; ** all-the-icons
(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons))
(use-package all-the-icons
  :ensure t
  :config
  (require 'font-lock+)
  )

;; ** auto-complete
(use-package auto-complete
  :ensure t
  :config
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" fp-config-savefile-dir)))


;; ** auto-revert
;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :ensure nil
  :pin manual
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  (setq auto-revert-use-notify t)
  ;; Possibly set files to ignore
  ;; (setq global-auto-revert-ignore-modes ())
  (global-auto-revert-mode t)
  )

;; ** avy
(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  :diminish avy-mode
  :bind (("C-x C-SPC" . avy-goto-char)
         ;; ("C-x C-x" . avy-goto-word-or-subword-1)
         ("C-c j" . avy-goto-word-or-subword-1)
         ("s-." . avy-goto-word-or-subword-1)
         ("C-x C-l" . avy-goto-line)))

;; ** anzu
;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

;; ** beacon
;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode +1)
;;   )

;; ** bookmark
(use-package bookmark
  :ensure nil
  :pin manual
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" fp-config-savefile-dir)
        bookmark-save-flag 1)
  )

;; ** bookmark+
;; (use-package bookmark+)


;; ** browse-kill-ring
;; smarter kill-ring navigation
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings)
  (global-set-key (kbd "s-y") 'browse-kill-ring)
  )

;; ** company mode
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 3)
  (delete 'company-capf company-backends)
  (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-statistics
  :ensure t
  :config
  (setq company-statistics-file
        (expand-file-name "company-statistics-file" fp-config-savefile-dir)))

;; ** crux
(use-package crux
  :bind (([remap kill-whole-line] . crux-kill-whole-line)
         ("C-^" . crux-top-join-line))
  :ensure t)


;; ** dash
(use-package dash
  :ensure t)


;; ** dictionary
(use-package dictionary
  :ensure t)


;; ** diff-hl
;; (use-package diff-hl
;;   :ensure t
;;   :config
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   (global-diff-hl-mode +1)
;;   )

;; ** dired
(use-package dired
  :ensure nil
  :pin manual
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; Don't use any ls.exe
  (setq ls-lisp-use-insert-directory-program nil)
  ;; Sort the same way as Unix ls does
  (setq ls-lisp-UCA-like-collation nil)
  ;; Use human readable sizes
  (setq dired-listing-switches "-alh")

  (setq dired-guess-shell-alist-user
        '(; ("\\.pdf\\'" "start")
          ;; ("\\.pdf\\'" "c:/Local/SumatraPDF/SumatraPDF -reuse-instance")
          ("\\.pdf\\'" "start")
          ("\\.zip\\'" "start")
          ("\\.docx?\\'" "start")
          ("\\.xlsx?\\'" "start")
          ("\\.pptx?\\'" "start")
          ("\\.jpe?g\\'" "start")
          ("\\.png\\'" "start")
          ("\\.bmp\\'" "start")
          ("\\.html\\'" "start")
          ))

  (defadvice dired-shell-stuff-it (around dired-execute-start-command)
    "Use w32-shell-execute to execute `start' commands."
    (if (string= (ad-get-arg 0) "start")
        (dolist (file (ad-get-arg 1))
          (w32-shell-execute nil (expand-file-name file default-directory)))
      ad-do-it))

  (ad-activate 'dired-shell-stuff-it)

  ;; Dired invokes dired-run-shell-command anyway, even when
  ;; the command as already been processed by advising
  ;; dired-shell-stuff-it. In this case the command is null.
  ;; We want to avoid any error message.

  (defadvice dired-run-shell-command (around dired-ignore-null-command)
    "Ignore null command in dired-run-shell-command."
    (when (ad-get-arg 0)
      ad-do-it))

  (ad-activate 'dired-run-shell-command)
  ;;  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  (defun copy-buffer-file-name-as-kill (choice)
    "Copy the buffer-file-name to the kill-ring"
    (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
    (let ((new-kill-string)
          (name (if (eq major-mode 'dired-mode)
                    (dired-get-filename)
                  (or (buffer-file-name) ""))))
      (cond ((eq choice ?f)
             (setq new-kill-string name))
            ((eq choice ?d)
             (setq new-kill-string (file-name-directory name)))
            ((eq choice ?n)
             (setq new-kill-string (file-name-nondirectory name)))
            (t (message "Quit")))
      (when new-kill-string
        (message "%s copied" new-kill-string)
        (kill-new new-kill-string))))

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )


;; ** dired+
(use-package dired+
  :ensure t
  :config
  (global-dired-hide-details-mode -1)
  )

;; ** dired-x
;; enable some really cool extensions like C-x C-j (dired-jump)
(use-package dired-x
  :ensure nil
  :pin manual)

;; ** discover-my-major
(use-package discover-my-major
  :ensure t)

;; ** doc-view
;; (use-package doc-view
;;   :config
;;   (add-hook 'doc-view-mode-hook (lambda () (centered-cursor-mode -1)))
;;   (define-key doc-view-mode-map (kbd "<right>") 'doc-view-next-page)
;;   (define-key doc-view-mode-map (kbd "<left>") 'doc-view-previous-page)
;;   (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;   (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;   (global-set-key (kbd "C-<wheel-up>") 'doc-view-enlarge)
;;   (global-set-key (kbd "C-<wheel-down>") 'doc-view-shrink)

;;   (setq doc-view-continuous t))

;; ** easy-kill
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  )

;; ** ediff
(use-package ediff
  :ensure nil
  :pin manual
  :config
  (ediff-set-diff-options 'ediff-diff-options "--strip-trailing-cr")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

;; ** epl
(use-package epl
  :ensure t)

;; ** expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region))
  :ensure t)

;; ** eyebrowse
;; (use-package eyebrowse
;;   :diminish eyebrowse-mode
;;   :config (progn
;;             (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;;             (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;;             (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;;             (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;;             (define-key eyebrowse-mode-map (kbd "S-s-<right>") 'eyebrowse-next-window-config)
;;             (define-key eyebrowse-mode-map (kbd "S-s-<left>") 'eyebrowse-prev-window-config)
;;             (setq eyebrowse-new-workspace t)))

;; ** flycheck
(use-package flycheck
  :ensure t)

;; ** flyspell
;; flyspell-mode does spell-checking on the fly as you type
(use-package flyspell
  :ensure t
  :config
  (setenv "LANG" "en_US")
  ;; (setenv "DICPATH" (concat ".;" (expand-file-name "../etc/hunspell" exec-directory))) ;;
  (setenv "DICPATH" "c:/Local/MSys64/mingw64/share/hunspell") ;;
  (setq ispell-program-name "hunspell.exe"
        ispell-dictionary "en_US"
        ispell-extra-args '("--sug-mode=ultra"))
  )

;; ** framemove
;; DEPRECATED, not available on ELPA anymore
;; (use-package framemove
;;   :ensure t
;;   :config
;;   ;; FIXME: does this load windmove?
;;   (setq framemove-hook-into-windmove t)
;;   )

;; ** gist
(use-package gist
  :ensure t)

;; ** git-timemachine
(use-package git-timemachine
  :ensure t)

;; ** gitconfig-mode
(use-package gitconfig-mode
  :ensure t)

;; ** gitignore-mode
(use-package gitignore-mode
  :ensure t)

;; ** helpful
(use-package helpful
  :ensure t)

;; ** FIXME git-gutter
;; (use-package git-gutter
;;   :ensure t
;;   :config
;;   ;; If you enable global minor mode
;;   ;; (global-git-gutter-mode t)

;;   ;; If you would like to use git-gutter.el and linum-mode
;;   (git-gutter:linum-setup)
  
;;   ;; If you enable git-gutter-mode for some modes
;;   ;; (add-hook 'ruby-mode-hook 'git-gutter-mode)
;;   ;; (add-hook 'python-mode-hook 'git-gutter-mode)
  
;;   (global-set-key (kbd "C-x C-g") 'git-gutter)
;;   (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  
;;   ;; Jump to next/previous hunk
;;   (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;;   (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  
;;   ;; Stage current hunk
;;   (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  
;;   ;; Revert current hunk
;;   (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
  
;;   ;; Mark current hunk
;;   (global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
;;   )

;; ** god-mode
(use-package god-mode
  :ensure t)

;; ** grizzl
;; (use-package grizzl
;;   :ensure t)

;; ** guru-mode
(use-package guru-mode
  :ensure t)

;; ** highlight-symbol
(use-package highlight-symbol
  :ensure t)

;; ** hippie-exp
;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp
  :ensure t
  :bind (("M-/" . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

;; ** hl-line
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode +1))

;; ** htmlize
(use-package htmlize
  :ensure t)

;; ** hungry-delete

;;; deletes all the whitespace when you hit backspace or delete
;; (use-package hungry-delete
;;   :ensure t
;;   :config
;;   (global-hungry-delete-mode))

;; ** ido
(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-url-at-point nil
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" fp-config-savefile-dir)
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length 0
        ido-max-directory-size 100000
        ido-file-extensions-order '(".org" ".txt" "*.tex" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")
        )

  (ido-mode +1)
  (ido-everywhere +1) 
  ;; ** ido-ubiquitous
  (use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode +1))
  
  ;; ** ido-vertical-mode
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode +1))
  
  ;; ** flx-ido
;;; smarter fuzzy matching for ido
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode +1)
    ;; disable ido faces to see flx highlights
    (setq ido-use-faces nil)
    )
  
  ;; ** ido-hacks
  ;; (use-package ido-hacks
  ;;   :ensure t)
  )

;; ** ivy/counsel/swiper
;; (use-package flx :ensure t)
;; (use-package ivy
;;   :ensure t
;;   :diminish (ivy-mode)
;;   :bind (("C-x b" . ivy-switch-buffer))
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-display-style 'fancy))
;; (use-package counsel
;;   :ensure t
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-h v" . counsel-describe-variable)
;;          ("C-h f" . counsel-describe-function))
;;   :config
;;   ;; miz fuzzy with plus (.* for each space)
;;   ;; http://oremacs.com/2016/01/06/ivy-flx/
;;   (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))
;;   (setq ivy-initial-inputs-alist nil))
;; (use-package swiper
;;   :ensure t
;;   :bind (("C-s" . swiper)
;;          ("C-c u" . swiper-all)))

;; ** image-dired
(use-package image-dired
  :ensure t
  :config
  (setq image-dired-dir (expand-file-name "image-dired/" temporary-file-directory)))

;; ** imenu-anywhere
(use-package imenu-anywhere
  :ensure t)

;; ** js2-mode
(use-package js2-mode
  :ensure t)

;; ** key chord
(use-package key-chord
  :ensure t
  :after (org tex-site)
  ;; :defer 10
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)
  ;; (key-chord-define-global "]]" "\\")
  ;; (key-chord-define-global ";;" "/")
  ;; (key-chord-define-global "::" "?")
  ;; (key-chord-define-global "}}" "|")
  )

;; ** langtool
(use-package langtool
  :ensure t
  :config
  (setq langtool-language-tool-jar "c:/Local/LanguageTool-3.4/languagetool-commandline.jar"
        langtool-java-bin "c:/Program Files/Java/jre1.8.0_121/bin/java.exe"))
;; ** lua-mode
(use-package lua-mode
  :ensure t)

;; ** magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (if ido-everywhere
      (setq magit-completing-read-function 'magit-ido-completing-read))
  
  ;;This setting is needed to use ivy completion:
  ;; (setq magit-completing-read-function 'ivy-completing-read)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (defadvice magit-expand-git-file-name (around magit-expand-git-file-name-around)
    "Fix MSYS2 pathnames"
    (setq ad-return-value
          (replace-regexp-in-string  "^\\([c-z]\\):/\\1/" "\\1:/" ad-do-it))
    )
  (ad-activate 'magit-expand-git-file-name)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

;; ** magithub
(use-package magithub
  :after magit
  :config
  (setq magithub-dir (expand-file-name "magithub" fp-config-savefile-dir))
  (magithub-feature-autoinject t))

;; ** markdown-mode
;;(use-package markdown-mode)

;; ** markdown-mode+
;;(use-package markdown-mode+)

;; ** memory-usage
(use-package memory-usage
  :ensure t)

;; ** midnight
(use-package midnight
  ;; clean up obsolete buffers automatically
  :ensure t)

;; ** move-text
(use-package move-text
  :ensure t)

;; ** nlinum
;; (use-package nlinum
;;   :ensure t
;;   :config
;;   ;; Linum format to avoid graphics glitches in fringe
;;   (setq nlinum-format " %4d ")
;;   )

;; ** oauth2

(use-package oauth2
  :ensure t
  :init
  (defvar oauth--token-data)
  (defvar url-http-method)
  (defvar url-http-data)
  (defvar url-http-extra-headers)
  (defvar url-callback-function)
  (defvar url-callback-arguments)
  :config
  (setq oauth2-token-file (expand-file-name "oauth2.plstore" fp-config-savefile-dir)))


;; ** operate-on-number
(use-package operate-on-number
  :ensure t)

;; ** outshine
(use-package outshine
  :ensure t
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'lisp-mode-hook 'outline-minor-mode)
  ;; (add-hook 'clojure-mode-hook 'outline-minor-mode)
  ;; (add-hook 'ess-mode-hook 'outline-minor-mode)
  ;; (add-hook 'ledger-mode-hook 'outline-minor-mode)
  (add-hook 'message-mode-hook 'outline-minor-mode)
  (defvar outline-minor-mode-prefix "\M-#")
  (setq outshine-use-speed-commands t)

  ;; * Fix outshine version outshine-20141221.1805

  ;; (defun outline-hide-sublevels (keep-levels)
  ;;   "Hide everything except the first KEEP-LEVEL headers."
  ;;   (interactive "p")
  ;;   (if (< keep-levels 1)
  ;;       (error "Must keep at least one level of headers"))
  ;;   (setq keep-levels (1- keep-levels))
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     ;; Skip the prelude, if any.
  ;;     (unless (outline-on-heading-p t) (outline-next-heading))
  ;;     (hide-subtree)
  ;;     (show-children keep-levels)
  ;;     (condition-case err
  ;;         (while (outline-get-next-sibling)
  ;;           (hide-subtree)
  ;;           (show-children keep-levels))
  ;;       (error nil))))
  )


;; ** ov
(use-package ov
  :ensure t)

;; ** page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode 1))

;; ** pcache
(use-package pcache
  :ensure t
  :config
  (setq pcache-directory
        (let ((dir (locate-user-emacs-file "var/pcache/")))
          (make-directory dir t)
          dir)))

;; ** pdf-tools

;; wrapper for save-buffer ignoring arguments
(defun fp/save-buffer-no-args ()
  "Save buffer ignoring arguments"
  (save-buffer))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind (("C-c C-g" . pdf-sync-forward-search)
         :map pdf-view-mode-map
         ("M-w" . pdf-view-kill-ring-save)
         ([C-insert] . pdf-view-kill-ring-save)
         ("h" . pdf-annot-add-highlight-markup-annotation)
         ("t" . pdf-annot-add-text-annotation)
         ("D" . pdf-annot-delete))
  :config

  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use isearch instead of swiper
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; wait until map is available
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'newline)
    ;; save after adding comment
    (advice-add 'pdf-annot-edit-contents-commit :after 'fp/save-buffer-no-args)))
  
;; http://pragmaticemacs.com/emacs/even-more-pdf-tools-tweaks/

(use-package org-pdfview
  :ensure t)

;; ** perspective
;; (use-package perspective)

;; ** popup
(use-package popup)

;; ** popwin
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

;; ** ps-print
(use-package ps-print
  :ensure nil
  :pin manual
  :config
  ;; It can help sometimes to be able to print source code!
  ;; (setenv "GS_LIB" "C:/Local/gs9.15/lib;C:/Local/gs9.15/fonts")
  (setq ps-printer-name t)
  (setq ps-lpr-command "gswin32c.exe")
  (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH"
                          "-sDEVICE=mswinpr2"
                          "-sPAPERSIZE=a4"))
  )

;; ** projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands projectile-global-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; FIXME: is it needed?
  (setq projectile-mode-line
        '(:eval (if (projectile-project-p)
                    (if (file-remote-p default-directory)
                        " Projectile"
                      (format " Projectile[%s]" (projectile-project-name)))
                  "No project")))
  (setq projectile-cache-file (expand-file-name  "projectile.cache" fp-config-savefile-dir)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" fp-config-savefile-dir))

  ;; So projectile works with ido (or ivy)
  (setq projectile-completion-system 'ido)
  ;; (setq projectile-completion-system 'ivy)

  (setq projectile-indexing-method 'alien)
  (projectile-global-mode)
  (projectile-global-mode t)
  )


;; ** rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "dark slate gray")
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "brown")
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "deep sky blue")
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "magenta")
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "goldenrod")
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "lime green")
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "black")
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "cyan")
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "yellow")
  ;; (set-face-foreground 'rainbow-delimiters-unmatched-face "red")
  )

;; ** ranger

(use-package ranger
  :ensure t
  :config
  ;; *** Setting as Default Directory Handler
  ;; Ranger has the ability to be used as the default directory handler
  ;; when Emacs identifies a directory is opened. To make deer the
  ;; default handler, set ranger-override-dired and restart.
  (setq ranger-override-dired t)

  ;; *** Configuration

  ;; Most parameters can be toggled on and off and stay within the
  ;; current emacs session. Any settings that are desired on startup
  ;; should be set below.

  ;; *** Buffer Management

  ;; When disabling the mode you can choose to kill the buffers that
  ;; were opened while browsing the directories.

  (setq ranger-cleanup-on-disable t)

  ;; Or you can choose to kill the buffer just after you move to another
  ;; entry in the dired buffer.

  ;; (setq ranger-cleanup-eagerly t)

  ;; *** Directory Listing

  ;; You can choose to show dotfiles at ranger startup, toggled by zh.

  (setq ranger-show-dotfiles t)

  ;; *** Window Decoration and Appearance

  ;; Ranger by default modifies the header-line in dired to make a more
  ;; consistent appearance to the real ranger. Setting to nil will
  ;; disable this feature.

  (setq ranger-modify-header t)

  ;; Define custom function used to output header of primary ranger
  ;; window. Must return a string that is placed in the header-line.

  (setq ranger-header-func 'ranger-header-line)

  ;; Define custom function used to output header of parent and preview
  ;; windows. Must return a string that is placed in the header-line.

  (setq ranger-parent-header-func 'ranger-parent-header-line)
  (setq ranger-preview-header-func 'ranger-preview-header-line)

  ;; The cursor can also be hidden for a seamless user experience
  ;; showing just the cursor line. This feature can be disabled in the
  ;; cases for themes that do not have a visible cursor line

  ;; (setq ranger-hide-cursor nil)

  ;; *** Delays

  ;; Certain window display options are defined with a delay for a
  ;; better user experience. The below options can be customized to
  ;; adjust time to display the preview and the footer information.

  ;; (setq ranger-footer-delay 0.2)
  ;; (setq ranger-preview-delay 0.040)

  ;; *** Parent Window Options

  ;; You can set the number of folders to nest to the left, adjusted by z- and z+.

  (setq ranger-parent-depth 2)

  ;; You can set the size of the parent windows as a fraction of the frame size.

  ;; (setq ranger-width-parents 0.12)

  ;; When increasing number of nested parent folders, set max width as
  ;; fraction of frame size to prevent filling up entire frame with
  ;; parents.

  ;; (setq ranger-max-parent-width 0.12)

  ;; *** Preview Window Options

  ;; Set the default preference to preview selected file.

  (setq ranger-preview-file t)

  ;; You can choose to show previews literally, or through find-file, toggled by zi.

  (setq ranger-show-literal nil)

  (setq ranger-preview-delay-time 0.07)
  (setq ranger-setup-preview-delay-timer nil)

  ;; You can set the size of the preview windows as a fraction of the frame size.

  ;; (setq ranger-width-preview 0.55)

  ;; You probably don't want to open certain files like videos when
  ;; previewing. To ignore certain files when moving over them you can
  ;; customize the following to your liking:

  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))

  ;; To set the max files size (in MB), set the following parameter:

  (setq ranger-max-preview-size 10)

  ;; The preview function is also able to determine if the file selected is a binary file. If set to t, these files will not be previewed.
  ;; (setq ranger-dont-show-binary t)

  ;; Fix the masks for the elisp internal implementation of ls
  (setq ranger-dired-display-mask '(t t t t t t t)
        ranger-dired-hide-mask '(nil nil nil nil nil nil t))
  )

;; ** re-builder
(use-package re-builder
  :ensure t
  :config
  (setq reb-re-syntax 'string)
  )

;; ** recentf
;; save recent files
(use-package recentf
  :ensure nil
  :pin manual
  :config
  (setq recentf-save-file (expand-file-name "recentf" fp-config-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)

  (defun fp-config-recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
                (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list fp-config-savefile-dir package-user-dir)))))

  (add-to-list 'recentf-exclude 'fp-config-recentf-exclude-p)

  (recentf-mode +1)
  )

;; ** request
;; Not even sure about who is using it?
(use-package request
  :ensure t
  :config
  (setq request-storage-directory (locate-user-emacs-file "var/request/")))

;; ** sanityinc-tomorrow theme
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t))
;; Possibly enhance the previous theme
(use-package graphene-meta-theme
  :ensure t
  :config
  (load-theme 'graphene-meta t))

;; ** savehist
(use-package savehist
  :ensure nil
  :pin manual
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" fp-config-savefile-dir))
  (savehist-mode +1)
  )

;; ** saveplace
(use-package saveplace
  :ensure nil
  :pin manual
  :config
  (setq save-place-file (expand-file-name "saveplace" fp-config-savefile-dir))
  (save-place-mode 1))

;; ** server
(use-package server
  :ensure nil
  :pin manual
  :config
  ;; Launch emacs server
  ;; It is annoying to change location of authentication file
  ;; because it requires emacsclient to be aware of it.
  (setq server-auth-dir (locate-user-emacs-file "var/server/"))
  (defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
    "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
    (ad-set-arg 0
                (mapcar (lambda (fn)
                          (let ((name (car fn)))
                            (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                                (cons
                                 (match-string 1 name)
                                 (cons (string-to-number (match-string 2 name))
                                       (string-to-number (or (match-string 3 name) ""))))
                              fn))) files)))

  (server-start)
  )

;; ** smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  ;; delegate theming to the currently active theme
  (setq sml/theme nil)
  (add-hook 'after-init-hook #'sml/setup))

;; ** smex
;;; smex, remember recently and most frequently used commands
(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name "smex-items" fp-config-savefile-dir))
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("C-x C-m" . smex)
   ("M-X" . smex-major-mode-commands)))

;; ** smartparens
;; smart pairing for all

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  ;; (smartparens-strict-mode 0)
  (show-smartparens-global-mode +1)
  )


;; ** smartrep
(use-package smartrep
  :ensure t
  :config
  (smartrep-define-key global-map "C-c ."
    '(("+" . apply-operation-to-number-at-point)
      ("-" . apply-operation-to-number-at-point)
      ("*" . apply-operation-to-number-at-point)
      ("/" . apply-operation-to-number-at-point)
      ("\\" . apply-operation-to-number-at-point)
      ("^" . apply-operation-to-number-at-point)
      ("<" . apply-operation-to-number-at-point)
      (">" . apply-operation-to-number-at-point)
      ("#" . apply-operation-to-number-at-point)
      ("%" . apply-operation-to-number-at-point)
      ("'" . operate-on-number-at-point)))

  )

;; ** spaceline
(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; ** ssh
(use-package ssh
  :ensure nil
  :pin manual
  :config
  ;; Requires that sessions have been saved under putty.
  ;; Connect with session name.

  (setq ssh-program "c:/Local/putty/plink.exe")
  (setq ssh-explicit-args '("-load"))

  ;; Cannot run it in hook
  (defun my-comint-simple-send (proc string)
    "[Overrided] Default function for sending to PROC input STRING.
This just sends STRING plus a newline.  To override this,
set the hook `comint-input-sender'."
    (let ((send-string
           (if comint-input-sender-no-newline
               string
             ;; Sending as two separate strings does not work
             ;; on Windows, so concat the \n before sending.
             (concat string "\r")))) ; My change here \n -> \r
      (comint-send-string proc send-string))
    (if (and comint-input-sender-no-newline
             (not (string-equal string "")))
        (process-send-eof)))

  (defun my-fix-plink ()
    (interactive)
    (setq comint-process-echoes t)
    (setq comint-input-sender 'my-comint-simple-send)
    (setq-local comint-input-sender-no-newline nil))

  (add-hook 'ssh-mode-hook 'my-fix-plink)
  )

;; ** sumatra-forward
(use-package sumatra-forward
  :load-path ".emacs.d/local/sumatra-forward"
  :ensure nil
  :pin manual)

;; ** thingatpt
(use-package thingatpt
  :ensure nil
  :pin manual)

;; ** tramp
(use-package tramp
  :ensure nil
  :pin manual
  :config
  (setq tramp-default-method "plink"
        tramp-default-user "user"
        tramp-default-host "host.foo.bar"
        tramp-persistency-file-name (expand-file-name "tramp" fp-config-savefile-dir)
        )
  )

;; ** treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    ;; (use-package treemacs-evil
    ;;   :ensure t
    ;;   :demand t)
    (setq treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs--persist-file              (expand-file-name "treemacs-persist" fp-config-savefile-dir)
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-is-never-other-window      nil
          treemacs-goto-tag-strategy          'refetch-index)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))
  :bind
  (:map global-map
        ;; ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ("C-c ft"     . treemacs-toggle)
        ("C-c fT"     . treemacs)
        ("C-c fB"     . treemacs-bookmark)
        ("C-c f C-t"  . treemacs-find-file)
        ("C-c f M-t"  . treemacs-find-tag)))
;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header)
;;   :bind (:map global-map
;;               ("C-c fP" . treemacs-projectile)
;;               ("C-c fp" . treemacs-projectile-toggle)))

;; ** undo-tree
(use-package undo-tree
  :ensure nil
  :pin manual
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  ;; autosave the undo-tree history
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode)
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)))

;; ** unfill
(use-package unfill
  :ensure t
  :commands (unfill-paragraph
             unfill-region)
  :bind ("M-Q" . unfill-paragraph))

;; ** uniquify
;; meaningful names for buffers with the same name
(use-package uniquify
  :ensure nil
  :pin manual
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

;; ** url
(use-package url
  :ensure nil
  :pin manual
  :config
  (setq url-configuration-directory (expand-file-name "url/" fp-config-savefile-dir))
  )

;; ** volatile-highlights
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  )

;; ** wgrep
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  ; (setq wgrep-change-readonly-file t)
  )
(use-package wgrep-ag
  :ensure t
  :requires ag
  :config
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )


;; ** which-func
(use-package which-func
  :ensure nil
  :pin manual
  :diminish which-function-mode
  :config
  (which-function-mode 1))

;; ** which-key
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right)
  (which-key-mode +1)
  )

;; ** FIXME: whitespace mode
;; Whitespace mode could be turned on for a couple of modes. But which ones ?
;; Same applies for autofill.
;; Text modes could benefit from a light whitespace.
;; Default configuration is way too heavy.
(use-package whitespace
  :config
  ;; make whitespace-mode use just basic coloring
  (setq whitespace-line-column 80) ;; limit line length
  ;;  (setq whitespace-style '(face tabs empty trailing lines-tail space-before-tab space-after-tab))
  (setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
  (setq whitespace-display-mappings
       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
  (global-whitespace-mode 0))

;; ** windmove
;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :ensure nil
  :pin manual
  :config
  (windmove-default-keybindings 'super)
  ;; Avoid trigerring errors
  (setq windmove-wrap-around t)
  ;; automatically save buffers associated with files on buffer switch
  ;; and on windows switch
  (defun fp-config-auto-save-command ()
    "Save the current buffer if `fp-config-auto-save' is not nil."
    (when (and ;; fp-config-auto-save
           buffer-file-name
           (buffer-modified-p (current-buffer))
           (file-writable-p buffer-file-name))
      (save-buffer)))
  ;; advise all window switching functions
  (advise-commands "auto-save"
                   (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                   before
                   (fp-config-auto-save-command))
  (add-hook 'mouse-leave-buffer-hook 'fp-config-auto-save-command)
  (when (version<= "24.4" emacs-version)
    (add-hook 'focus-out-hook 'fp-config-auto-save-command))
  )

;; ** winner
(use-package winner
  :ensure nil
  :pin manual
  :config
  (winner-mode +1))

;; ** writegood-mode
(use-package writegood-mode
  :ensure t
  :config
  (eval-after-load "writegood-mode"
    '(diminish 'writegood-mode)))

;; (use-package yaml-mode)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-installed-snippets-dir (locate-user-emacs-file "savefile/snippets/")))

;; ** zenburn-theme
;; (use-package zenburn-theme
;;           :ensure t
;;           :init
;;           (setq zenburn-colors-alist
;;                 '(("zenburn-fg+1"     . "#FFFFEF")
;;                   ("zenburn-fg"       . "#DCDCCC")
;;                   ("zenburn-fg-1"     . "#70705E")
;;                   ("zenburn-bg-2"     . "#000000")
;;                   ("zenburn-bg-1"     . "#202020")
;;                   ("zenburn-bg-05"    . "#2D2D2D")
;;                   ("zenburn-bg"       . "#313131")
;;                   ("zenburn-bg+05"    . "#383838")
;;                   ("zenburn-bg+1"     . "#3E3E3E")
;;                   ("zenburn-bg+2"     . "#4E4E4E")
;;                   ("zenburn-bg+3"     . "#5E5E5E")
;;                   ("zenburn-red+1"    . "#E9B0B0")
;;                   ("zenburn-red"      . "#D9A0A0")
;;                   ("zenburn-red-1"    . "#C99090")
;;                   ("zenburn-red-2"    . "#B98080")
;;                   ("zenburn-red-3"    . "#A97070")
;;                   ("zenburn-red-4"    . "#996060")
;;                   ("zenburn-orange"   . "#ECBC9C")
;;                   ("zenburn-yellow"   . "#FDECBC")
;;                   ("zenburn-yellow-1" . "#EDDCAC")
;;                   ("zenburn-yellow-2" . "#DDCC9C")
;;                   ("zenburn-green-1"  . "#6C8C6C")
;;                   ("zenburn-green"    . "#8CAC8C")
;;                   ("zenburn-green+1"  . "#9CBF9C")
;;                   ("zenburn-green+2"  . "#ACD2AC")
;;                   ("zenburn-green+3"  . "#BCE5BC")
;;                   ("zenburn-green+4"  . "#CCF8CC")
;;                   ("zenburn-cyan"     . "#A0EDF0")
;;                   ("zenburn-blue+1"   . "#9CC7FB")
;;                   ("zenburn-blue"     . "#99DDE0")
;;                   ("zenburn-blue-1"   . "#89C5C8")
;;                   ("zenburn-blue-2"   . "#79ADB0")
;;                   ("zenburn-blue-3"   . "#699598")
;;                   ("zenburn-blue-4"   . "#597D80")
;;                   ("zenburn-blue-5"   . "#436D6D")
;;                   ("zenburn-magenta"  . "#E090C7")))
;;           )

;; ** zop-to-char
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))


;; * Programming
;; ** python
;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :config
;;   (add-hook 'python-mode-hook 'elpy-mode)
;;   (add-hook 'python-mode-hook 'smartparens-mode)
;;   (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
;;   ;;  (add-hook 'python-mode-hook 'jedi:setup)
;;   (defun my/python-mode-hook ()
;;     (company-mode +1)
;;     (add-to-list 'company-backends 'company-jedi))

;;   (add-hook 'python-mode-hook 'my/python-mode-hook)

;;   ;; Sets the python interpreter to be ipython. To trick emacs into
;;   ;; thinking we're still running regular python, we run ipython in
;;   ;; classic mode.
;;   (setq
;;    python-shell-interpreter "ipython"
;;    python-shell-interpreter-args "-i --classic"
;;    python-environment-directory (locate-user-emacs-file "savefile/python-environments")))

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :config
;;   (elpy-enable)
;;   ;; (elpy-use-ipython)
;;   ;; (setq elpy-rpc-backend "jedi")
;;   )

;; (use-package company-jedi
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (add-to-list 'company-backends 'company-jedi))

;; ** anaconda-mode
(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (setq anaconda-mode-installation-directory (locate-user-emacs-file "var/anaconda-mode"))
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  ;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")
  ;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/dependency")
  )



;; ** ediprolog
(use-package ediprolog
  :ensure t)

;; ** nxml
(use-package nxml-mode
  :ensure nil
  :pin manual
  :config
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)

  ;; pom files should be treated as xml files
  (add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))

  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t))


;; ** pseudocode
(use-package fp-pseudocode
  :ensure nil
  :pin manual)
;; ** slime
(use-package lisp-mode
  :ensure nil
  :pin manual)
(use-package slime
  :ensure nil
  :pin manual
  :config
  (modify-syntax-entry ?\| "_ 23bn" lisp-mode-syntax-table)

  (setq slime-lisp-implementations
        '((sbcl ("c:/Local/SBCL/current/sbcl.exe" "--")
                :coding-system utf-8-unix
                :env ("SBCL_HOME=c:/Local/SBCL/current/"))
          (ccl ("c:/Local/CCL/current/wx86cl64.exe"
                "-n" "-K" "utf-8" "-l" "c:/Local/CCL/ccl-init.lisp")
               :coding-system utf-8-unix
               :env ("CCL_DEFAULT_DIRECTORY=c:/Local/CCL/current/"))
          (ccl-ht ("c:/Local/CCL/current/wx86cl64.exe"
                   "-n" "-K" "utf-8" "-l" "c:/Home/Web-new/csfpweb.lisp")
                  :coding-system utf-8-unix
                  :env ("CCL_DEFAULT_DIRECTORY=c:/Home/Web-new/lisp/"))

          (lww ("c:/Local/Lispworks/lw-slime.exe"))
          (acl ("c:/Local/acl82express/allegro.exe"))))

  (setq slime-default-lisp 'ccl)

  (setq slime-repl-history-file (expand-file-name "slime-history.eld" fp-config-savefile-dir))

  (slime-setup '(slime-fancy))

  (defun fp-slime-disconnect ()
    (when (fboundp 'slime-disconnect-all)
      (slime-disconnect-all)))

  (add-hook 'kill-emacs-hook 'fp-slime-disconnect)

  ;; (defun prelude-start-slime ()
  ;;   (unless (slime-connected-p)
  ;;     (save-excursion
  ;;       (if (find-process-by-name "ccl-slime")
  ;;           (slime-connect "127.0.0.1" 4006)
  ;;         (slime)))))

  ;; (add-hook 'slime-mode-hook
  ;;           (lambda ()
  ;;             (require 'slime-typeout-frame)
  ;;             (slime-ensure-typeout-frame)
  ;;             ))

  ;; (add-hook 'slime-autodoc-mode-hook
  ;;           (lambda ()
  ;;             (setq eldoc-documentation-function
  ;;                   (lambda (&rest args)
  ;;                     (let ((docstring (apply #'slime-autodoc args)))
  ;;                       (when docstring
  ;;                         (with-current-buffer (get-buffer-create "*typeout*")
  ;;                           (goto-char (point-max))
  ;;                           (insert docstring "\n"))))))))

  (setq common-lisp-hyperspec-root "file://C:/local/LispWorks/lib/6-1-0-0/manual/online/CLHS/")
  )
;; ** specif
(use-package fp-specif
  :ensure nil
  :pin manual)

;; ** magic-latex-buffer
(use-package magic-latex-buffer
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  (setq magic-latex-enable-block-highlight nil
        magic-latex-enable-suscript        t
        magic-latex-enable-pretty-symbols  t
        magic-latex-enable-block-align     nil
        magic-latex-enable-inline-image    nil))
;; ** tex
(use-package tex-site
  :ensure auctex
  :after (magic-latex-buffer)
  :mode ("\\.tex\\'" . latex-mode)
  :bind
  (:map TeX-mode-map
   ("<f5>" . "C-u C-c C-c")
   ("<f6>" . TeX-next-error)
   :map LaTeX-mode-map
   ("S-<f5>" . run-latexmk))
  :config
  (require 'latex)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (magic-latex-buffer)
              (LaTeX-math-mode)
              (rainbow-delimiters-mode)
              (flyspell-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-engine 'luatex)
              (setq global-font-lock-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)))

                                        ;https://github.com/politza/pdf-tools/issues/187

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)
  ;; nil because I don't want the pdf to be opened again in the same frame after C-c C-a
  ;; (setq TeX-view-program-selection nil)
  ;; (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  ;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

                                        ;add org ref into auctex
  ;; https://github.com/jkitchin/org-ref/issues/216
  ;; (add-hook 'LaTeX-mode-hook (lambda () (require 'org-ref)))

  ;; https://github.com/politza/pdf-tools/pull/60
  (setq pdf-sync-forward-display-action
        '(display-buffer-reuse-window (reusable-frames . t)))
  ;; same thing, now I can jump from pdf in another frame into source
  (setq pdf-sync-backward-display-action
        '(display-buffer-reuse-window (reusable-frames . t)))

                                        ; language specific hooks in auctex
  ;; (add-hook 'TeX-language-dk-hook
  ;;           (lambda () (ispell-change-dictionary "brasileiro")))


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

  (setq TeX-auto-global (expand-file-name
                         "auctex-auto-generated-info/" temporary-file-directory))
  (setq TeX-auto-local (expand-file-name
                        "auctex-auto-generated-info/" temporary-file-directory))

  ;; TODO: solve the problem of AUC-TeX wrongly parsing headers printed
  ;; by Web2C.

  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))
  (add-to-list 'LaTeX-verbatim-environments "comment")

  )


;; (setq TeX-view-program-selection
;;       '((output-dvi "DVI Viewer")
;;         (output-pdf "PDF Viewer")
;;         (output-html "HTML Viewer")))

;; ;; this section is good for OS X only
;; ;; TODO add sensible defaults for Linux/Windows
;; (setq TeX-view-program-list
;;       '(("DVI Viewer" "open %o")
;;         ("PDF Viewer" "c:/Local/SumatraPDF/SumatraPDF.exe -reuse-instance %o")
;;         ("HTML Viewer" "open %o")))

;; (defun fp-config-latex-mode-defaults ()
;;   (turn-on-auto-fill)
;;   (abbrev-mode +1))

;; (setq fp-config-latex-mode-hook 'fp-config-latex-mode-defaults)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                              (run-hooks 'fp-config-latex-mode-hook)))

;;; fp-config-latex.el ends here
;;; AUC-TeX
;;; auctex
;; (add-to-list 'load-path
;;   (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../elpa/auctex-11.86/")))
;; (require 'latex)
;; (require 'tex-site)
;; (require 'tex-mik)

;; (autoload 'TeX-load-hack
;;   (expand-file-name "../tex-site.el" (file-name-directory load-file-name)))
;; (TeX-load-hack)

;; (put 'LaTeX-command 'safe-local-variable 't)

;; (add-to-list 'load-path
;;  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../elpa/sumatra-forward-2008.10.8/")))
;; (require 'sumatra-forward)

;; Set the default PDF reader to SumatraPDF. The executable should be in PATH

;; (setcdr (assoc "^pdf$" TeX-output-view-style)
;;         '("." "c:/Local/SumatraPDF/SumatraPDF.exe -reuse-instance %o"))

;; (setq TeX-engine 'luatex)

;; (setq TeX-source-correlate-mode t)
;; (setq TeX-source-correlate-method 'synctex)
;; (setq TeX-view-program-list
;;       '(("SumatraPDF" ("\"c:/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
;;                         (mode-io-correlate " -forward-search %b %n ") " %o"))))

;; (eval-after-load 'tex
;;   '(progn
;;      (assq-delete-all 'output-pdf TeX-view-program-selection)
;;      (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

;; (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
;;                                    (output-dvi "Yap")
;;                                    (output-html "start"))
;;       )
;; (setcdr (assoc "LaTeX" TeX-command-list)
;;         '("%l -file-line-error %(mode) \"%t\"" TeX-run-TeX nil
;;           (latex-mode doctex-mode)
;;           :help "Run LaTeX"))

;; ** company auctex
;; (use-package company-auctex
;;   :ensure t
;;   :defer t
;;   :config
;;   (company-auctex-init))

;; ** latex preview pane

(use-package latex-preview-pane
  :disabled t
  :bind ("M-p" . latex-preview-pane-mode)
  :config
  (setq doc-view-ghostscript-program "gswin32c")

  (custom-set-variables
   '(shell-escape-mode "-shell-escape")
   '(latex-preview-pane-multifile-mode (quote auctex))))


;; ** reftex
(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

;; ** shell

(use-package eshell
  :defer t
  :config
  (setq eshell-directory-name (expand-file-name "eshell" fp-config-savefile-dir))
  ;; Start a new eshell even if one is active.
  (bind-key "C-x M"  '(lambda () (interactive) (eshell t)))
  :bind (
         ;; Start eshell or switch to it if it's active.
         ("C-x m" . eshell)))

;; Start a regular shell if you prefer that.
(bind-key "C-x M-m" 'shell)
(bind-key "C-x H-m" 'ansi-term)

;; ** tuareg
(use-package tuareg
  :ensure t
  )

;; ** web-mode
(use-package web-mode
  :ensure t
  :config
  (push '("php" . "\\.phtml\\'") web-mode-engine-file-regexps)
  (dolist (engine-regexp web-mode-engine-file-regexps)
    (when (cdr engine-regexp)
      (add-to-list 'auto-mode-alist `(,(cdr engine-regexp) . web-mode)))))


;; * Org
;; ** Loading
(use-package org
  :ensure nil
  :pin manual
  ;; need to load it right away, because the configuration occurs
  ;; below, outside of the :config section. Configuration is too large
  ;; to put it in the section.
  :demand t
  :init
  (setq org-modules
        '(org-annotate-file
                                        ; org-bbdb
          org-bibtex
          ;; org-bibtex-extras
          org-checklist
          org-contacts
                                        ; org-crypt
                                        ; org-docview
          org-drill
          org-eval
          org-expiry
                                        ; org-gnus
                                        ; org-favtable
                                        ; org-gnus
          org-habit
          org-id
          org-info
          org-inlinetask
          org-interactive-query
                                        ; org-irc
                                        ; org-jsinfo
          org-learn
                                        ; org-man
                                        ; org-mew
          org-mime
                                        ; org-mhe
          org-mouse
          org-panel
          org-protocol
          org-protocol-capture-html
                                        ; org-rmail
          org-screen
                                        ; org-taskjuggler
          org-timer
          org-toc
                                        ; org-vm
                                        ; org-w3m
          org-wl
          ob-ipython
          ox
          ox-bibtex
          ox-reveal
          ox-latex
          ox-beamer
          ox-html
          ox-koma-letter
          ox-reveal
          ox-ipynb
          ox-publish
          ))
  :config
  (org-load-modules-maybe t)
  )
;; ** Org initialization

(setq org-directory "~/Org/")

;; Hook to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                         'org-update-all-dblocks t t)))

(add-hook 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

;; Some of them are too large
;; (add-hook 'org-mode-hook 'org-display-inline-images)

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)
;; Don't use actual width by default
(setq org-image-actual-width nil)

;; Costly!
;; (setq org-use-property-inheritance t)

;; ** footnotes
(setq org-footnote-auto-label 'confirm)
(setq org-footnote-auto-adjust t)
(setq org-footnote-define-inline nil)

;; To make org show leading stars use
;; (setq org-hide-leading-stars nil)

;; This removes the indentation in the org-file but displays it as if it
;; was indented while you are working on the org file buffer.
(setq org-startup-indented t)

(setq org-hide-block-startup t)

;; The following setting hides blank lines between headings which keeps folded view nice and compact.
(setq org-cycle-separator-lines 0)

;; The following setting prevents creating blank lines before headings
;; but allows list items to adapt to existing blank lines around the
;; items:
(setq org-blank-before-new-entry '((heading . auto)
                                   (plain-list-item . auto)))

;; To create new headings in a project file it is really convenient to
;; use C-RET, C-S-RET, M-RET, and M-S-RET. This inserts a new headline
;; possibly with a TODO keyword. With the following setting
(setq org-insert-heading-respect-content nil)
;; org inserts the heading at point for the M- versions and respects
;; content for the C- versions. The respect content setting is
;; temporarily turned on for the C- versions which adds the new
;; heading after the content of the current item. This lets you hit
;; C-S-RET in the middle of an entry and the new heading is added
;; after the body of the current entry but still allow you to split an
;; entry in the middle with M-S-RET.

;; Notes at the top
(setq org-reverse-note-order nil)

(setq org-show-context-detail '((isearch . lineage)
                                (bookmark-jump . lineage)
                                (default . canonical)))

;; ** Attachments
(setq org-id-method 'uuidgen)

;; ** Open files externally

(setq org-file-apps
      (list '("\\.mm\\'" . emacs)
            ;; '("\.pdf::\(\d+\)\'" . "c:/Local/SumatraPDF/SumatraPDF -reuse-instance -page %1 %s")
            ;; '("\\.pdf\\'" . "c:/Local/SumatraPDF/SumatraPDF -reuse-instance %s")

            (cons "\\.pdf\\'" (lambda (_file path) (org-pdfview-open path)))

            (cons "\\.pdf::\\(\\d+\\)\\'" (lambda (_file path) (org-pdfview-open path)))
            '(auto-mode . emacs)
            '(remote . emacs)
            (cons 'system (lambda (file _path)
                            (with-no-warnings (w32-shell-execute "open" file))))
            (cons t (lambda (file _path)
                      (with-no-warnings (w32-shell-execute "open" file))))
            ))

;; ** org-id
(setq org-id-locations-file
      (expand-file-name "org-id-locations" fp-config-savefile-dir))

;; ** Exporting tables
(setq org-table-export-default-format "orgtbl-to-csv")

;; ** Insert inactive timestamps and exclude from export
(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)

;; To prevent the timestamps from being exported in documents I use the following setting
(setq org-export-with-timestamps nil)


;; ** Keys & UI

                                        ; (require 'org-cua-dwim)
                                        ; (org-cua-dwim-activate)

(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-support-shift-select t)

(setq org-use-speed-commands t)
;; (setq org-use-speed-commands
;;       (lambda nil
;;         (and (looking-at org-outline-regexp-bol)
;;              (not (org-in-src-block-p t)))))

(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))
;;; https://blog.aaronbieber.com/2016/11/23/creating-org-mode-structure-templates.html
(add-to-list 'org-structure-template-alist
             (list "p" (concat ":PROPERTIES:\n"
                               "?\n"
                               ":END:")))
(add-to-list 'org-structure-template-alist
             (list "eh" (concat ":EXPORT_FILE_NAME: ?\n"
                                ":EXPORT_TITLE:\n"
                                ":EXPORT_OPTIONS: toc:nil html-postamble:nil num:nil")))

(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))
(setq org-fast-tag-selection-single-key 'expert)

;; Disable keys in org-mode
;;    C-c [
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
                                        ; (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-occur-link-in-agenda-files)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-switchb)
(define-key global-map "\C-cc" 'org-capture)

;; And allow ace-jump-mode within my org files
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)

(setq org-completion-use-ido t)

;; The following setting make RET insert a new line instead of opening
;; links. This setting is a love-hate relationship for me. When it
;; first came out I immediately turned it off because I wanted to
;; insert new lines in front of my links and RET would open the link
;; instead which at the time I found extremely annoying. Since then
;; I've retrained my fingers to hit RET at the end of the previous
;; line.
(setq org-return-follows-link t)

;; Jump to target
;; Needed by org-goto so that motion keys work.
(setq org-goto-auto-isearch nil)
;; Targets complete directly with IDO
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)

(setq org-loop-over-headlines-in-active-region t)

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)


;; ** Display

(setq org-pretty-entities t)

(setq org-highlight-latex-and-related '(latex))

;; Don't remove the highlighting after an occur search (C-c / /)
;; (setq org-remove-highlights-with-change nil)

;; Not mandatory
                                        ; (setq org-fontify-done-headline t)

(setq org-fontify-emphasized-text t)
;; Only needed if setting a background face for headlines
                                        ; (setq org-fontify-whole-heading-line t)
(setq org-hide-emphasis-markers nil)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Hack to change the appearance of the checkboxes [X] via the ML
(font-lock-add-keywords
 'org-mode `(("\\[X\\]"
              (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                        "☑") ; ✔ ☐ ☑
                        nil)))
             ("\\[ \\]"
              (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                        "☐")
                        nil)))))

;; Highlight clock when running overtime
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))

;; ** Minimize Emacs frames
(setq org-link-frame-setup '((vm . vm-visit-folder-other-window)
                             (vm-imap . vm-visit-imap-folder-other-window)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file-other-window)
                             (wl . wl-other-window)))

(setq org-link-mailto-program '(browse-url-mail "mailto:%a?subject=%s"))

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;; Allow alphabetic lists to be recognized for lists
(setq org-alphabetical-lists t)

(setq org-allow-promoting-top-level-subtree t)



;; ** TODO Keywords

(setq org-todo-keywords
      '( ; for tasks
        (sequence "TODO(t)" "NEXT(n@/!)" "BLOCKED(b@/!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "DONE(d)")
        ; for papers or code
        (sequence "REVIEW(r)" "DELVE(d)" "|" "CANCELLED(c@/!)" "DONE(d)")))


(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("BLOCKED" :foreground "MediumVioletRed" :weight bold)
        ("NEXT" :foreground "CornFlowerBlue" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "LightSlateGrey" :weight bold)
        ("REVIEW" :foreground "gold" :weight bold)
        ("DELVE" :foreground "DarkOrchid" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("BLOCKED" ("BLOCKED" . t))
        ("NEXT" ("NEXT" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD") ("BLOCKED") ("REVIEW") ("DELVE") ("NEXT"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("BLOCKED") ("NEXT"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("BLOCKED"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("BLOCKED") ("NEXT") ("DELVE") ("REVIEW"))
        ("REVIEW" ("DELVE"))
        ("DELVE" ("DELVE" . t))))

;; Fast todo selection allows changing from any task todo state to any
;; other state directly by selecting the appropriate key from the fast
;; todo selection key menu.
(setq org-use-fast-todo-selection t)

;; ** TODO Capture

(setq org-default-notes-file "~/Org/notes.org")

(setq org-capture-templates-contexts nil)

(setq org-datetree-add-timestamp 'inactive)

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol

(setq org-capture-templates
        `(("t" "Tasks" entry
           (file+headline "~/Org/notes.org" "Tasks")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Notes" entry
           (file+headline "~/Org/notes.org" "Notes")
           "* REVIEW %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal entry" plain
           (file+olp+datetree "~/Org/Diary.org")
           "%i\n%?\n"
           :unnarrowed t)
          ("J" "Journal entry with date" plain
           (file+datetree+prompt "~/Org/Diary.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)
          ("s" "Journal entry with date, scheduled" entry
           (file+datetree+prompt "~/Org/Diary.org" "Journal")
           "* \n%K - %a\n%t\t%i\n%?\n"
           :unnarrowed t)
          ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
           "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
          ("w" "Web site" entry (file+olp "~/Org/notes.org" "Web")
           "* %c :website:\n%U\n\n %?%:initial")
          ))

;; (setq org-capture-templates
;;         `(("t" "Tasks" entry
;;            (file+headline "~/Org/notes.org" "Inbox")
;;            ,my/org-basic-task-template)
;;           ("T" "Quick task" entry
;;            (file+headline "~/Org/notes.org" "Inbox")
;;            "* TODO %^{Task}\nSCHEDULED: %t\n"
;;            :immediate-finish t)
;;           ("i" "Interrupting task" entry
;;            (file+headline "~/Org/notes.org" "Inbox")
;;            "* STARTED %^{Task}"
;;            :clock-in :clock-resume)
;;           ("e" "Emacs idea" entry
;;            (file+headline "~/Org/Software.org" "Emacs")
;;            "* TODO %^{Task}"
;;            :immediate-finish t)
;;           ("E" "Energy" table-line
;;            (file+headline "~/personal/organizer.org" "Track energy")
;;            "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
;;            :immediate-finish t
;;            )
;;           ("b" "Business task" entry
;;            (file+headline "~/personal/business.org" "Tasks")
;;            ,my/org-basic-task-template)
;;           ("p" "People task" entry
;;            (file+headline "~/personal/people.org" "Tasks")
;;            ,my/org-basic-task-template)
;;           ("j" "Journal entry" plain
;;            (file+olp+datetree "~/Org/Diary.org")
;;            "%i\n%?\n"
;;            :unnarrowed t)
;;           ("J" "Journal entry with date" plain
;;            (file+datetree+prompt "~/Org/Diary.org")
;;            "%K - %a\n%i\n%?\n"
;;            :unnarrowed t)
;;           ("s" "Journal entry with date, scheduled" entry
;;            (file+datetree+prompt "~/Org/Diary.org" "Journal")
;;            "* \n%K - %a\n%t\t%i\n%?\n"
;;            :unnarrowed t)
;;           ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
;;            "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
;;           ("w" "Web site" entry (file+olp "~/Org/notes.org" "Web")
;;            "* %c :website:\n%U\n\n %?%:initial")
;;           ("db" "Done - Business" entry
;;            (file+headline "~/Org/notes.org" "Tasks")
;;            "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
;;           ("dp" "Done - People" entry
;;            (file+headline "~/Org/notes.org" "Tasks People")
;;            "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
;;           ("dt" "Done - Task" entry
;;            (file+headline "~/Org/notes.org" "Inbox")
;;            "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
;;           ("q" "Quick note" item
;;            (file+headline "~/Org/notes.org" "Quick notes"))
;;   ;;         ("l" "Ledger entries")
;;   ;;         ("lm" "MBNA" plain
;;   ;;          (file "~/personal/ledger")
;;   ;;          "%(org-read-date) %^{Payee}
;;   ;;   Liabilities:MBNA
;;   ;;   Expenses:%^{Account}  $%^{Amount}
;;   ;; " :immediate-finish t)
;;   ;;         ("ln" "No Frills" plain
;;   ;;          (file "~/personal/ledger")
;;   ;;          "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
;;   ;;   Liabilities:MBNA
;;   ;;   Assets:Wayne:Groceries  $%^{Amount}
;;   ;; " :immediate-finish t)
;;   ;;         ("lc" "Cash" plain
;;   ;;          (file "~/personal/ledger")
;;   ;;          "%(org-read-date) * %^{Payee}
;;   ;;   Expenses:Cash
;;   ;;   Expenses:%^{Account}  %^{Amount}
;;   ;; ")
;;           ("B" "Book" entry
;;            (file+datetree "~/Org/notes.org" "Books")
;;            "* %^{Title}  %^g
;;   %i
;;   *Author(s):* %^{Author} \\\\
;;   *ISBN:* %^{ISBN}

;;   %?

;;   *Review on:* %^t \\
;;   %a
;;   %U"
;;            :clock-in :clock-resume)
;;            ("C" "Contact" entry (file "~/Org/notes.org" "Contacts")
;;             "* %(org-contacts-template-name)
;;   :PROPERTIES:
;;   :EMAIL: %(my/org-contacts-template-email)
;;   :END:")
;;            ("n" "Daily note" table-line (file+olp "~/Org/notes.org" "Inbox")
;;             "| %u | %^{Note} |"
;;             :immediate-finish t)
;;            ("r" "Notes" entry
;;             (file+datetree "~/Org/notes.org")
;;             "* %?\n\n%i\n%U\n"
;;             )))

;; (setq org-capture-templates
;;       `(("t" "todo" entry (file "~/Org/notes.org")
;;          "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;         ("r" "respond" entry (file "~/Org/notes.org")
;;          "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;         ("n" "note" entry (file "~/Org/notes.org")
;;          "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;         ("j" "Journal" entry (file+datetree "~/Org/Diary.org")
;;          "* %?\n%U\n" :clock-in t :clock-resume t)
;;         ;; ("w" "org-protocol" entry (file "~/Org/notes.org")
;;         ;;  "* TODO Review %c\n%U\n" :immediate-finish t)
;;         ("w" "Web site"
;;          entry (file+olp "~/org/notes.org" "Web")
;;          "* %c :website:\n%U %?%:initial")
;;         ;; FIXME: shouldn't meeting be appointments? -> Diary.org?
;;         ("m" "Meeting" entry (file "~/Org/notes.org")
;;          "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;         ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;         ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;          "* %? [[%:link][%:description]] \nCaptured On: %U")
;;         ;; ("p" "Phone call" entry (file "~/Org/notes.org")
;;         ;;  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;         ("h" "Habit" entry (file "~/Org/notes.org")
;;          "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
;;         ;; ("J" "Journal Entry" plain
;;         ;;  (file+datetree "~/Org/Diary.org")
;;         ;;  "%U\n\n%?" :empty-lines-before 1)
;;         ("W" "Log Work Task" entry
;;          (file+datetree "~/Org/Worklog.org")
;;          "* TODO %^{Description}  %^g\n%?\n\nAdded: %U"
;;          :clock-in t
;;          :clock-keep t)
;;         ))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


;; ** Agenda

;; TODO: http://cachestocaches.com/2016/9/my-workflow-org-agenda/
;; Synchronization
;; TODO: http://cestlaz.github.io/posts/using-emacs-26-gcal/#.WG52MOtj0wE.reddit
;; TODO: https://www.youtube.com/watch?v=cIzzjSaq2N8&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&index=33
;; TODO: https://www.youtube.com/playlist?list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE

(setq org-agenda-include-diary nil)
;; (setq org-agenda-insert-diary-extract-time t)
(setq org-agenda-diary-file "~/Org/Diary.org")

(setq org-agenda-start-on-weekday 1)

(setq org-agenda-span 'day)

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-bulk-mark-char "*")

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; (setq org-agenda-dim-blocked-tasks t)

;; (setq org-agenda-entry-text-maxlines 10)
;; (setq org-agenda-file-regexp "\\.org\\'")
(setq org-agenda-files
      `("~/Org/"
                                        ; ,@(mapcan #'(lambda (dir)
                                        ;               (remove-if #'(lambda (f) (or (not (file-directory-p f))
                                        ;                                            (string-match "\\.\\.?$" f)))
                                        ;                          (directory-files dir t))) '("~/Projets/" "~/Cours/"))
	))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; ;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Sorting order for tasks on the agenda
;; (setq org-agenda-sorting-strategy
;;       (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
;;               (todo category-up effort-up)
;;               (tags category-up effort-up)
;;               (search category-up))))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo category-up time-up)
        (tags tsia-down ts-down category-up time-up)
        (search time-down timestamp-down ts-down tsia-down)))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-14t%s")
        (timeline . "  % s")
        (todo . " %i %-14:c")
        (tags . " %i %-14:c")
        (search . " %i %-14:c")))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; (setq org-agenda-remove-tags t)
;; (setq org-agenda-restore-windows-after-quit t)
;; (setq org-agenda-show-inherited-tags nil)
;; (setq org-agenda-skip-additional-timestamps-same-entry t)
;; (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;; Use agenda persistent filters
(setq org-agenda-persistent-filter t)

;; Custom agenda command definitions

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))
        ("p" . "Papers + Name tag searches")
        ("pa" tags-tree "ai")
        ("pe" tags-tree "eiah")))
      
;; (setq org-agenda-custom-commands
;;       (quote (("N" "Notes" tags "NOTE"
;;                ((org-agenda-overriding-header "Notes")
;;                 (org-tags-match-list-sublevels t)))
;;               ("h" "Habits" tags-todo "STYLE=\"habit\""
;;                ((org-agenda-overriding-header "Habits")
;;                 (org-agenda-sorting-strategy
;;                  '(todo-state-down effort-up category-keep))))
;;               (" " "Agenda"
;;                ((agenda "" nil)
;;                 (tags "REFILE"
;;                       ((org-agenda-overriding-header "Tasks to Refile")
;;                        (org-tags-match-list-sublevels nil)))
;;                 (tags-todo "-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Stuck Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-stuck-projects)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-HOLD-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-projects)
;;                             (org-tags-match-list-sublevels 'indented)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-CANCELLED/!NEXT"
;;                            ((org-agenda-overriding-header (concat "Project Next Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
;;                             (org-tags-match-list-sublevels t)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(todo-state-down effort-up category-keep))))
;;                 (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Project Subtasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-non-project-tasks)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Standalone Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-project-tasks)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-CANCELLED+WAITING|HOLD/!"
;;                            ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
;;                                                                   (if bh/hide-scheduled-and-waiting-next-tasks
;;                                                                       ""
;;                                                                     " (including WAITING and SCHEDULED tasks)")))
;;                             (org-agenda-skip-function 'bh/skip-non-tasks)
;;                             (org-tags-match-list-sublevels nil)
;;                             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
;;                             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
;;                 (tags "-REFILE/"
;;                       ((org-agenda-overriding-header "Tasks to Archive")
;;                        (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
;;                        (org-tags-match-list-sublevels nil))))
;;                nil))))


;; ** Clocking

;; https://github.com/schmendrik/OrgClockTray

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; save file
(setq org-clock-persist-file (expand-file-name "org-clock-save.el" fp-config-savefile-dir))
;; Enable auto clock resolution for finding open clocks
;; (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
(setq org-clock-sound t)

(setq org-clock-idle-time 15)


;; ** Time clocking

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; By default I want to see deadlines in the agenda 30 days before the due date.
(setq org-deadline-warning-days 30)


;; ** Calendar

;; In case it might be needed someday

(setq org-icalendar-include-todo 'all)
(setq org-icalendar-combined-name "Fabrice Popineau ORG")
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "Europe/Paris")
(setq org-icalendar-store-UID t)


;; ** GCal

(setq google/oauth-client-id "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.apps.googleusercontent.com")
(setq google/oauth-client-secret "XXXXXXXXXXXXXXXXXXXXXXXX")

(setq google/expire-time 5)

(setq google-calendar/down-days 120)
(setq google-calendar/up-days 120)
(setq google-calendar/calendar-files "~/Org/google-calendars.org")

(setq google-calendar/diary-file (expand-file-name "~/Org/Diary.org"))
(setq google-calendar/tag-associations '(("xxxxx.yyyyy@gmail.com" "tag1")
                                         ("xxxxxxxxxxxxxxxxxxx@group.calendar.google.com" "tag2")))
(setq google-calendar/auto-archive nil)


;; ** Google
(use-package google-oauth
  :ensure google-contacts
  :defer t
  )
(use-package org-google
  :ensure nil
  :pin manual
  :defer t
  )


;; ** Tags

;; Tags with fast selection keys
;; (setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;; Tags with fast selection keys
(setq org-tag-persistent-alist '((:startgrouptag)
                                 ("publish")
                                 (:grouptags)
                                 ("noexport" . ?E)
                                 ("nopublic" . ?P)
                                 ("webmenu"  . ?m)
                                 ("web"      . ?w)
                                 ("slides"   . ?s)
                                 ("pdf"      . ?p)
                                 (:endgrouptag)
                                 ))

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; To make all of the matched headings for a tag show at the same
;; level in the agenda set the following variable
(setq org-tags-match-list-sublevels t)


;; ** Refile

;;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html

(setq org-refile-targets '((nil :maxlevel . 9)
                           ("notes.org" :regexp . "Viewed")
                           (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; Annoying because requires to C-0 C-C C-w often
                                        ; (setq org-refile-use-cache t)

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(defun my/org-read-datetree-date (d)
  "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))

(defun my/org-refile-to-archive-datetree (&optional bfn)
  "Refile an entry to a datetree under an archive."
  (interactive)
  (require 'org-datetree)
  (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/Org/Diary.org"))))
         (datetree-date (my/org-read-datetree-date (org-read-date t nil))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil
                              (with-current-buffer bfn
                                (save-excursion
                                  (org-datetree-find-date-create datetree-date)
                                  (point))))))
  (setq this-command 'my/org-refile-to-journal))

;; ** Archive

(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")


;; ** Logging

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; ** Babel

;; (setq org-ditaa-jar-path "~/java/ditaa.jar")
;; (setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

                                        ; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (lisp . t)
   (dot . t)
   (org . t)
   (python . t)
   (gnuplot . t)
   (shell . t)
   ;; (ditaa . t)
   ;; (R . t)
   ;; (ruby . t)
   ;; (clojure . t)
   ;; (ledger . t)
   ;; (plantuml . t)
   ))

;; (setq org-babel-latex-htlatex-packages
;;      '("[usenames]{color}" "{tikz}" "{color}" "{listings}" "{amsmath}"))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
(add-to-list 'org-src-lang-modes '("pseudocode" . pseudocode))
(add-to-list 'org-src-lang-modes '("specif" . specif))



;; ** PDF-tools

;;; org-pdfview.el --- Support for links to documents in pdfview mode

;; Copyright (C) 2014 Markus Hauck

;; Author: Markus Hauck <markus1189@gmail.com>
;; Maintainer: Markus Hauck <markus1189@gmail.com>
;; Keywords: org, pdf-view, pdf-tools
;; Version: 0.1
;; Package-Requires: ((org "6.01") (pdf-tools "0.40"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Add support for org links from pdfview buffers like docview.
;;
;; To enable this automatically, use:
;;     (eval-after-load 'org '(require 'org-pdfview))

;; If you want, you can also configure the org-mode default open PDF file function.
;;     (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
;;     (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))

;;; Code:
;; (require 'org)
(require 'pdf-tools)
(require 'pdf-view)

(org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export)
(add-hook 'org-store-link-functions 'org-pdfview-store-link)

(defun org-pdfview-open (link)
  "Open LINK in pdf-view-mode."
  (cond ((string-match "\\(.*\\)::\\([0-9]*\\)\\+\\+\\([[0-9]\\.*[0-9]*\\)"  link)
         (let* ((path (match-string 1 link))
                (page (string-to-number (match-string 2 link)))
                (height (string-to-number (match-string 3 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)
           (image-set-window-vscroll
            (round (/ (* height (car (pdf-view-image-size))) (frame-char-height))))))
        ((string-match "\\(.*\\)::\\([0-9]+\\)$"  link)
         (let* ((path (match-string 1 link))
                (page (string-to-number (match-string 2 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)))
        (t
         (org-open-file link 1))
        ))

(defun org-pdfview-store-link ()
  "Store a link to a pdfview buffer."
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let* ((path buffer-file-name)
           (page (pdf-view-current-page))
           (link (concat "pdfview:" path "::" (number-to-string page))))
      (org-store-link-props
       :type "pdfview"
       :link link
       :description path))))

(defun org-pdfview-export (link description format)
  "Export the pdfview LINK with DESCRIPTION for FORMAT from Org files."
  (let* ((path (when (string-match "\\(.+\\)::.+" link)
                 (match-string 1 link)))
         (desc (or description link)))
    (when (stringp path)
      (setq path (org-link-escape (expand-file-name path)))
      (cond
       ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
       ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
       ((eq format 'ascii) (format "%s (%s)" desc path))
       (t path)))))

(defun org-pdfview-complete-link ()
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
  (concat (replace-regexp-in-string "^file:" "pdfview:" (org-file-complete-link))
          "::"
          (read-from-minibuffer "Page:" "1")))


;; (provide 'org-pdfview)
;;; org-pdfview.el ends here

;; ** org-noter

(use-package org-noter
  :ensure t)

;; ** Org Ref

;;
;;

(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
;;; ** Citations

;;
;; Bibliography and bibliography notes handling.
;; https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
;; http://www-public.telecom-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
;; https://emacs.stackexchange.com/questions/3375/loading-bibtex-file-in-org-mode-file
;; http://blog.modelworks.ch/?p=379
;; https://github.com/vikasrawal/orgpaper/
;;

;; The scheme is as follows:
;; - database is stored in a big org file
;; - bibtex file is extracted by org-bibtex whenever the bib file is older than the org file
;; - only 2 kind of links: bibtex and papers (more suitable to tasks)

(require 'reftex)
(require 'reftex-vars)
(require 'reftex-cite)
(require 'org-bibtex)
(require 'ox-bibtex)

(setq org-bibtex-autogen-keys t)

(setq org-bibtex-headline-format-function
  '(lambda (entry) 
     (if (cdr (assq :url entry))
         (format "[[%s][%s]]" (cdr (assq :url entry)) (cdr (assq :title entry)))
       (cdr (assq :title entry)))))

;; Export bibliography keyword

(defadvice org-latex-keyword (around org-latex-keyword-bibliography)
  "Exports `bibliography' keyword with LaTeX backend."
  (let ((key (org-element-property :key (ad-get-arg 0)))
        (value (org-element-property :value (ad-get-arg 0))))
    (if (and (string= (org-element-property :key (ad-get-arg 0)) "BIBLIOGRAPHY")
             (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\([^\r\n]*\\)" value))
        (let ((bibfile (match-string 1 value))
              (bibstyle (match-string 2 value)))
          (message "file: %s\nstyle: %s\n" bibfile bibstyle)
          (setq ad-return-value (concat "\\bibliography{"
                                        (expand-file-name bibfile)
                                        "}\n\\bibliographystyle{"
                                        bibstyle
                                        "}\n")))
      ad-do-it)))

(ad-activate 'org-latex-keyword)

;; Re-define bibtex link type

;; http://www-public.tem-tsp.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/

(defun fp-bibtex-cite-export-handler (path desc format)
  ;; (message "my-rtcite-export-handler is called : path = %s, desc = %s, format ;;= %s" path desc format)
  (cond ((eq format 'latex)
         (if (or (not desc)
                 (equal 0 (search "bibtex:" desc)))
             (format "\\cite{%s}" path)
           (format "\\cite[%s]{%s}" desc path)))
        ((eq format 'html)
         (format "[%s]"
                 (mapconcat
                  (lambda (key)
                    (format "<a href=\"#%s\">%s</a>"
                            key
                            (or (cdr (assoc key org-bibtex-html-entries-alist))
                                key)))
                  (org-split-string path ",") ","))
         )))

(defun fp-strip-string (string)
  "Strip leading and trailing whitespace from the string."
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" string)))

(defun fp-open-key-under-cursor (link)
  "Open key under the bibtex cursor. We search forward from
point to get a comma, or the end of the link, and then backwards
to get a comma, or the beginning of the link. that delimits the
keyword we clicked on. We then open link from bibtex file."
  (interactive)
  (let* ((object (org-element-context))
         (link-string (org-element-property :path object)))

    ;; we need the link path start and end
    (save-excursion
      (goto-char (org-element-property :begin object))
      (search-forward link-string nil nil 1)
      (setq link-string-beginning (match-beginning 0))
      (setq link-string-end (match-end 0)))

    ;; The key is the text between commas, or the link boundaries
    (save-excursion
      (if (search-forward "," link-string-end t 1)
          (setq key-end (- (match-end 0) 1)) ; we found a match
        (setq key-end link-string-end))) ; no comma found so take the end
    ;; and backward to previous comma from point which defines the start character
    (save-excursion
      (if (and (> (point) link-string-beginning)
               (search-backward "," link-string-beginning 1 1))
          (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
        (setq key-beginning link-string-beginning))) ; no match found
    ;; save the key we clicked on.
    (setq bibtex-key (fp-strip-string (buffer-substring key-beginning key-end)))
    (set-text-properties 0 (length bibtex-key) nil bibtex-key)
    (org-open-link-from-string (format "[[file:%s::#%s]]" (fp-find-org-bibtex-file) bibtex-key))
    ))

;; add functions to complete papers and bibtex entries
(defun org-bibtex-complete-link (&optional arg)
  "Create a bibtex link using reftex autocompletion."
  (concat "bibtex:"
          (mapconcat 'identity (reftex-do-citation nil t nil) ",")))

(org-add-link-type "bibtex" 'fp-open-key-under-cursor 'fp-bibtex-cite-export-handler)


;; Define papers link type

(defvar *fp-papers-directory*
  (expand-file-name "~/Papers/"))

(defun fp-open-paper-under-cursor (link)
  (org-open-link-from-string (format "[[file:%s%s]]" *fp-papers-directory* link)))

(defun fp-paper-cite-export-handler (path desc format)
  (cond ((eq format 'latex)
         (format "\\url{%s%s}" *fp-papers-directory* path))
        ((eq format 'html)
         (format "<a href=\"%s%s\">%s</a>" *fp-papers-directory* path (or desc path)))
        ))
(defun org-papers-complete-link (&optional arg)
  "Create a papers link using ido autocompletion."
  (concat "papers:"
          (substring
           (ido-read-file-name "papers: " "~/Papers/")
           (length *fp-papers-directory*))
          )
  )

(org-add-link-type "papers" 'fp-open-paper-under-cursor 'fp-paper-cite-export-handler)

;; Setup org-reftex when entering a new org document

;; Add a few keywords to org-bibtex-types
(defun add-keyword-to-entry (entry)
  (if (eq :optional (car entry))
      (append entry '(:keywords :url :biburl :bibsource :hal_id :hal_version :abstract :issn :isbn :pagetotal))
    entry))

(setq org-bibtex-types
      (mapcar #'(lambda (entry) `(,(car entry) ,@(mapcar #'add-keyword-to-entry (cdr entry)))) org-bibtex-types))

(defvar *fp-default-org-bibtex-file*
  (expand-file-name "~/Papers/Bibliography.org"))

(defun fp-find-org-bibtex-file ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
        (if (save-excursion
              (or (re-search-forward re nil t)
                  (re-search-backward re nil t)))
            (concat (match-string-no-properties 1) ".org"))))))

;; TODO: something wrong here. How to decide that we are writing a paper?
;; Either insert a template of headers or define a minor mode.

(defun org-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (let ((paper-p (fp-find-org-bibtex-file)))
         (when paper-p
           ;; Reftex should use the org file as master file. See C-h v TeX-master for infos.
           (setq TeX-master t)
           (turn-on-reftex)
           ;; Enable auto-revert-mode to update reftex when bibtex file changes on disk
           ;; Already done by Prelude.
           ;; (global-auto-revert-mode t)

           (setq org-bibtex-file
                 (or (fp-find-org-bibtex-file)
                     *fp-default-org-bibtex-file*))
           (setq reftex-default-bibliography
                 (list (replace-regexp-in-string "\.org$" ".bib" org-bibtex-file)))

           (reftex-parse-all)
                                        ;add a custom reftex cite format to insert links
           (reftex-set-cite-format '((?b . "[[bibtex:%l]]")
                                     (?p . "[[papers:%U][%t]]")))
           ;; org-reftex-citation prevents to change reftex-cite-format
           ;; besides, it doesn't do much more than verifying bibliography
           ;; is set up.
           (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
           ;; Useless. Click on the link.
           ;; (define-key org-mode-map (kbd "C-c (") 'org-reftex-search))
           ))))

(add-hook 'org-mode-hook 'org-reftex-setup)

;; ** Exporter
(setq org-export-default-language "fr")
(setq org-export-coding-system 'utf-8)
(setq org-export-backends '(latex odt icalendar html ascii rss koma-letter))
(setq org-export-highlight-first-table-line t)
(setq org-export-html-extension "html")
(setq org-export-html-with-timestamp nil)
(setq org-export-skip-text-before-1st-heading nil)
(setq org-export-with-LaTeX-fragments t)
(setq org-export-with-archived-trees nil)
(setq org-export-with-drawers '("HIDE"))
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts '{})
(setq org-export-with-tags 'not-in-toc)
(setq org-export-with-timestamps t)
(setq org-export-with-toc nil)
(setq org-export-with-priority t)
(setq org-export-dispatch-use-expert-ui nil)
(setq org-export-use-babel nil)
(setq org-export-allow-bind-keywords t)


;; ** LaTeX

;; (setq org-latex-hyperref-template "\\hypersetup{\n pdfauthor={Fabrice Popineau},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={english}}\n")

;; (setq org-latex-listings 'minted)
(setq org-latex-listings 't)

(setq org-latex-create-formula-image-program 'imagemagick) ;; imagemagick

(setq org-create-formula-image-latex-command "pdflatex")
(setq org-create-formula-image-convert-command "convert.exe")

(setq org-preview-latex-process-alist
      `((dvipng :programs
          ("latex" "dvipng" "gs")
          :description "dvi > png" :message "you need to install the programs: latex, dvipng and ghostscript." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
          (1.0 . 1.0)
          :latex-compiler
          ("latex -interaction nonstopmode -output-directory %o %f")
          :image-converter
          ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
        (dvisvgm :programs
          ("latex" "dvisvgm" "gs")
          :description "dvi > svg" :message "you need to install the programs: latex, dvisvgm and ghostscript." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
          (1.7 . 1.5)
          :latex-compiler
          ("latex -interaction nonstopmode -output-directory %o %f")
          :image-converter
          ("dvisvgm %f -n -b min -c %S -o %O"))
        (imagemagick :programs
          ("latex" "convert" "gs")
          :description "pdf > png" :message "you need to install the programs: latex, imagemagick and ghostscript." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
          (1.414 . 1.0)
          :latex-compiler
          ("pdflatex -interaction nonstopmode -output-directory %o %f")
          :image-converter
          (,(concat org-create-formula-image-convert-command " -density %D -trim -antialias %f -quality 100 %O")))))

;; (setq org-format-latex-options
;;       '(:foreground default :background default :scale 1.414 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
;;                     ("begin" "$1" "$" "$$" "\\(" "\\["))
;;       )

(setq org-latex-pdf-process
      '("c:/Local/Msys64/usr/bin/perl.exe c:/Local/TeXLive/texmf-dist/scripts/latexmk/latexmk.pl -cd -pdf -bibtex -pv- %f"))


(add-to-list
 'org-latex-packages-alist
 '("" "fpsupelec-snippet" t)
 )

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq org-latex-preview-ltxpng-directory
      (expand-file-name "c:/temp/ltxpng/"))

(plist-put org-format-latex-options :scale 1.0)

(defvar fp-inside-questions nil)

(defun fp-reset-inside-questions (&rest args)
  (setq fp-inside-questions ()))

(add-hook 'org-export-before-parsing-hook 'fp-reset-inside-questions)

(defun fp-org-latex-compute-exam-section (level numbered)
                                        ; (message "level %s numbered %s" level numbered)
  (case level
    (1 (if fp-inside-questions
           (progn (setq fp-inside-questions nil)
                  '("\\section{%s}" "\\end{questions}"))
         (progn (setq fp-inside-questions nil)
                '("\\section{%s}" ""))))
    (2 (if fp-inside-questions
           '("\\myquestion{%s}" . "")
         (progn
           (setq fp-inside-questions t)
           '("\\begin{questions}\n\\myquestion{%s}" . ""))))
    (3 '("\\begin{soluce} %% %s" "\\end{soluce}"))
    (t '("%s" "")))
  )

(defun fp-org-latex-compute-examshort-section (level numbered)
                                        ; (message "level %s numbered %s" level numbered)
  (case level
    (1 '("\\begin {questions} %% %s" "\\end{questions}"))
    (2 '("\\myquestion{%s}" . ""))
    (3 '("\\begin{soluce} %% %s" "\\end{soluce}"))
    (t '("%s" "")))
  )

;;; Fix the \question[] "section" header of the exam class.
;;; At this point, it would almost make sense to derive a new backend
;;; specifically for this case.

(defun fp-get-org-headline-string-element  (headline backend info)
  "Return the org element representation of an element.  Does not
    work with verbatim only headlines, e.g. \"* ~Verb~.\""
  (let ((prop-point (next-property-change 0 headline)))
    (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

(defun fp-latex-filter-headline-exam (headline backend info)
  "Replace question sections with \\question[] sections"
  ;;; Get the element we are looking at.
  (let* ((e (fp-get-org-headline-string-element headline backend info))
         (level (org-export-get-relative-level e info)))
    ;; (message "level = %s" level)
    (when (and (org-export-derived-backend-p backend 'latex)
               (= 2 level))
      ;; (message "headline = %s" headline)
      (if (string-match  "\\\\question\\[[^][]*\\]" headline)
          (progn
            ;; (message "headline1 = <%s>" headline)
            ;; (message "headline1 replaced = <%s>" (replace-regexp-in-string
            ;;                                         "\\\\question\\[[^][]*\\]" "\\\\question" headline))
            (replace-regexp-in-string
             "\\\\question\\[[^][]*\\]" "\\\\question" headline))

        ;; (message "headline2 = <%s>" headline)
        ;; (message "headline2 replaced = <%s>" (replace-regexp-in-string "\\\\question\\[[^[]*\\[\\([0-9][0-9]*\\)[^]]*\\]\s*\\]"
        ;;                                                               "\\\\question[\\1]"
        ;;                                                               headline))
        (replace-regexp-in-string "\\\\question\\[[^[]*\\[\\([0-9][0-9]*\\)[^]]*\\]\s*\\]"
                                  "\\\\question[\\1]"
                                  headline)))))

(defun fp-latex-filter-headline-exam (headline backend info)
  (message "********************\nheadline = %s\ninfo = %s" info)
  (message "type = %s" (org-element-type info))
  (message "level = %s" (plist-get info :level))
  ;;  (when (eq 'headline (org-element-type info))
  ;;    (message "headline = %s\nlevel = %s" headline
  ;;             (org-export-get-relative-level headline info)))
  )
                                        ;(setq org-export-filter-headline-functions nil)
;; (add-to-list 'org-export-filter-headline-functions
;;             'fp-latex-filter-headline-exam)

(setq org-latex-classes
      '(("beamer" "\\documentclass[presentation]{beamer}\n     [NO-DEFAULT-PACKAGES]\n     [PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("cv" "\\documentclass{europasscv}\n     [NO-DEFAULT-PACKAGES]\n     [NO-PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("poly" "\\documentclass[poly,french]{fpsupelec}\n     [NO-DEFAULT-PACKAGES]\n     [NO-PACKAGES]\n     [EXTRA]"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\Exercise{%s}" . "\\Exercise{%s}"))
        ;; This is a multiple TD class
        ("td" "\\documentclass{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\fpremark{%s}\\fpnewtd" . "\\fpremark{%s}\\fpnewtd")
         ;; ("\\fpremark{%s}" . "\\fpremark{%s}")
         ("\\Exercise{%s}" . "\\Exercise{%s}")
         ("\\begin{questiion} %% %s" "\\end{questiion}")
         ("\\begin{soluce} %% %s" "\\end{soluce}"))
        ;; This is a single TD class
        ;; FIXME: check html vs pdf exporter, especially question numbering
        ("tdsimple" "\\documentclass{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ;; ("\\fpremark{%s}\\fpnewtd" . "\\fpremark{%s}\\fpnewtd")
         ("\\Exercise{%s}" . "\\Exercise{%s}")
         ("\\begin{questiion} %% %s" "\\end{questiion}")
         ("\\begin{soluce} %% %s" "\\end{soluce}"))
        ("exam" "\\documentclass[addpoints,solutions,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         fp-org-latex-compute-exam-section
         )
        ("examshort" "\\documentclass[addpoints,solutions,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         fp-org-latex-compute-examshort-section
         )
        ("slides" "\\documentclass[presentation,compress,french,slides]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ; ("\\paragraph{%s}" . "\\paragraph*{%s}")
         )
        ("lecture" "\\documentclass[9pt,presentation,compress,french,lecture,slides]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ; ("\\paragraph{%s}" . "\\paragraph*{%s}")
         )
        ("springer" "\\documentclass{svjour3patch}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("llncs" "\\documentclass{llncs}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("IEEEtran" "\\documentclass{IEEEtran}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("llncs" "\\documentclass{llncs}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("acmproc" "\\documentclass{acm_proc_article-sp}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("my-letter"
         "\\documentclass\{scrlttr2\}\n\\usepackage[english]{babel}\n\\setkomavar{frombank}{(1234)\\,567\\,890}\n\[DEFAULT-PACKAGES]\n\[NO-PACKAGES]\n\[EXTRA]")
        ("ma-lettre"
         "\\documentclass\{scrlttr2\}\n\\usepackage[french]{babel}\n\\setkomavar{frombank}{(1234)\\,567\\,890}\n\[NO-DEFAULT-PACKAGES]\n\[NO-PACKAGES]\n\[EXTRA]")
        ("article" "\\documentclass[a4paper,french,article]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("report" "\\documentclass[11pt]{memoir}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("offrestage" "\\documentclass{memoir}\n     [NO-DEFAULT-PACKAGES]\n     [NO-PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("book" "\\documentclass[11pt]{memoir}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


;; ** Beamer

;; FIXME: why isn't this file loaded earlier?
(require 'ox-beamer)

;; Beamer
;; (setq org-beamer-outline-frame-title "Survol")
(add-to-list 'org-beamer-environments-extra
             '("only" "o" "\\only%a{%h%x" "}"))
(add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

;; ** Frame (beamer_act, not quite right)
;; *** The problem on this frame
;; I want Block 1 and Block 2 to occupy the same space on successive
;; slides.
;; *** OnlyEnv 1
;; :B_onlyenv:
;; :PROPERTIES:
;; :BEAMER_env: onlyenv
;; :BEAMER_ACT: <1>
;; :END:
;; **** Block 1
;; Text
;; *** OnlyEnv 2
;; :B_onlyenv:
;; :PROPERTIES:
;; :BEAMER_env: onlyenv
;; :BEAMER_ACT: <2>
;; :END:
;; **** Block 2
;; Text 2


;; ** HTML

(setq org-html-head "")
(setq org-html-inline-images t)
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)

(setq org-html-html5-fancy t)
(setq org-html-htmlize-output-type 'css)

(setq org-html-table-row-open-tag
      '(lambda (number group-number start-group-p end-group-p top-p bottom-p)
         (cond (top-p "<tr class=\"tr-top\">")
               (bottom-p "<tr class=\"tr-bottom\">")
               (t (if (= (mod number 2) 1)
                      "<tr class=\"tr-odd\">"
                    "<tr class=\"tr-even\">")))))

(setq org-html-table-row-close-tag
      "</tr>")

(setq org-html-special-string-regexps
      (append '(
                ("\\<LuaLaTeX\\>" . "LUA<span class=\"TypoLATEX\">L<span class=\"A\">A</span><span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span></span>")
                ("\\<LaTeX\\>" . "<span class=\"TypoLATEX\">L<span class=\"A\">A</span><span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span></span>")
                ("\\<LuaTeX\\>" . "LUA<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>")
                ("\\<ConTeXt\\>" . "CON<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>t")
                ("\\<TeX\\>" . "<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>")
                )
              org-html-special-string-regexps))


(defadvice org-html-convert-special-strings (around org-html-convert-special-strings-around)
  "Take case into account in `org-html-convert-special-strings'."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'org-html-convert-special-strings)


;; ** Publish

;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

(setq org-publish-timestamp-directory
      (expand-file-name  "org-timestamps/" fp-config-savefile-dir))

;; *** to PDF

;;: Define our own backend.
;;; Primary goal is to resolve links against our
;;; Org file structure.
;;; Links outside the subtree have to be resolved as
;;; html links pointing to the web site.
(org-export-define-derived-backend 'fp-latex 'latex
  :translate-alist '((link . fp-latex-link-translate)))

;;; Up to now, we fix the \includesvg command by
;;; switching back to \includegraphics from the same
;;; file but with pdf extension.
;;; The point is that I prefer to convert svg files to pdf
;;; myself using a batch file.
(defun fp-latex-link-translate (link contents info)
                                        ;(message "link = %s\ncontents = %s\ninfo = %s\n" link contents info)
  (let ((image-code ;; (org-export-with-backend 'latex link contents info)
         (org-latex-link link contents info)
         ))
    (setq image-code (replace-regexp-in-string "^\\\\includesvg\\([^{]*{[^}]*\\)}"
                                               "\\\\includegraphics\\1.pdf}"
                                               image-code
                                               nil nil))
    ))


(defun fp-org-notebook-export-function()
  (interactive)
  (let* ((export-file-name (org-export-output-file-name ".ipynb" t))
         )
    (message "Generating %s" export-file-name)
    ;; (org-export-to-file 'jupyter-notebook file nil t nil nil nil nil)
    (ox-ipynb-export-to-ipynb-file nil t nil nil nil)
    ))


(defun fp-export-all-notebook ()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (save-mark-and-excursion
     (goto-char (point-min))
     (org-map-entries #'fp-org-notebook-export-function "notebook" 'file))))


(defun fp-org-pdf-export-function()
  (interactive)
  (let* ((file (org-export-output-file-name ".tex" t))
         (dir (file-name-directory file))
         (outdir (if (and (> (length dir) 4)
                          (string= "pdf/" (substring dir -4 (length dir))))
                     dir
                   (expand-file-name "pdf/" dir)))
         (outfile (org-export-output-file-name ".tex" t outdir))
         )
    (message "Generating %s" outfile)
    (org-export-to-file 'fp-latex outfile nil t nil nil nil
                        (lambda (file) (org-latex-compile file)))))

(defun fp-export-all-pdf ()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (save-mark-and-excursion
     (goto-char (point-min))
     (org-map-entries #'fp-org-pdf-export-function "pdf" 'file))))

(defun fp-export-pdf ()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (save-mark-and-excursion
     (when (org-current-level)
       (unless (org-at-heading-p) (outline-back-to-heading))
       (loop until (or (= 1 (org-current-level))
                       (member "pdf" (org-get-tags-at)))
             do (outline-up-heading 1))
       (when (member "pdf" (org-get-tags-at))
         (fp-org-pdf-export-function))))))


;; *** to Reveal


(defun fp-export-current-slides ()
  (interactive)
  (let ((org-use-tag-inheritance nil)
        (fp-export-level 1)
        (fp-export-position 0)
        (fp-export-links (fp-gather-links-dest))
        (fp-export-root (fp-get-export-root))
        )
                                        ;, (fp-gather-links-dest)
    (org-map-entries #'(lambda ()
                         (org-export-to-file
                             'fp-reveal
                             (org-export-output-file-name ".html" t) nil t))
                     "slides" 'file)
    ;; (let ((org-export-before-processing-hook 'fp-remove-subtrees))
    ;;   (org-map-entries #'fp-org-html-export-function "webpage" 'file))
    ))

(defun fp-export-current-slides()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (org-map-entries
     #'(lambda ()
         (org-export-to-file
             'reveal
             (org-export-output-file-name ".html" t) nil t)) "slides" 'file)))

;; *** to HTML

;; TODO: http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

;;; I use a template scheme. My main template file is
;;; ~/Org/templates/template-page-normale.html
;;; This template page includes elisp code in <lisp> ... </lisp>
;;; tags that are evaluated when exporting.

;;; Taken from o-blog

(defun ob:eval-lisp()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
                       when publishing a page."
  (save-excursion
    (save-restriction
      (save-match-data
        ;; needed for thing-at-point
        (html-mode)
        (beginning-of-buffer)
        (let ((open-tag "<lisp>\\|{lisp}\\|\\[lisp\\]")
              (close-tag "</lisp>\\|{/lisp}\\|\\[/lisp\\]")
              beg end sexp)
          (while (search-forward-regexp open-tag nil t)
            (setq beg (- (point) (length  (match-string 0))))
            (when (search-forward-regexp close-tag nil t)
              (setq end (point))
              (backward-char (length (match-string 0)))
              (backward-sexp)
              (setq sexp (substring-no-properties (thing-at-point 'sexp)))
              ;; In some exporters (pandoc) " are replaced with &quot; which
              ;; breaks lisp interpolation.
              (with-temp-buffer
                (insert sexp)
                (goto-char (point-min))
                (while (search-forward "&quot;" nil t)
                  (replace-match "\"" nil t))
                (setq sexp (buffer-string)))
              (narrow-to-region beg end)
              (delete-region (point-min) (point-max))
              (insert
               (save-match-data
                 (condition-case err
                     (let ((object (eval (read sexp))))
                       (cond
                        ;; result is a string
                        ((stringp object) object)
                        ;; a list
                        ((and (listp object)
                              (not (eq object nil)))
                         (let ((string (pp-to-string object)))
                           (substring string 0 (1- (length string)))))
                        ;; a number
                        ((numberp object)
                         (number-to-string object))
                        ;; nil
                        ((eq object nil) "")
                        ;; otherwise
                        (t (pp-to-string object))))
                   ;; error handler
                   (error ; (debug error)
                    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
              (goto-char (point-min))
              (widen))))))))

(defun fp-export-body ()
  "Export a subtree to HTML using `fp-html' exporter, body only."
  (with-current-buffer org-buffer
    ;; (message "org-buffer = %s\n" (buffer-name org-buffer))
    (let* ((element (org-element-at-point))
           (tags (org-export-get-tags element nil))
           (text (org-element-property :title element))
           (level (1- (org-element-property :level element)))
           (body (org-export-as 'fp-html t nil t))
           (html 
            (if (or (= 0 level) (cl-intersection '("webjumbo" "webpage") tags :test #'equal))
                (format "<div class=\"jumbotron webjumbo\"><h1 class=\"thumbnail\">%s</h1>%s<br/></div>"
                 text body)
              (concatenate 'string
                           (format "<h%d>%s</h%d>" level text level)
                           "<br/>"
                           body))))
      (replace-regexp-in-string "<p>[[:space:]]*<br>[[:space:]]*</p>" "<br>" html))))

(defun fp-export-origo-body ()
  "Export a subtree to HTML using `fp-html' exporter, body only."
  (with-current-buffer org-buffer
    ;; (message "org-buffer = %s\n" (buffer-name org-buffer))
    (concatenate 'string
                 "<div class=\"col "
                 (if (fp-get-news)
                     "c8"
                   "c10")
                 "\">"
                 (org-export-as 'fp-html t nil t)
                 "</div>")))

(defun fp-get-news ())

(defun fp-export-origo-format-news (news)
  "")

(defun fp-export-origo-news ()
  (when (fp-get-news)
    (concatenate 'string
                 "<div class=\"col c2\""
                 (fp-export-origo-format-news (fp-get-news))
                 "</div>")))


(defvar fp-export-current-heading "")

(defun fp-export-origo-get-title ()
  (upcase fp-export-current-heading)
  )

(defun fp-org-html-export-function()
  "Main HTML export function for a subtree."
  (let ((org-buffer (current-buffer))
        (fp-export-current-heading (nth 4 (org-heading-components)))
        (template-buffer
         (find-file-noselect (or (and (fp-get-variable "HTML_TEMPLATE")
                                      (expand-file-name (fp-get-variable "HTML_TEMPLATE") "~/Org/"))
                                 "~/Org/templates/template-page-normale.html") t))
        (file (org-export-output-file-name ".html" t)))
    (incf fp-export-position)
    (message "org-heading-components: %S\n" (org-heading-components))
    (when file
      ;; (message "fp-org-html-export-function\n\tfile=%s\n\tfp-export-scope=%s" file fp-export-scope)
      (with-current-buffer (find-file-noselect file t)
        (erase-buffer)
        (insert-buffer-substring template-buffer)
        (ob:eval-lisp)
        (save-buffer)
        )
      )))

(defun fp-test ()
  (org-map-entries #'(lambda ()
                       (message (format "%s" (org-get-tags-at (point) t)))) "web" 'file))

(defun fp-export-current-document()
  (interactive)
  (let ((org-use-tag-inheritance nil)
        (fp-export-level 1)
        (fp-export-position 0)
        (fp-export-links (fp-gather-links-dest))
        (fp-export-root (fp-get-export-root))
        )
                                        ;, (fp-gather-links-dest)
    (let* ((fp-export-scope 'file)
           (org-use-tag-inheritance '("webpage" "ARCHIVE"))
           (previous-level 0)
           fp-export-menu
           )
      (org-map-entries #'(lambda ()
                           (when (cl-intersection '("web" "webmenu" "webpage") (org-get-tags-at (point) t) :test #'string-equal)
                             ;; (message "tags = %s" (org-get-tags-at (point) t))
                             (fp-build-menu)))
                       "webmenu-webpage-nopublic-ARCHIVE|web-webpage-nopublic-ARCHIVE|webtarget-ARCHIVE"
                       'file)
      ;; (message "menu=%s" fp-export-menu)
      (org-map-entries #'fp-org-html-export-function
                       ;; "web-webpage-nopublic|webmenu-webpage-nopublic"
                       "web-webpage-ARCHIVE"
                       'file))

    (org-map-entries #'(lambda ()
                         (let ((fp-export-scope 'tree)
                               (org-use-tag-inheritance '("webpage"))
                               (previous-level 0)
                               fp-export-menu)
                           ;;(fp-build-menu)
                           ;;(incf previous-level)
                           (org-map-entries #'(lambda () (fp-build-menu))
                                            "webmenu-nopublic-ARCHIVE|web-nopublic-ARCHIVE|webtarget-ARCHIVE"
                                            'tree)
                           (setq fp-export-menu (getf (car fp-export-menu) :submenu))
                           (org-map-entries #'fp-org-html-export-function
                                            ;; "web-nopublic|webmenu-nopublic"
                                            "web-nopublic-ARCHIVE"
                                            'tree)))
                     "webpage-ARCHIVE" 'file)
    ;; (let ((org-export-before-processing-hook 'fp-remove-subtrees))
    ;;   (org-map-entries #'fp-org-html-export-function "webpage" 'file))
    ))

(defun fp-export-get-ga-id ()
  (with-current-buffer org-buffer
    (or (fp-get-variable "HTML_GA_ID")
        "")))

(defun fp-export-title ()
  (with-current-buffer org-buffer
    (or (fp-get-variable "Title")
        "")))

(defun fp-export-description ()
  (with-current-buffer org-buffer
    (or (fp-get-variable "Description")
        "")))

(defun fp-export-keywords ()
  (with-current-buffer org-buffer
    (or (fp-get-variable "Keywords")
        "")))

(local-set-key (kbd "<f5>") #'fp-export-current-document)

;; ***** TODO enhance crossref when publishing

;; Look at the definition of `org-publish-org-to'

;; Add `org-publish--store-crossrefs' and
;; `org-publish-collect-index' to final output filters.
;; The latter isn't dependent on `:makeindex', since we
;; want to keep it up-to-date in cache anyway.
;; (org-combine-plists
;;  plist
;;  `(:crossrefs
;;    ,(org-publish-cache-get-file-property
;;      (expand-file-name filename) :crossrefs nil t)
;;    :filter-final-output
;;    (org-publish--store-crossrefs
;;     org-publish-collect-index
;;     ,@(plist-get plist :filter-final-output))))

;; This mechanism has to be added to my export functions.
;; Possibly copying the needed functions because I don't have
;; the full cache mechanism of ox-publish



;;; Exporting an Org buffer has to be done in 2 passes.
;;; An Org buffer may contain headings with web tags
;;; and headings with webpage tags.
;;; The former is the main/outer structure of the Org file
;;; while the later designates another tree that is referenced
;;; by the former.


;; Interesting filter. Simplify it to keep ignoreheading only for html.
(defun tsd-filter-headline-tags (contents backend info)
  "Ignore headlines with tag `ignoreheading' and/or start LaTeX
                section with `newpage' or `clearpage' command."
  (cond ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\`.*newpage.*\n" (downcase contents))
              ;; if you want to get rid of labels use the string
              ;; "\\`.*ignoreheading.*\n.*\n"
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "\\\\newpage\n" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\`.*clearpage.*\n" (downcase contents))
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "\\\\clearpage\n" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex 'html 'ascii)
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?newpage.*?}\\(.*\n\\)"
                            (downcase contents)))
         (replace-match "\\\\newpage\n\\1\\2"  nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?clearpage.*?}\\(.*\n\\)" (downcase contents)))
         (replace-match "\\\\clearpage\n\\1\\2"  nil nil contents))))

;; (add-to-list 'org-export-filter-headline-functions 'tsd-filter-headline-tags)

;;  (org-export-to-file 'html (org-export-output-file-name ".html" t) nil t nil t)
(defun fp-html-body-filter (data backend info)
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "<p>[\s\n]*</p>" "" data)))

(add-to-list 'org-export-filter-body-functions 'fp-html-body-filter)

;; (defun fp-html-link-filter (data backend info)
;;   "Handle id and custom_id links."
;;   (let* ((beg (next-property-change 0 data))
;;          (link (if beg (get-text-property beg :parent data))))
;;     (cond ((and link
;;                 (org-export-derived-backend-p backend 'html)
;;                 (member (org-element-property :type link) '("id" "custom_id")))
;;            (format "\\cite{%s}" (org-element-property :path link)))
;;           ((and link
;;                 (org-export-derived-backend-p backend 'latex)
;;                 (string= (org-element-property :type link) "file")
;;                 (string= (org-element-property :path link) "~/.bib.bib"))
;;            (format "\\cite{%s}" (org-element-property :search-option link)))
;;           (t data))))

(setq org-export-with-broken-links 'mark)
;; (setq org-export-with-broken-links 'user)
;; (setq org-export-user-broken-link-handler #'fp-html-export-link)


;; (defun fp-html-export-link (data info)
;;   (if (org-export-derived-backend-p backend 'html)
;;                                         ;      (message "data = %s\n" data)
;;       ))

;; (setq org-export-filter-link-functions '(fp-html-export-link-filter))

;;; Define our own export backend to be able to fix
;;; links destinations.

(org-export-define-derived-backend 'fp-html 'html
  :translate-alist '((link . fp-html-link-translate)
                     (headline . fp-html-headline-translate)))

(org-export-define-derived-backend 'fp-reveal 'reveal
  :translate-alist '((link . fp-html-link-translate)
                     (headline . fp-html-headline-translate)))

(defun fp-export-all-html-files ()
  (let ((files '("c:/Home/Org/AI.org"
                 ;; "c:/Home/Org/Agents.org"
                 "c:/Home/Org/AlgoCS.org"
                 "c:/Home/Org/BigData.org"
                 ;; "c:/Home/Org/Bookmarks.org"
                 ;; "c:/Home/Org/CV.org"
                 ;; "c:/Home/Org/Diary.org"
                 ;; "c:/Home/Org/EIAH.org"
                 "c:/Home/Org/FP-Supelec.org"
                 ;; "c:/Home/Org/HDR.org"
                 ;; "c:/Home/Org/Lettres.org"
                 ;; "c:/Home/Org/NewsMining.org"
                 "c:/Home/Org/PDL2A.org"
                 ;; "c:/Home/Org/Personal.org"
                 ;; "c:/Home/Org/Program-Samples.org"
                 ;; "c:/Home/Org/Programming.org"
                 ;; "c:/Home/Org/Publications.org"
                 ;; "c:/Home/Org/Research.org"
                 "c:/Home/Org/Software.org"
                 ;; "c:/Home/Org/Students.org"
                 "c:/Home/Org/THCS.org"
                 ;; "c:/Home/Org/Teaching.org"
                 ;; "c:/Home/Org/Worklog.org"
                 ;; "c:/Home/Org/foo-1.org"
                 ;; "c:/Home/Org/foo.org"
                 ;; "c:/Home/Org/notes.org"
                 ;; "c:/Home/Org/org-reveal-example.org"
                 )))
    (with-current-buffer (current-buffer)
      (save-excursion
        (save-window-excursion
          (dolist (f files nil)
            (org-open-file f t nil)
            (fp-export-current-document)))))
    )
  )

;; ***** Links destinations

(defvar fp-export-links ())

(defun fp-get-dest-filename-at ()
  (cl-loop
   when (org--property-local-values "EXPORT_FILE_NAME" nil)
   do (return (concat "../" (car (org--property-local-values "EXPORT_FILE_NAME" nil))))
   if (> (org-current-level) 1)
   do (outline-up-heading 1 t)
   else do (return nil))
  )

(defun fp-get-link-dest-filename-at ()
  (let* ((context
          ;; Only consider supported types, even if they are not
          ;; the closest one.
          (org-element-lineage
           (org-element-context)
           '(clock comment comment-block footnote-definition
                   footnote-reference headline inlinetask keyword link
                   node-property timestamp)
           t))
         (type (org-element-type context))
         (value (org-element-property :value context))
         (reference-buffer (current-buffer))
         arg
         target-filename)
    (message "type = %s value = %s\n" type value)
    (cond
     ((not context) (error "No link found"))
     ;; Exception: open timestamps and links in properties
     ;; drawers, keywords and comments.
     ;; ((memq type '(comment comment-block keyword node-property))
     ;;  (call-interactively #'org-open-at-point-global))
     ;; On a headline or an inlinetask, but not on a timestamp,
     ;; a link, a footnote reference or on tags.
     ;; ((and (memq type '(headline inlinetask))
     ;;       ;; Not on tags.
     ;;       (let ((case-fold-search nil))
     ;;         (save-excursion
     ;;           (beginning-of-line)
     ;;           (looking-at org-complex-heading-regexp))
     ;;         (or (not (match-beginning 5))
     ;;             (< (point) (match-beginning 5)))))
     ;;  (let* ((data (org-offer-links-in-entry (current-buffer) (point) arg))
     ;;         (links (car data))
     ;;         (links-end (cdr data)))
     ;;    (if links
     ;;        (dolist (link (if (stringp links) (list links) links))
     ;;          (search-forward link nil links-end)
     ;;          (goto-char (match-beginning 0))
     ;;          (org-open-at-point))
     ;;      (require 'org-attach)
     ;;      (org-attach-reveal 'if-exists))))
     ;; ;; On a clock line, make sure point is on the timestamp
     ;; ;; before opening it.
     ;; ((and (eq type 'clock)
     ;;       value
     ;;       (>= (point) (org-element-property :begin value))
     ;;       (<= (point) (org-element-property :end value)))
     ;;  (org-follow-timestamp-link))
     ;; ;; Do nothing on white spaces after an object.
     ((>= (point)
          (save-excursion
            (goto-char (org-element-property :end context))
            (skip-chars-backward " \t")
            (point)))
       (error "No link found"))
     ;; ((eq type 'timestamp) (org-follow-timestamp-link))
     ;; ;; On tags within a headline or an inlinetask.
     ;; ((and (memq type '(headline inlinetask))
     ;;       (let ((case-fold-search nil))
     ;;         (save-excursion (beginning-of-line)
     ;;                         (looking-at org-complex-heading-regexp))
     ;;         (and (match-beginning 5)
     ;;              (>= (point) (match-beginning 5)))))
     ;;  (org-tags-view arg (substring (match-string 5) 0 -1)))
     ((eq type 'link)
      ;; When link is located within the description of another
      ;; link (e.g., an inline image), always open the parent
      ;; link.
      (let* ((link (let ((up (org-element-property :parent context)))
                     (if (eq (org-element-type up) 'link) up context)))
             (type (org-element-property :type link))
             (path (org-link-unescape (org-element-property :path link))))
        ;; Switch back to REFERENCE-BUFFER needed when called in
        ;; a temporary buffer through `org-open-link-from-string'.
        (with-current-buffer (or reference-buffer (current-buffer))
          (cond
           ((equal type "file")
            (if (string-match "[*?{]" (file-name-nondirectory path))
                (dired path)
              ;; Look into `org-link-parameters' in order to find
              ;; a DEDICATED-FUNCTION to open file.  The function
              ;; will be applied on raw link instead of parsed
              ;; link due to the limitation in `org-add-link-type'
              ;; ("open" function called with a single argument).
              ;; If no such function is found, fallback to
              ;; `org-open-file'.
              (let* ((option (org-element-property :search-option link))
                     (app (org-element-property :application link))
                     (dedicated-function
                      (org-link-get-parameter
                       (if app (concat type "+" app) type)
                       :follow)))
                (if dedicated-function
                    (funcall dedicated-function
                             (concat path
                                     (and option (concat "::" option))))
                  (apply #'org-open-file
                         path
                         (cond (arg)
                               ((equal app "emacs") 'emacs)
                               ((equal app "sys") 'system))
                         (cond ((not option) nil)
                               ((string-match-p "\\`[0-9]+\\'" option)
                                (list (string-to-number option)))
                               (t (list nil
                                        (org-link-unescape option)))))))))
           ((functionp (org-link-get-parameter type :follow))
            (funcall (org-link-get-parameter type :follow) path))
           ((member type '("coderef" "custom-id" "fuzzy" "radio"))
            (message "custom-id or fuzzy link %s\n" link)
            (unless (run-hook-with-args-until-success
                     'org-open-link-functions path)
              (if (not arg) (org-mark-ring-push)
                (switch-to-buffer-other-window
                 (org-get-buffer-for-internal-link (current-buffer))))
              (let ((destination
                     (org-with-wide-buffer
                      (if (equal type "radio")
                          (org-search-radio-target
                           (org-element-property :path link))
                        (org-link-search
                         (if (member type '("custom-id" "coderef"))
                             (org-element-property :raw-link link)
                           path)
                         ;; Prevent fuzzy links from matching
                         ;; themselves.
                         (and (equal type "fuzzy")
                              (+ 2 (org-element-property :begin link)))))
                      (point))))
                (unless (and (<= (point-min) destination)
                             (>= (point-max) destination))
                  (widen))
                (goto-char destination))))
           (t (browse-url-at-point)))
          (save-excursion
            (setq target-filename
                  (cl-loop
                   when (org--property-local-values "EXPORT_FILE_NAME" nil)
                   do (return (concat "../" (car (org--property-local-values "EXPORT_FILE_NAME" nil))))
                   if (> (org-current-level) 1)
                   do (outline-up-heading 1 t)
                   else do (return nil))))
          )))
     ;; On a footnote reference or at a footnote definition's label.
     ((or (eq type 'footnote-reference)
          (and (eq type 'footnote-definition)
               (save-excursion
                 ;; Do not validate action when point is on the
                 ;; spaces right after the footnote label, in
                 ;; order to be on par with behaviour on links.
                 (skip-chars-forward " \t")
                 (let ((begin
                        (org-element-property :contents-begin context)))
                   (if begin (< (point) begin)
                     (= (org-element-property :post-affiliated context)
                        (line-beginning-position)))))))
      (org-footnote-action))
     (t (error "No link found")))
    target-filename)
  )

(defun fp-print-link (link)
  (let* (
         (type (org-element-property :type link))
         (raw-link (org-element-property :raw-link link))
         (cbeg (org-element-property :contents-begin link))
         (cend (org-element-property :contents-end link))
         (path (org-element-property :path link))
         (search-option (org-element-property :search-option link))
         (contents (or (and cbeg cend (buffer-substring cbeg cend))
                       "")
                   ))
    (message "type = %s raw-link = %s contents = %s path = %s search-option = %s" type raw-link contents path search-option)))

(defun fp-test ()
  (save-restriction
    (save-excursion
      (widen)
      (org-element-map (org-element-parse-buffer) 'link
        #'(lambda (l) (fp-print-link l))
        )))
  )

;; Something is wrong here.
;; The formatting shouldn't happen there.
;; Especially, referencing images shouldn't be
;; processed at this time.
(defun fp-gather-links-dest ()
  ""
  (let (links
        (ht-links (make-hash-table :test #'equal)))
    (save-restriction
      (save-excursion
        (widen)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (elt)
            ;; (message "prop = %s %s" (org-element-property :type elt) (org-element-property :raw-link elt))
            (fp-print-link elt)
            ;; Shouldn't file be part of it ?
            (if (or  (member (org-element-property :type elt) '("custom-id" "fuzzy"))
                     (org-element-property :search-option elt))
                (push elt links))))
        (mapc (lambda (link)
                (goto-char (org-element-property :begin link))
                (message "**Trying to link ") (fp-print-link link)
                (let* ((type (org-element-property :type link))
                       (sourcefile
                        (save-excursion (fp-get-dest-filename-at)))
                       (destfile
                        (if (member type '("fuzzy" "file"))
                            (with-current-buffer (current-buffer)
                              (save-excursion
                                (save-window-excursion
                                  (fp-get-link-dest-filename-at))))
                          sourcefile))
                       (search-option (org-link-unescape (or (org-element-property :search-option link)
                                                             (org-element-property :path link))))
                       (raw-link (org-element-property :raw-link link))
                       (cbeg (org-element-property :contents-begin link))
                       (cend (org-element-property :contents-end link))
                       (contents (replace-regexp-in-string
                                  "^file:" "/"
                                  (or (and cbeg cend
                                           (buffer-substring cbeg cend))
                                      raw-link) t t))
                       )

                  (message "**Linked type = %s raw-link = %s contents = %s source = %s -> dest = %s\n" type raw-link contents sourcefile destfile)
                  ;; Beware: only image filetypes!
                  ;; See org-html-inline-image-rules
                  (cond
                   ((and ;; (member type '("http" "https"))
                     ;; (assoc type org-html-inline-image-rules)
                     (string-match (cdr (assoc "http" org-html-inline-image-rules)) contents))
                    (if (equal destfile sourcefile)
                        (setf (gethash raw-link ht-links)
                              (format "<a href=\"%s\"><img src=\"%s\"/></a>" raw-link contents))
                      (setf (gethash raw-link ht-links)
                            (format "<a href=\"http:%s%s\"><img src=\"%s\"/></a>" destfile raw-link contents))
                      ))
                   (t (setf (gethash raw-link ht-links)
                            (if (string-match "^#"  search-option)
                                (format "<a href=\"%s%s\">%s</a>" destfile search-option contents)
                              (format "<a href=\"%s#%s\">%s</a>" destfile search-option contents))))
                   )))
              links)))
    ht-links))


;; Here we should translate what is needed, i.e. the target of the link
(defun fp-html-link-translate (link contents info)
  (or (gethash (org-element-property :raw-link link) fp-export-links)
      (org-html-link link contents info)))

;; FIXME: hard to swap Question with numbers
(defun fp-html-headline-translate (headline contents info)
  (let* (
         ;; (text (org-export-data (org-element-property :title headline) info))
         ;; (full-text (funcall (plist-get info :html-format-headline-function)
         ;;   todo todo-type priority text tags info))
         (tags (org-export-get-tags headline info))
         )
    (if (member "webclip" tags)
        (let*
            ((level (+ (org-export-get-relative-level headline info)
                       (1- (plist-get info :html-toplevel-hlevel))))
             (author (org-element-property :AUTHOR headline))
             (url (org-element-property :URL headline))
             (year (org-element-property :YEAR headline))
             (image (org-element-property :IMAGE headline))
             (text (org-export-data (org-element-property :title headline) info))
             (contents (or contents ""))
             (first-content (car (org-element-contents headline)))
             (extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
             )
          (message "url=%s" url)
          (format "<div id=\"%s\" class=\"webclip %s\">\n<a href=\"%s\">%s</a>\n%s</div>"
                  (concat "outline-container-"
			  (org-export-get-reference headline info))
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  url
                  (format "\n<h%d>%s</h%d>\n"
                          level text level)
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  )
            )
        (replace-regexp-in-string "<span class=\"section-number-\\([0-9]+\\)\">\\([0-9\\.]+\\)</span>\\(\\s-\\|\n\\)+Question"
                                  "Question <span class=\"section-number-\\1\">\\2</span>"
                                  (org-html-headline headline contents info) nil nil))))


;; (defun fp-remove-subtrees (backend)
;;   (let (beg)
;;     (widen)
;;     (org-previous-visible-heading 1)
;;     (setq beg (point))
;;     (org-next-visible-heading 1)
;;     (backward-char)
;;     (narrow-to-region beg (point))))



;; ***** Menus

(defun fp-test-menu (scope)
  (let ((org-buffer (current-buffer))
        (org-use-tag-inheritance '("webpage"))
        (org-trust-scanner-tags t)
        (previous-level 1)
        (count 0)
        (org-agenda-skip-function  ))
    (org-map-entries #'(lambda ()
                                        ;                         (when (string-match ":webpage" org-scanner-tags)
                                        ;                        (setq org-map-continue-from)
                                        ;                         )
                         (when (car (org--property-local-values "EXPORT_FILE_NAME" nil))
                           (list  :link (car (org--property-local-values "EXPORT_FILE_NAME" nil)))))
                     "web-webpage-nopublic|webmenu-webpage-nopublic|webpage-web-nopublic"
                     scope
                     #'(lambda () (org-agenda-skip-subtree-if 'regexp "webpage+web"))
                     ))
  )


(defvar fp-export-menu nil)

(defvar fp-export-level 1)
(defvar fp-export-position 0)

;;; No tag inheritance here

(defun fp-under-tag-p (tag)
  (let ((org-use-tag-inheritance t))
    (not (null (member tag (org-get-tags-at)))))
  )

(defvar fp-export-scope nil)

;;; We should export webpage subtrees on their own only.  We should
;;; ignore them when exporting the main org file web pages.  However,
;;; we should take webpage subtrees into account when building the main
;;; navigation menu: it is the link towards the subtree.
;;; Question: where is the link back? In the properties?

;;; Actually, it would be simpler if the subtree had a `home'
;;; section. This way, building the menu for 'tree scope would
;;; be the same has for 'file scope.

(defun fp-export-menu ()
  (let* ( ;; (org-buffer (current-buffer))
         (org-use-tag-inheritance '("webpage" "ARCHIVE"))
         (previous-level 1)
         (count 0)
         (fp-export-level 1)
         (fp-export-position 0)
         menu
         html-menu)
    (with-current-buffer org-buffer
      (save-mark-and-excursion
       (when (eq fp-export-scope 'tree)
         (fp-build-menu)
         (incf previous-level)
         (org-map-entries #'(lambda () (fp-build-menu)) "webmenu-nopublic-ARCHIVE|web-nopublic-ARCHIVE" fp-export-scope
                          ;; (lambda () (org-agenda-skip-subtree-if 'regexp "webpage"))
                          ))
       (when (eq fp-export-scope 'file)
         (org-map-entries #'(lambda () (fp-build-menu)) "webmenu-nopublic-ARCHIVE|web-webpage-nopublic-ARCHIVE|webpage-web-nopublic-ARCHIVE" fp-export-scope
                          ;; (lambda () (org-agenda-skip-subtree-if 'regexp "webpage"))
                          ))))
    (setq html-menu
          ;; (fp-build-menu-format-menu
          ;;  (if (fp-get-webpage-at fp-export-menu fp-export-level fp-export-position)
          ;;      (fp-get-level fp-export-menu fp-export-level fp-export-position)
          ;;    (fp-get-level-0 fp-export-menu)))
          (fp-build-menu-format-menu menu)
          )
    (message "fp-export-menu %s\n" menu)
                                        ;(message "%s %s\n" fp-export-level fp-export-position)
                                        ;(message "%s\n" html-menu)
    html-menu
    ))

(defun fp-export-menu ()
  (fp-build-menu-format-menu fp-export-menu))

(defun fp-export-origo-menu ()
  (fp-build-menu-origo-format-menu fp-export-menu))

(defun fp-export-material-get (part)
  (case part
    (:title
     (with-current-buffer org-buffer
       (or (fp-get-variable "Title")
           "")))
    (:icon
     (with-current-buffer org-buffer
       (or (fp-get-variable "HTML_ICON")
           "")))
    (:description
      (with-current-buffer org-buffer
        (or (fp-get-variable "Description")
            "")))
    (:url
     (concat "/"
      (org-export-output-file-name ".html" t)))
    (:author
      (with-current-buffer org-buffer
        (or (fp-get-variable "Author")
        "")))
    (:menu
     (fp-build-menu-material-format-menu fp-export-menu))
    (:ga
     (with-current-buffer org-buffer
       (or (fp-get-variable "HTML_GA_ID")
           "")))
    (:body
     (with-current-buffer org-buffer
       ;; (message "org-buffer = %s\n" (buffer-name org-buffer))
       (org-export-as 'fp-html t nil t)))
    (:palette
     (with-current-buffer org-buffer
       (let ((palette (fp-get-variable "HTML_PALETTE")))
         (or (and palette (downcase palette))
             "grey"))))
    (:accent
     (with-current-buffer org-buffer
       (let ((accent (fp-get-variable "HTML_ACCENT")))
         (or (and accent (downcase accent))
             "yellow")))))
  )


(defun fp-get-webpage-at (menu level pos)
  (when menu
    (cond ((and ; (eq level (getf (car menu) :level))
            (eq pos (getf (car menu) :count)))
           (getf (car menu) :webpage))
          (t (or (and  (getf (car menu) :submenu)
                       (fp-get-webpage-at (getf (car menu) :submenu) level pos))
                 (fp-get-webpage-at (cdr menu) level pos)))))
  )

(defun fp-get-at (menu level pos fn)
  (when menu
    (cond ((and (eq level (getf (car menu) :level))
                (eq pos (getf (car menu) :count)))
           (funcall fn menu level pos))
          ((getf (car menu) :submenu)
           (fp-get-at (getf (car menu) :submenu) level pos fn))
          (t (fp-get-at (cdr menu) level pos fn))))
  )

(defun fp-get-level-0 (menu)
  (loop for item in menu
        collect (if (getf item :webpage)
                    (car (getf item :submenu))
                  item))
  )

(defun fp-get-level (menu level pos)
  (when menu
    (cond
     ((cl-member-if #'(lambda (elt)
                        (and ; (eq (getf elt :level) level)
                         (eq (getf elt :count) pos)))
                    (getf (car menu) :submenu))
      (getf (car menu) :submenu))
     (t (or (and (getf (car menu) :submenu)
                 (fp-get-level (getf (car menu) :submenu) level pos))
            (fp-get-level (cdr menu) level pos))))
    ))

(defun fp-build-menu-push-at-right-place (l new current previous)
  ;; (message "l = %s\nnew = %s\ncurrent = %s\nprevious = %s\n" l new current previous)
  (let ((ll (last l))
        (bl nil))
    (while (and ll (member :submenu (car ll)))
      (setq bl ll)
      (setq ll (last (cadr (member :submenu (car ll))))))
    (cond ((null ll) (setq l new))
          ((> current previous)
           (nconc (car ll) (list :submenu new)))
          ((< current previous)
           (nconc bl new))
          (t (nconc ll new)))
    ;; (message "=> %s\n" l)
    l))

(defun fp-build-menu ()
  (let* ((current-level (org-current-level))
         (item (list (list :text (car (org--property-local-values "EXPORT_HTML_MENU_TEXT" nil))
                           :icon (car (org--property-local-values "EXPORT_HTML_MENU_ICON" nil))
                           :link (concat "/" (car (org--property-local-values "EXPORT_FILE_NAME" nil)))
                           :level current-level
                           :count (incf fp-export-position)
                           :webpage (fp-under-tag-p "webpage")))))
    (setq fp-export-menu
          (fp-build-menu-push-at-right-place fp-export-menu item current-level previous-level))
    (setq previous-level current-level)
    fp-export-menu
    ))
;; (org-map-entries #'(lambda () (org-element-property :title (org-element-at-point))  ) "webmenu|web" 'file
;;                  #'(lambda () (org-agenda-skip-subtree-if 'regexp ":webpage:")))

;; (org-map-entries #'(lambda ()
;;                      (show-subtree)
;;                      (org-next-visible-heading 1)
;;                      (list :text (car (org--property-local-values "EXPORT_HTML_MENU_TEXT" nil))
;;                            :icon (car (org--property-local-values "EXPORT_HTML_MENU_ICON" nil))
;;                            :link (concat "/" (car (org--property-local-values "EXPORT_FILE_NAME" nil)))))
;;                  "webpage" 'file)

(defun fp-build-menu-format-menu (menu)
  (apply #'concatenate 'string
         (loop for entry in menu
               collect (fp-build-menu-format-entry entry))))

(defun fp-build-menu-origo-format-menu (menu)
  (apply #'concatenate 'string
         (loop for entry in menu
               collect (fp-build-menu-origo-format-entry entry))))

(defun fp-build-menu-material-format-menu (menu)
  (apply #'concatenate 'string
         (loop for entry in menu
               collect (fp-build-menu-material-format-entry entry))))

(defun fp-build-menu-format-entry (entry)
  (let ((submenu (and (member :submenu entry)
                      (fp-build-menu-format-menu (cadr (member :submenu entry))))))
    (concatenate 'string
                 (concatenate 'string
                              "<li><a href=\""
                              (or (and
                                   (not submenu)
                                   (cadr (member :link entry))
                                   ) "#")
                              "\""
                              (when submenu
                                " data-toggle=\"collapse\" data-target=\"#submenu\"")
                              ">"
                              (when (member :icon entry)
                                (concatenate 'string "<i class=\"fa fa-2x " (cadr (member :icon entry)) "\"></i>"))
                              (or (cadr (member :text entry)) "")
                              ;;(when submenu
                              ;;  "<span class=\"fa fa-2x fa-carret-down\"/>")
                              "</a>"
                              (when submenu
                                (concatenate 'string "<ul class=\"nav nav-second-level collapse\" id=\"submenu\">"
                                             submenu "</ul>"))
                              "</li>"
                              ))))

(defun fp-build-menu-origo-format-entry (entry)
  (let ((submenu (and (member :submenu entry)
                      (fp-build-menu-origo-format-menu (cadr (member :submenu entry))))))
    (concatenate 'string
                 (concatenate 'string
                              "<li><a href=\""
                              (or (and
                                   (not submenu)
                                   (cadr (member :link entry))
                                   ) "#")
                              "\""
                              " title=\""
                              (or (cadr (member :text entry)) "")
                              "\">"
                              (or (cadr (member :text entry)) "")
                              "</a>"
                              (when submenu
                                (concatenate 'string "<ul class=\"subpages\">"
                                             submenu "</ul>"))
                              "</li>"
                              ))))

(defun fp-build-menu-material-format-entry (entry)
  (let ((submenu (and (member :submenu entry)
                      (fp-build-menu-material-format-menu (cadr (member :submenu entry))))))
    (message "submenu = %s" submenu)
    (message "entry = %s" entry)
    (concatenate 'string
                 (concatenate 'string
                              "<li><a href=\""
                              (or (and
                                   (not submenu)
                                   (cadr (member :link entry))
                                   ) "#")
                              "\" "
                              (when submenu
                                "class=\"current\" ")
                              "title=\""
                              (or (cadr (member :text entry)) "")
                              "\">"
                              (or (cadr (member :text entry)) "")
                              ;;(when submenu
                              ;;  "<span class=\"fa fa-2x fa-carret-down\"/>")
                              "</a>"
                              (when submenu
                                (concatenate 'string "<ul class=\"scrollspy\">"
                                             (replace-regexp-in-string "<li>" "<li class=\"anchor\">" submenu) "</ul>"))
                              "</li>"
                              ))))

(defun fp-get-variable (var)
  (or
   (org-entry-get (point) (concat "EXPORT_" var) t)
   (save-mark-and-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+" var ":\\s-*\\(.*\\)\\s-*$") nil t)
      (match-string 1)))))

(defun fp-export-profile ()
  (with-current-buffer org-buffer
    (concat  (format "<div class=\"course hidden-xs\" style=\"background-image:url(%s%s)\"></div>\n"
                     (fp-get-export-root)
                     (fp-get-variable "HTML_ICON"))
             (format "<div class=\"title\">\n
                                 <h2>%s</h2>\n
                                       <img src=\"/images/Logo-CentraleSupelec-small.png\"/>\n
                                  </div>\n"
                     (fp-get-variable "HTML_TITLE")
                     ))))

(defvar fp-export-root nil)

(defun fp-get-export-root ()
  (or fp-export-root
      (fp-get-variable "HTML_ROOT")))

(defun fp-export-colophon ()
  "©<small>2017<br/><a href=\"/popineau/index.html\">Fabrice Popineau</a></small>")

(defmacro by-backend (&rest body)
  `(cl-case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))



;; Save images from clipboard

;; http://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it

(defun org-insert-image-from-clipboard ()
  (interactive)
  (let* ((the-dir (file-name-directory buffer-file-name))
         (attachments-dir (expand-file-name
                           (expand-file-name "figures/"
                                             (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                           the-dir))
         (png-file-name (format-time-string "%a%d%b%Y_%H%M%S.png"))
         (png-path (concat attachments-dir "/" png-file-name))
         (temp-buffer-name "CbImage2File-buffer"))
    (call-process "d:/source/emacs/CbImageToFile/CbImageToFile.exe" nil temp-buffer-name nil png-path)
    (let ((result (with-current-buffer temp-buffer-name (buffer-string))))
      (progn
        (kill-buffer temp-buffer-name)
        (if (string= result "")
            (progn
              (insert (concat "[[./figures/" png-file-name "]]"))
              (org-display-inline-images))
          (insert result))))))

(define-key org-mode-map (kbd "C-S-v") 'org-insert-image-from-clipboard)


;;

;; https://mail.google.com/mail/u/0/#inbox/156fdea424e9f2a7

(defun cpit/filter-begin-only (type)
  "Remove BEGIN_ONLY %s blocks whose %s doesn't equal TYPE.
For those that match, only remove the delimiters.

On the flip side, for BEGIN_EXCEPT %s blocks, remove those if %s equals TYPE. "
  (goto-char (point-min))
  (while (re-search-forward " *#\\+BEGIN_\\(ONLY\\|EXCEPT\\) +\\([a-z]+\\)\n" nil t)
    (let ((only-or-export (match-string-no-properties 1))
          (block-type (match-string-no-properties 2))
          (begin-from (match-beginning 0))
          (begin-to (match-end 0)))
      (re-search-forward (format " *#\\+END_%s +%s\n" only-or-export block-type))
      (let ((end-from (match-beginning 0))
            (end-to (match-end 0)))
        (if (or (and (string= "ONLY" only-or-export)
                     (string= type block-type))
                (and (string= "EXCEPT" only-or-export)
                     (not (string= type block-type))))
            (progn                      ; Keep the block,
                                        ; delete just the comment markers
              (delete-region end-from end-to)
              (delete-region begin-from begin-to))
          ;; Delete the block
          (message "Removing %s block" block-type)
          (delete-region begin-from end-to))))))
(add-hook 'org-export-before-process #'cpit/filter-begin-only)

;; ** Org Clock
;; https://github.com/jbranso/.emacs.d/blob/master/lisp/init-org.org#my-org-capure-templates
(use-package org-clock
  :defer t
  :ensure nil
  :pin manual
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persistence-insinuate t)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STRT")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t))
;; ** Org page
;; Org-page - A package for generate a static blog.

;; (use-package org-page
;;   :ensure t
;;   :bind (("C-x C-a p" . op/do-publication-and-preview-site)
;;          ("C-x C-a C-p" . op/do-publication)
;;          ("C-x C-a C-n" . op/new-post))
;;   :config
;;   (setq op/repository-directory "c:/Users/Nasser/OneDrive/nasseralkmim.github.io/")
;;   (setq op/site-domain "http://nasseralkmim.github.io/")
;;   (setq op/personal-disqus-shortname "nasseralkmim")
;;   (setq op/site-main-title "Nasser Alkmim")
;;   (setq op/site-sub-title "~/-")
;;   (setq op/personal-github-link "https://github.com/nasseralkmim")
;;   (setq op/personal-google-analytics-id "74704246")

;;   (setq op/category-ignore-list '("themes" "assets" "blog"))

;;   (setq op/category-config-alist
;;         '(("blog" ;; this is the default configuration
;;            :label "Notes"
;;            :show-meta t
;;            :show-comment t
;;            :uri-generator op/generate-uri
;;            :uri-template "/blog/%y/%m/%d/%t/"
;;            :sort-by :date     ;; how to sort the posts
;;            :category-index t) ;; generate category index or not
;;           ("index"
;;            :show-meta nil
;;            :show-comment nil
;;            :uri-generator op/generate-uri
;;            :uri-template "/"
;;            :sort-by :date
;;            :category-index nil)
;;           ("about"
;;            :show-meta nil
;;            :show-comment nil
;;            :uri-generator op/generate-uri
;;            :uri-template "/about/"
;;            :sort-by :date
;;            :category-index nil))))


;; * Desktop

;; This functionality is provided by desktop-save-mode (“feature”
;; name: “desktop”). The mode is not on by default in emacs 23.1, and
;; has a lot options. The following is init settings for the mode for
;; ErgoEmacs

;; Goal: have emacs always auto open the set of opened files in last
;; session, even if emacs crashed in last session or the OS crashed in
;; last session. Also, don't bother users by asking questions like “do
;; you want to save desktop?” or “do you want to override last session
;; file?”, because these are annoying and terms like “session” or
;; “desktop” are confusing to most users because it can have many
;; meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕
;; at the variable “user-emacs-directory” (default value is
;; “~/.emacs.d/”).  This file is our desktop file. It will be auto
;; created and or over-written.  if a emacs expert has other desktop
;; session files elsewhere, he can still use or manage those.

(use-package desktop
  :ensure nil
  :pin manual
  :init
  ;; Automatically save and restore sessions
  (setq desktop-dirname             "~/.emacs.d/savefile/"
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "lock"
        desktop-path                (list desktop-dirname)
        desktop-save                t
        desktop-files-not-to-save   "^$" ;reload tramp paths
        desktop-load-locked-desktop nil
        ;; desktop-buffers-not-to-save
        ;; (concat "\\("
        ;;         "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
        ;;         "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
        ;;         "\\)$")
        desktop-auto-save-timeout   30)
  (desktop-save-mode +1)
  :config
  ;; desktop: don't save flyspell and flycheck
  (add-to-list 'desktop-minor-mode-table
               '(flyspell-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(flycheck-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(flymake-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(undo-tree-mode nil))
  )



;; * Window management

;; Emacs is stripped of all power to split frames.
;; http://www.reflexivereflection.com/posts/2018-04-06-disabling-emacs-window-management.html

(setq display-buffer-alist
      '((popwin:display-buffer-condition popwin:display-buffer-action)
        ("shell.*" (display-buffer-same-window) ())
        (".*" (display-buffer-reuse-window
               display-buffer-same-window
               display-buffer-reuse-mode-window
               display-buffer-use-some-window)
         (reusable-frames . t))))

;; (defun anders/same-window-instead
;;     (orig-fun buffer alist)
;;   (display-buffer-same-window buffer nil))
;; (advice-add 'display-buffer-pop-up-window :around 'anders/same-window-instead)

;; (advice-remove 'display-buffer-pop-up-window 'anders/same-window-instead)

;; (defun anders/do-select-frame (orig-fun buffer &rest args)
;;   (let* ((old-frame (selected-frame))
;;          (window (apply orig-fun buffer args))
;;          (frame (window-frame window)))
;;     (unless (eq frame old-frame)
;;       (select-frame-set-input-focus frame))
;;     (select-window window)
;;     window))

;; (advice-add 'display-buffer :around 'anders/do-select-frame)
;; (advice-remove 'display-buffer 'anders/do-select-frame)

;; (setq frame-auto-hide-function 'delete-frame)

;; (advice-add 'set-window-dedicated-p :around
;;             (lambda (orig-fun &rest args) nil))

;; (use-package e2wm
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-+") 'e2wm:start-management))


;; * Info path
(use-package info
  :ensure nil
  :pin manual
  :config
  (setq Info-additional-directory-list '("c:/Local/MSys64/mingw64/share/info" "c:/Local/MSys64/usr/share/info"))

  (add-to-list 'Info-directory-list "d:/source/emacs/elisp/org-mode/doc/"))


;; * Secrets

;; Various personal ids/keys/email/etc.

(let ((secrets-file (expand-file-name "var/secrets.el" fp-config-dir)))
  (when (file-exists-p secrets-file)
    (load secrets-file)))

;; * Various
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'autopair-newline 'disabled nil)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(advice-add 'upcase-region
            :around
  '(lambda (oldfun &rest args)
     "Only apply upcase-region when region active."
     (when (region-active-p)
       (apply oldfun args))))
(advice-add 'downcase-region
            :around
  '(lambda (oldfun &rest args)
     "Only apply downcase-region when region active."
     (when (region-active-p)
       (apply oldfun args))))

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
