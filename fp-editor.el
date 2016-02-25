;;; fp-editor.el --- Emacs Prelude: Fabrice Popineau preload configuration.
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


;;; * Packages
(defvar fp-config-packages
  '(ag
    auctex
    auto-complete
    bookmark+
    dictionary
    dired+
    ediprolog
    exec-path-from-shell
    framemove
                                        ; git-commit-mode
    highlight-symbol
    htmlize
    js2-mode
    lua-mode
    ;; magithub
    ;; markdown-mode markdown-mode+ melpa
    memory-usage
    oauth2
    page-break-lines
    pcache
    popup
    ;; python-pep8 python-pylint
    solarized-theme
    ;; sumatra-forward
    tuareg
    writegood-mode
    ;; yaml-mode
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(prelude-require-packages fp-config-packages)


(setq enable-local-variables :all)


(require 'page-break-lines)

(global-page-break-lines-mode 1)


;;; * Backups

;; (setq backup-by-copying t)
(setq backup-by-copying-when-linked t)


;;; * Dired

;; Don't use any ls.exe
(setq ls-lisp-use-insert-directory-program nil)
;; Sort the same way as Unix ls does
(setq ls-lisp-UCA-like-collation nil)
;; Use human readable sizes
(setq dired-listing-switches "-alh")

(setq dired-guess-shell-alist-user
      '(; ("\\.pdf\\'" "start")
        ("\\.pdf\\'" "c:/Local/SumatraPDF/SumatraPDF -reuse-instance")
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


;;; * Abbreviations

(require 'abbrev)
(setq abbrev-file-name (expand-file-name "abbrev_defs" prelude-savefile-dir))



;;; * Projectile

;; Is it needed?
(setq projectile-mode-line
      '(:eval (if (projectile-project-p)
                  (if (file-remote-p default-directory)
                      " Projectile"
                    (format " Projectile[%s]" (projectile-project-name)))
                "No project")))

;;; * PCache
;;; In case it is loaded

(setq pcache-directory
  (let ((dir (expand-file-name "pcache/" prelude-savefile-dir)))
    (make-directory dir t)
    dir))


;;; * OAuth2
(setq oauth2-token-file (expand-file-name "oauth2.plstore" prelude-savefile-dir))


;;; * SSH

;; Requires that sessions have been saved under putty.
;; Connect with session name.

(setq ssh-program "c:/Local/putty/plink.exe")
(setq ssh-explicit-args '("-load"))

(defun my-fix-plink ()
  (interactive)
  (setq comint-process-echoes t)
  (setq comint-input-sender 'my-comint-simple-send)
  (setq-local comint-input-sender-no-newline nil))

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

(add-hook 'ssh-mode-hook 'my-fix-plink)


;;; * Tramp

;; Prelude sets the default method to ssh!
(setq tramp-default-method "plink"
      tramp-default-user "popineau"
      tramp-default-host "foundry.supelec.fr"
      tramp-persistency-file-name (expand-file-name "tramp" prelude-savefile-dir)
      )


;;; * Magit

(defadvice magit-expand-git-file-name (around magit-expand-git-file-name-around)
  "Fix MSYS2 pathnames"
  (setq ad-return-value
        (replace-regexp-in-string  "^\\([c-z]\\):/\\1/" "\\1:/" ad-do-it))
  )
(ad-activate 'magit-expand-git-file-name)



;;; * Spell checker
;; use aspell instead of ispell
(setq ispell-program-name "hunspell.exe"
      ispell-dictionary "en_US"
      ispell-extra-args '("--sug-mode=ultra"))
(setenv "LANG" "en_US")
;; (setenv "DICPATH" (concat ".;" (expand-file-name "../etc/hunspell" exec-directory))) ;;


;;; * Emacs Lisp Mode

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

;; (eval-after-load "lisp-mode"
;;   (setq lisp-indent-function 'lisp-indent-function-kw))

(eval-after-load 'coffee-mode
  '(push 'coffee-mode sp-autoescape-string-quote-if-empty))



;;; * Ediff

(ediff-set-diff-options 'ediff-diff-options "--strip-trailing-cr")
(setq ediff-split-window-function 'split-window-horizontally)

;;; * Web Mode
(require 'web-mode)
(push '("php" . "\\.phtml\\'") web-mode-engine-file-regexps)
(dolist (engine-regexp web-mode-engine-file-regexps)
  (when (cdr engine-regexp)
    (add-to-list 'auto-mode-alist `(,(cdr engine-regexp) . web-mode))))


;;; * C# Mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; FIXME: this doesn't work.
(defun fp-csharp-mode-hook ()
  (message "Hook for C#")
  (setq c-comment-start-regexp "\\(?:/[*/]\\)\\|\\s!"))

(add-hook 'csharp-mode-hook 'fp-csharp-mode-hook t)

;; (eval-after-load 'auto-complete
;;   '(progn
;;     (require 'auto-complete-config)
;;     (setq ac-comphist-file (expand-file-name prelude-savefile-dir "ac-comphist.dat"))
;;     (ac-config-default)
;;     (define-key ac-completing-map (kbd "ESC") 'ac-stop)
;;     (setq ac-delay 0.125
;;           ac-auto-show-menu 0.25
;;      ac-auto-start 3
;;      ac-quick-help-delay 2.0
;;      ac-ignore-case nil
;;            ac-candidate-menu-min 2
;;            ac-use-quick-help t
;;            ac-limit 10
;;            ac-disable-faces nil)

;;      (setq-default ac-sources '(ac-source-imenu
;;                                 ac-source-words-in-buffer
;;                                 ac-source-words-in-same-mode-buffers
;;                                 ac-source-dictionary
;;                                 ac-source-filename))))


;;; * auto markdown(gfm)-mode
(push '("\\.md\\'" . gfm-mode) auto-mode-alist)
(push '("\\.markdown\\'" . gfm-mode) auto-mode-alist)
(add-hook 'gfm-mode-hook (lambda () (auto-fill-mode t)))

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; auto feature-mode
(push '("\\.feature\\'" . feature-mode) auto-mode-alist)

;; don't compile sass/scss on saving
(setq scss-compile-at-save nil)

;; 2-space indent for CSS
(setq css-indent-offset 2)

;; Attach de facto prog mode hooks after loading init file
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (dolist (hook graphene-prog-mode-hooks)
;;               (add-hook hook (lambda () (run-hooks 'graphene-prog-mode-hook))))))


;;; * Writegood

(require 'writegood-mode)
;; TODO: bind some key.

;;; * Guide Key
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode +1)

;;; Kill ring
(setq save-interprogram-paste-before-kill t)

;;; * Ghostscript
;; It can help sometimes to be able to print source code!
(setenv "GS_LIB" "C:/Local/gs9.15/lib;C:/Local/gs9.15/fonts")
(setq ps-printer-name t)
(setq ps-lpr-command "C:/Local/gs9.15/bin/gswin32c.exe")
(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH"
                        "-sDEVICE=mswinpr2"
                        "-sPAPERSIZE=a4"))

;;; * Url
(setq url-configuration-directory (expand-file-name "url/" prelude-savefile-dir))


;;; * Various
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'autopair-newline 'disabled nil)


;;; * Server
;; Launch emacs server
(require 'server)
(setq server-auth-dir (expand-file-name "server/" prelude-savefile-dir))
(server-start)

(provide 'fp-editor)
