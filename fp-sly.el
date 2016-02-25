;;; fp-sly.el --- Emacs Prelude: Fabrice Popineau preload configuration.
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

(require 'prelude-lisp)

;;; Code:
;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;; Change synatx class of ?|
(require 'lisp-mode)
(modify-syntax-entry ?\| "_ 23bn" lisp-mode-syntax-table)

;; Common Lisp support depends on SLY being installed with Quicklisp
;; (if (file-exists-p (expand-file-name "~/quicklisp/sly-helper.el"))
;;     (load (expand-file-name "~/quicklisp/sly-helper.el"))
;;     (message "%s" "SLY is not installed. Use Quicklisp to install it."))

;; a list of alternative Common Lisp implementations that can be
;; used with SLY. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLY with a negative prefix
;; argument, M-- M-x sly, you can select a program from that list.
;; (setq sly-lisp-implementations
;;       '((ccl ("ccl"))
;;         (clisp ("clisp" "-q"))
;;         (cmucl ("cmucl" "-quiet"))
;;         (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

(setq sly-lisp-implementations
      '((sbcl ("c:/Local/SBCL/current/sbcl.exe" "--")
         :coding-system utf-8-unix
         :env ("SBCL_HOME=c:/Local/SBCL/current/"))
        (ccl ("c:/Local/CCL/current/wx86cl64.exe" "-n" "-K" "utf-8" "-l" "c:/Local/CCL/ccl-init.lisp")
         :coding-system utf-8-unix
         :env ("CCL_DEFAULT_DIRECTORY=c:/Local/CCL/current"))
        (lww ("c:/Local/Lispworks/lw-sly.exe"))
        (acl ("c:/Local/acl82express/allegro.exe")))
        )

;; select the default value from sly-lisp-implementations
;; (if (eq system-type 'darwin)
;;     default to Clozure CL on OS X
;;     (setq sly-default-lisp 'ccl)
;;     default to SBCL on Linux and Windows
;;     (setq sly-default-lisp 'sbcl))

(setq sly-default-lisp 'ccl)

(require 'sly-autoloads)

(add-hook 'lisp-mode-hook (lambda () (sly-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-sly-mode t)))
(setq lisp-indent-function 'common-lisp-indent-function
      sly-complete-symbol-function 'sly-fuzzy-complete-symbol)
(setq sly-repl-history-file (expand-file-name "sly-history.eld" prelude-savefile-dir))
(sly-setup '(sly-mrepl sly-autodoc sly-fancy))

(add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
(add-hook 'sly-repl-mode-hook (lambda () (run-hooks 'prelude-interactive-lisp-coding-hook)))

(eval-after-load "sly"
                 '(progn
                   (setq sly-complete-symbol-function 'sly-fuzzy-complete-symbol
                    sly-fuzzy-completion-in-place t
                    sly-enable-evaluate-in-emacs t
                    sly-autodoc-use-multiline-p t
                    sly-auto-start 'always)

                   (define-key sly-mode-map (kbd "TAB") 'sly-indent-and-complete-symbol)
                   (define-key sly-mode-map (kbd "C-c C-s") 'sly-selector)))

(defun fp-sly-disconnect ()
  (when (fboundp 'sly-disconnect-all)
    (sly-disconnect-all)))

(add-hook 'kill-emacs-hook 'fp-sly-disconnect)

;; (defun sly-init-command (port-filename coding-system)
;;   "Return a string to initialize Lisp."
;;   (let ((loader (if (file-name-absolute-p sly-backend)
;;                     sly-backend
;;                   (concat sly-path sly-backend))))
;;     ;; Return a single form to avoid problems with buffered input.
;;     (format "%S\n\n"
;;             `(progn
;;                (load ,(expand-file-name loader)
;;                      :verbose t)
;;                (setq slynk-loader::*fasl-directory*
;;                      (merge-pathnames
;;                       (make-pathname
;;                        :directory (list :relative ".sly" "fasl"
;;                                         (if (slynk-loader::sly-version-string) (slynk-loader::sly-version-string))
;;                                         (slynk-loader::unique-dir-name)))
;;                       "c:/Home/"))
;;                (funcall (read-from-string "slynk-loader:init"))
;;                (funcall (read-from-string "slynk:start-server")
;;                         ,port-filename)))))



;; (eval-after-load "sly"
;;     '(progn
;;       ))

(defun prelude-start-sly ()
  (unless (sly-connected-p)
    (save-excursion
      (if (find-process-by-name "ccl-sly")
          (sly-connect "127.0.0.1" 4006)
        (sly)))))

(setq common-lisp-hyperspec-root "file://C:/local/LispWorks/lib/6-1-0-0/manual/online/CLHS/")

;;; fp-config-common-lisp.el ends here

(provide 'fp-sly)
