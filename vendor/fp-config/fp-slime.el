;;; fp-slime.el --- Emacs Prelude: Fabrice Popineau preload configuration.
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
;; the SBCL configuration file is in Common Lisp
  (add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

  ;; Common Lisp support depends on SLIME being installed with Quicklisp
  ;; (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  ;;     (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;;     (message "%s" "SLIME is not installed. Use Quicklisp to install it."))

  ;; a list of alternative Common Lisp implementations that can be
  ;; used with SLIME. Note that their presence render
  ;; inferior-lisp-program useless. This variable holds a list of
  ;; programs and if you invoke SLIME with a negative prefix
  ;; argument, M-- M-x slime, you can select a program from that list.
  ;; (setq slime-lisp-implementations
  ;;       '((ccl ("ccl"))
  ;;         (clisp ("clisp" "-q"))
  ;;         (cmucl ("cmucl" "-quiet"))
  ;;         (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

  (setq slime-lisp-implementations
        '((sbcl ("c:/Local/sbcl/current/sbcl.exe" "--")
           :coding-system utf-8-unix
           :env ("SBCL_HOME=c:/Local/sbcl/current/"))
          (ccl ("wx86cl.exe"))
          (lww ("c:/Local/Lispworks/lw-slime.exe"))
          (acl ("c:/Local/acl82express/allegro.exe")))
          )

  ;; select the default value from slime-lisp-implementations
  ;; (if (eq system-type 'darwin)
  ;;     ;; default to Clozure CL on OS X
  ;;     (setq slime-default-lisp 'ccl)
  ;;     ;; default to SBCL on Linux and Windows
  ;;     (setq slime-default-lisp 'sbcl))

  (setq slime-default-lisp 'ccl)

  (require 'slime-autoloads)

  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-repl-history-file (expand-file-name "slime-history.eld" prelude-savefile-dir))
  (slime-setup '(slime-repl slime-autodoc slime-fancy))

(defun fp-slime-disconnect ()
  (when (fboundp 'slime-disconnect-all)
    (slime-disconnect-all)))

(add-hook 'kill-emacs-hook 'fp-slime-disconnect)

  (defun slime-init-command (port-filename coding-system)
    "Return a string to initialize Lisp."
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                    (concat slime-path slime-backend))))
      ;; Return a single form to avoid problems with buffered input.
      (format "%S\n\n"
              `(progn
                 (load ,(expand-file-name loader)
                       :verbose t)
                 (setq swank-loader::*fasl-directory*
                       (merge-pathnames
                        (make-pathname
                         :directory (list :relative ".slime" "fasl"
                                          (if (swank-loader::slime-version-string) (swank-loader::slime-version-string))
                                          (swank-loader::unique-dir-name)))
                                "c:/Home/"))
                 (funcall (read-from-string "swank-loader:init"))
                 (funcall (read-from-string "swank:start-server")
                          ,port-filename)))))



;; (eval-after-load "slime"
;;     '(progn
;;       ))

(defun prelude-start-slime
    (unless (slime-connected-p)
      (save-excursion
        (if (find-process-by-name "lw-slime")
            (slime-connect "127.0.0.1" "4006")
            (slime)))))

(setq common-lisp-hyperspec-root "file://C:/local/LispWorks/lib/6-1-0-0/manual/online/CLHS/")

  ;;; fp-config-common-lisp.el ends here

(provide 'fp-slime)
