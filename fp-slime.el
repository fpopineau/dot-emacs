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

;; Change syntax class of ?|
(require 'lisp-mode)
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

(setq slime-repl-history-file (expand-file-name "slime-history.eld" prelude-savefile-dir))

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

(provide 'fp-slime)
;;; fp-slime.el ends here
