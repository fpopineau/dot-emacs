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

(setenv "HOME" "c:/Home/")
(setq abbreviated-home-dir
      (let ((abbreviated-home-dir "$foo"))
        (concat "\\`" (abbreviate-file-name (expand-file-name "~"))
                "\\(/\\|\\'\\)")))

(setq emacs-debug nil)
(setq debug-on-error t)

;; Character encodings default to utf-8.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Win32 !
;; As of Emacs 24.4, unicode filenames under NT should be honoured
;; (set-file-name-coding-system 'latin-1)
;; MS Windows clipboard is UTF-16LE
(set-clipboard-coding-system 'utf-16le-dos)

;; Fix load-path for those packages we mirror
(defvar *fp-config-packages-supplied-regexp*
  "\\(lisp/org/\\)")

(require 'cl)
(message "Load-path = %s" load-path)
;; (setq load-path
;;       (mapcar #'(lambda(elt)
;;                   (unless (string-match *fp-config-packages-supplied-regexp* elt)
;;                     elt))
;;               load-path))
(mapc #'(lambda(elt)
            (if (string-match *fp-config-packages-supplied-regexp* elt)
                (setq load-path (delq elt load-path))))
	load-path)

(message "Load-path = %s" load-path)

(add-to-list 'load-path (expand-file-name "../vendor/org-mode/lisp" (file-name-directory load-file-name)))
(message "Load-path = %s" load-path)
