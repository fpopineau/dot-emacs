;;; fp-look.el --- Emacs Prelude: Fabrice Popineau preload configuration.
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

(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(defvar fp-default-font "Consolas-10")
(defvar fp-variable-pitch-font "Segoe UI-10")
(defvar fp-fixed-pitch-font "Consolas-10")

;; Work around Emacs frame sizing bug when line-spacing
;; is non-zero, which impacts e.g. grizzl.
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (set (make-local-variable 'line-spacing) 0)))

(setq redisplay-dont-pause t)

(scroll-bar-mode -1)

(tool-bar-mode -1)

(blink-cursor-mode -1)

(require 'page-break-lines)
(global-page-break-lines-mode 1)

(require 'ansi-color)

;; (defvar fp-geometry-file
;;   (expand-file-name ".fp-geometry" user-emacs-directory)
;;   "The file where frame geometry settings are saved.")

;; (defun fp-load-frame-geometry ()
;;   "Load saved frame geometry settings."
;;   (if (file-readable-p fp-geometry-file)
;;       (with-temp-buffer
;;         (insert-file-contents fp-geometry-file)
;;         (read (buffer-string)))
;;     '(160 70 0 0)))

;; (defun fp-save-frame-geometry ()
;;   "Save current frame geometry settings."
;;   (with-temp-file fp-geometry-file
;;     (print (fp-get-geometry) (current-buffer))))

;; (defun fp-get-geometry ()
;;   "Get the current geometry of the active frame."
;;   (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

;; (defun fp-set-geometry ()
;;   "Set the default frame geometry using the values loaded from fp-geometry-file."
;;   (let ((geom (fp-load-frame-geometry)))
;;     (let ((f-width (car geom))
;;           (f-height (cadr geom))
;;           (f-top (caddr geom))
;;           (f-left (cadddr geom)))
;;       (setq default-frame-alist
;;             (append default-frame-alist
;;                     `((width . ,f-width)
;;                       (height . ,f-height)
;;                       (top . ,f-top)
;;                       (left . ,f-left)))))))

(defun fp-set-fonts ()
  "Set up default fonts."
  (unless fp-default-font
    (setq fp-default-font (face-font 'default)))
  (unless fp-variable-pitch-font
    (setq fp-variable-pitch-font (face-font 'variable-pitch)))
  (unless fp-fixed-pitch-font
    (setq fp-fixed-pitch-font (face-font 'fixed-pitch))))


(defun fp-look-startup-after-init ()
  "Load defaults for the overall Graphene look -- to be called after loading the init file so as to pick up custom settings."
  (if window-system
      (progn
;;        (fp-set-geometry)
;;        (add-hook 'kill-emacs-hook 'fp-save-frame-geometry)
        (setq-default line-spacing 2)
        (fp-set-fonts)
        (add-to-list 'default-frame-alist `(font . ,fp-default-font))
        (set-face-font 'default fp-default-font)
        (set-face-font 'variable-pitch fp-variable-pitch-font)
        (set-face-font 'fixed-pitch fp-fixed-pitch-font)
        (add-to-list 'default-frame-alist '(internal-border-width . 0))
        (set-fringe-mode '(8 . 0))
        (disable-theme 'zenburn)
        (color-theme-sanityinc-tomorrow-night)
        (require 'graphene-theme)
        (load-theme 'graphene t)
        (defadvice load-theme
          (after load-graphene-theme (theme &optional no-confirm no-enable) activate)
          "Load the graphene theme extensions after loading a theme."
          (when (not (equal theme 'graphene))
            (load-theme 'graphene t)))
        (when (not (eq system-type 'darwin))
          (menu-bar-mode -1)))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'fp-look-startup-after-init)

(provide 'fp-look)
