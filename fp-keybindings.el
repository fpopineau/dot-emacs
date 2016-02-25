;;; fp-keybindings.el --- Emacs Prelude: Fabrice Popineau key bindings configuration.
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
;;; graphene-keys.el --- Graphene keybindings

;; I really don't see why we should stick to letters for selection
;; when an extended keyboard is available!
(setq prelude-guru nil)

;; Nicer scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

(require 'framemove)
(setq framemove-hook-into-windmove t)

;; One day, I'll check this again and see if I can do something of it.
;; Load ergoemacs-keybindings minor mode
;; (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "fr") ; FR
;; (load "ergoemacs-keybindings/ergoemacs-mode")
;; (ergoemacs-mode 1)

(require 'undo-tree)

(global-set-key (kbd "C-z") 'undo-tree-undo)

(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; (global-set-key (kbd "C-x k")
;;                 'kill-default-buffer)
;; (global-set-key (kbd "C-x C-k")
;;                 'kill-buffer-and-window)
;; (global-set-key (kbd "C-c n")
;;                 'create-new-buffer)
;; (global-set-key (kbd "C-c N")
;;                 'new-emacs-instance)
;; (global-set-key (kbd "C-;")
;;                 'insert-semicolon-at-end-of-line)
;; (global-set-key (kbd "M-RET")
;;                 'newline-anywhere)
;; (global-set-key (kbd "C-M-;")
;;                 'comment-current-line-dwim)
;; (global-set-key (kbd "C->")
;;                 'increase-window-height)
;; (global-set-key (kbd "C-<")
;;                 'decrease-window-height)
;; (global-set-key (kbd "C-,")
;;                 'decrease-window-width)
;; (global-set-key (kbd "C-.")
;;                 'increase-window-width)
;; (global-set-key (kbd "C-c s")
;;                 'sr-speedbar-select-window)

(provide 'fp-keybindings)
;;; fp-keybindings.el ends here
