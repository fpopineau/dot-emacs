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
;;; graphene-speedbar.el --- Graphene defaults for Speedbar

(autoload 'sr-speedbar-open "sr-speedbar" "Open the in-frame speedbar" t)

(eval-after-load 'sr-speedbar
  '(progn
     (setq speedbar-hide-button-brackets-flag t
           speedbar-show-unknown-files t
           speedbar-smart-directory-expand-flag t
           speedbar-directory-button-trim-method 'trim
           speedbar-use-images nil
           speedbar-indentation-width 2
           speedbar-use-imenu-flag t
           speedbar-file-unshown-regexp "flycheck-.*"
           sr-speedbar-width 40
           sr-speedbar-width-x 40
           sr-speedbar-auto-refresh nil
           sr-speedbar-skip-other-window-p t
           sr-speedbar-right-side nil)

     ;; Refresh the speedbar when relevant hooks are run.
     (defvar graphene-speedbar-refresh-hooks)
     (defvar graphene-speedbar-refresh-hooks-added nil
       "Whether hooks have been added to refresh speedbar.")

     (add-hook 'speedbar-mode-hook
               '(lambda ()
                  (hl-line-mode 1)
                  (visual-line-mode -1)
                  (setq automatic-hscrolling nil)
                  (let ((speedbar-display-table (make-display-table)))
                    (set-display-table-slot speedbar-display-table 0 8230)
                    (setq buffer-display-table speedbar-display-table))
                  (when (not graphene-speedbar-refresh-hooks-added)
                    (lambda ()
                      (mapc (lambda (hook)
                              (add-hook hook 'speedbar-refresh))
                            graphene-speedbar-refresh-hooks)
                      (setq graphene-speedbar-refresh-hooks-added t)))))

     ;; More familiar keymap settings.
     (add-hook 'speedbar-reconfigure-keymaps-hook
               '(lambda ()
                  (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
                  (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
                  (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

     ;; Pin and unpin the speedbar
     (defvar graphene-speedbar-pinned-directory)

     (defadvice speedbar-update-directory-contents
       (around graphene-speedbar-pin-directory activate disable)
       "Pin the speedbar to the directory set in graphene-speedbar-pinned-directory."
       (let ((default-directory graphene-speedbar-pinned-directory))
         ad-do-it))

     (defadvice speedbar-dir-follow
       (around graphene-speedbar-prevent-follow activate disable)
       "Prevent speedbar changing directory on button clicks."
       (speedbar-toggle-line-expansion))

     (defadvice speedbar-directory-buttons-follow
       (around graphene-speedbar-prevent-root-follow activate disable)
       "Prevent speedbar changing root directory on button clicks.")

     (defvar graphene-speedbar-pin-advice
       '((speedbar-update-directory-contents around graphene-speedbar-pin-directory)
         (speedbar-dir-follow around graphene-speedbar-prevent-follow)
         (speedbar-directory-buttons-follow around graphene-speedbar-prevent-root-follow))
       "Advice to be enabled and disabled on graphene-[un]-pin-speedbar.")

     (defun graphene-speedbar-pin-advice-activate ()
       "Activate the advice applied to speedbar functions in order to pin it to a directory."
       (mapc 'ad-activate (mapcar 'car graphene-speedbar-pin-advice)))

     (defun graphene-pin-speedbar (directory)
       "Prevent the speedbar from changing the displayed root directory."
       (setq graphene-speedbar-pinned-directory directory)
       (mapc (lambda (ls) (apply 'ad-enable-advice ls)) graphene-speedbar-pin-advice)
       (graphene-speedbar-pin-advice-activate))

     (defun graphene-unpin-speedbar ()
       "Allow the speedbar to change the displayed root directory."
       (mapc (lambda (ls) (apply 'ad-disable-advice ls)) graphene-speedbar-pin-advice)
       (graphene-speedbar-pin-advice-activate))

     ;; Always use the last selected window for loading files from speedbar.
     (defvar last-selected-window
       (if (not (eq (selected-window) sr-speedbar-window))
           (selected-window)
         (other-window 1)))

     (defadvice select-window (after remember-selected-window activate)
       "Remember the last selected window."
       (unless (or (eq (selected-window) sr-speedbar-window)
                   (not (window-live-p (selected-window))))
         (setq last-selected-window (selected-window))))

     (defun sr-speedbar-before-visiting-file-hook ()
       "Function that hooks `speedbar-before-visiting-file-hook'."
       (select-window last-selected-window))

     (defun sr-speedbar-before-visiting-tag-hook ()
       "Function that hooks `speedbar-before-visiting-tag-hook'."
       (select-window last-selected-window))

     (defun sr-speedbar-visiting-file-hook ()
       "Function that hooks `speedbar-visiting-file-hook'."
       (select-window last-selected-window))

     (defun sr-speedbar-visiting-tag-hook ()
       "Function that hooks `speedbar-visiting-tag-hook'."
       (select-window last-selected-window))))

(provide 'fp-speedbar)
