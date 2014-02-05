;;; fp-environment.el --- Emacs Prelude: Fabrice Popineau preload configuration.
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

(custom-set-variables
   '(eval-expression-print-length nil)
   '(eval-expression-print-level nil))

(setenv "PATH" (concat "C:\\Windows\\System32"
                         ";C:\\Windows"
                         ";C:\\Windows\\System32\\Wbem"
                         ";C:\\Windows\\System32\\WindowsPowerShell\\v1.0"
                         ";C:\\Local\\Emacs\\bin"
                         ";C:\\Local\\TeXLive\\bin\\win32"
                         ";C:\\MSYS2\\local\\bin"
                         ";C:\\MSYS2\\bin"
                         ";C:\\MSYS2\\MingW64\\bin"
                         ";C:\\Local\\Putty"
                         ";C:\\Local\\Bazaar"
                         ";C:\\Local\\Git\\bin"
                         ";C:\\Local\\ImageMagick-6.8.1-5\\"
                         ";C:\\Local\\GhostScript\\GS9.06\\bin\\"
                         ";C:\\Local\\Subversion\\bin"
                         ";C:\\Local\\Hunspell\\bin"
                         ";C:\\Local\\CCL-1.8"
                         ";C:\\Local\\SumatraPDF"
                         ";C:\\Local\\SwiProlog\\bin"
                         ";C:\\Local\\BibTeX2HTML"
                         ;; ";C:\\Local\\Anaconda\\Scripts"
                         ))
(setq exec-path (split-string (getenv "PATH") ";"))
(setenv "TEXMFLOCAL" "$TEXMFROOT/texmf-local")

(setenv "BZR_SSH" "plink.exe")

;;; Make emacs open all files in last emacs session.

;; This functionality is provided by desktop-save-mode (“feature” name: “desktop”). The mode is not on by default in emacs 23.1, and has a lot options. The following is init settings for the mode for ErgoEmacs.

;; Goal: have emacs always auto open the set of opened files in last session, even if emacs crashed in last session or the OS crashed in last session. Also, don't bother users by asking questions like “do you want to save desktop?” or “do you want to override last session file?”, because these are annoying and terms like “session” or “desktop” are confusing to most users because it can have many meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕 at the variable “user-emacs-directory” (default value is “~/.emacs.d/”).  This file is our desktop file. It will be auto created and or over-written.  if a emacs expert has other desktop session files elsewhere, he can still use or manage those.

(require 'desktop)

(defvar desktop-buffers-not-to-save
  (concat "\\("
          "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
          "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
          "\\)$")
  "Buffers we want to be ignored by desktop-mode."
  )
(defvar desktop-modes-not-to-save
  '(dired-mode Info-mode info-lookup-mode fundamental-mode lisp-interaction-mode help-mode)
  "Buffers with on of these modes will be ignored by desktop-mode."
  )

(when prelude-whitespace
  (add-hook 'desktop-after-read-hook 'whitespace-unload-function t))

(defun desktop-file-modtime-reset ()
  "Reset `desktop-file-modtime' so the user is not bothered."
  (interactive)
  (run-with-timer 5 nil
          (lambda ()
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory))))

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)

  ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname prelude-savefile-dir)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save desktop-dirname)))))

    ;; Read default desktop
    (if (file-exists-p (desktop-full-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook 'desktop-file-modtime-reset)
    )
  )

(add-hook 'after-init-hook 'desktop-settings-setup "APPEND")

;; use aspell instead of ispell
(setq ispell-program-name "hunspell.exe"
      ispell-dictionary "en_US"
      ispell-extra-args '("--sug-mode=ultra"))
(setenv "LANG" "en_US")
(setenv "DICPATH" (concat ".;" (expand-file-name "../etc/hunspell" exec-directory))) ;;

(provide 'fp-environment)
