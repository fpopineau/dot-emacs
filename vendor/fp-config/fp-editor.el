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

(defvar fp-config-packages
  '(auctex
    auto-complete bookmark+ color-theme-sanityinc-tomorrow dictionary dired+
    ediprolog exec-path-from-shell framemove fuzzy git-commit-mode
    highlight-symbol htmlize js2-mode lua-mode ;; magithub
    ;; markdown-mode markdown-mode+ melpa
    memory-usage org-cua-dwim page-break-lines
    popup
    ;; python-pep8 python-pylint
    solarized-theme
    ;; sumatra-forward
    tuareg yaml-mode)
  "A list of packages to ensure are installed at launch.")

(prelude-require-packages fp-config-packages)

(setq enable-local-variables :all)
;;; graphene-editing.el --- Graphene editing defaults

;; Soft-wrap lines
(global-visual-line-mode t)

;; Linum format to avoid graphics glitches in fringe
(setq linum-format " %4d ")

;; apply syntax highlighting to all buffers
(global-font-lock-mode +1)

;; Abbreviations
(setq abbrev-file-name (expand-file-name "abbrev-defs" prelude-savefile-dir))

;; smartparens
(defun graphene--sp-pair-on-newline (id action context)
  "Put trailing pair on newline and return to point."
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun graphene--sp-pair-on-newline-and-indent (id action context)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (graphene--sp-pair-on-newline id action context)
  (indent-according-to-mode))


(eval-after-load 'coffee-mode
  '(push 'coffee-mode sp-autoescape-string-quote-if-empty))


(eval-after-load 'smartparens
  '(progn
     (require 'smartparens-config)
     (sp-pair "{" nil :post-handlers
              '(:add ((lambda (id action context)
                        (graphene--sp-pair-on-newline-and-indent id action context)) "RET")))
     (sp-pair "[" nil :post-handlers
              '(:add ((lambda (id action context)
                        (graphene--sp-pair-on-newline-and-indent id action context)) "RET")))

     (sp-local-pair '(markdown-mode gfm-mode) "*" "*"
                    :unless '(sp-in-string-p)
                    :actions '(insert wrap))

     (sp-pair "\"" nil :unless '(sp-point-after-word-p))
     (sp-pair "'" nil :unless '(sp-point-after-word-p))
     (setq sp-highlight-pair-overlay nil)))

(require 'web-mode)
(push '("php" . "\\.phtml\\'") web-mode-engine-file-regexps)
(dolist (engine-regexp web-mode-engine-file-regexps)
  (when (cdr engine-regexp)
    (add-to-list 'auto-mode-alist `(,(cdr engine-regexp) . web-mode))))

(eval-after-load 'auto-complete
  '(progn
     (require 'auto-complete-config)
     (ac-config-default)
     (define-key ac-completing-map (kbd "ESC") 'ac-stop)
     (setq ac-delay 0.125
           ac-auto-show-menu 0.25
           ac-auto-start 3
           ac-quick-help-delay 2.0
           ac-ignore-case nil
           ac-candidate-menu-min 2
           ac-use-quick-help t
           ac-limit 10
           ac-disable-faces nil)

     (setq-default ac-sources '(ac-source-imenu
                                ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers
                                ac-source-dictionary
                                ac-source-filename))))

(eval-after-load 'flycheck
  '(progn
     (defun graphene--flycheck-display-errors-function (errors)
       (mapc (lambda (err)
               (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
             errors))
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function 'graphene--flycheck-display-errors-function)))

;; Main hook to be run on entering de facto prog modes, enabling linum, autopair,
;; autocomplete, plus setting binding newline key to newline-and-indent
;; (add-hook 'graphene-prog-mode-hook
;;           (lambda ()
;;             (when graphene-linum-auto
;;               (graphene-linum))
;;             (when graphene-autocomplete-auto
;;               (graphene-autocomplete))
;;             (when graphene-autopair-auto
;;               (graphene-autopair))
;;             (when 'graphene-parens-auto
;;                 (graphene-parens))
;;             (when 'graphene-errors-auto
;;               (graphene-errors))
;;             (define-key (current-local-map) [remap newline] 'newline-and-indent)))

(defun graphene-linum ()
  (linum-mode t))

(defun graphene-autocomplete ()
  (require 'auto-complete)
  (auto-complete-mode t))

(defun graphene-autopair ()
  (require 'smartparens)
  (smartparens-mode t))

(defun graphene-parens ()
  (show-smartparens-mode t)
  (setq sp-show-pair-delay 0))

(defun graphene-errors ()
  (require 'flycheck)
  (flycheck-mode))

;; auto markdown(gfm)-mode
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

;; Default Ruby filetypes
(dolist (regex
         '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$" "Gemfile$" "Capfile$" "Guardfile$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))

;; Remap newline to newline-and-indent in ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key (current-local-map) [remap newline] 'reindent-then-newline-and-indent)))

;; Attach de facto prog mode hooks after loading init file
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (dolist (hook graphene-prog-mode-hooks)
;;               (add-hook hook (lambda () (run-hooks 'graphene-prog-mode-hook))))))

;;; graphene-env.el --- Graphene environment defaults

(require 'smex)
(smex-initialize)

(setq inhibit-startup-message t
      color-theme-is-global t
      resize-mini-windows nil
      uniquify-buffer-name-style 'forward
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-auto-revert-mode t)

(ido-mode 1)

(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'autopair-newline 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'fp-editor)
