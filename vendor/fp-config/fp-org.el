;;; fp-org.el --- Emacs Prelude: Fabrice Popineau Org mode configuration.
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

(setq org-modules '(; org-bbdb
                    org-bibtex
                    ;; org-bibtex-extras
                    org-checklist
                    org-contacts
                                        ;                          org-crypt
                    org-drill
                    ox
                    ox-latex
                    ox-beamer
                    ox-html
                    ox-publish
                                        ;                          org-gnus
                    org-favtable
                    org-habit
                    org-id
                    org-info
                    org-irc
                    org-inlinetask
                                        ;                          org-irc
                                        ;                          org-mew
                    org-mime
                                        ;                          org-mhe
                    org-protocol
                                        ;                          org-rmail
                    org-timer
                                        ;                          org-vm
                    org-wl
                    org-mouse
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                                        ;                      org-man
                    org-panel
                    org-screen
                    org-toc
                                        ;                          org-w3m
                    ))
(require 'htmlize)
                                        ; (require 'org-install)
(require 'org)
(org-load-modules-maybe t)

(require 'org-cua-dwim)
(org-cua-dwim-activate)

                                        ; (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-directory "~/Org")
(setq org-default-notes-file "~/Org/notes.org")

(add-to-list 'Info-default-directory-list "c:/mirror/elisp/org-mode/doc")

(defun fp-org-hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun fp-org-set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun fp-org-make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-subtree)
                                      ("P" . bh/narrow-to-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

;; Custom Key Bindings
; (global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
(global-set-key (kbd "<f9> w") 'widen)
(global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'tabify)
(global-set-key (kbd "<f9> U") 'untabify)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
; (global-set-key (kbd "<f11>") 'org-clock-goto)
; (global-set-key (kbd "C-<f11>") 'org-clock-in)
; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)
;; I use C-c r to start capture mode when using SSH from my Android phone
(global-set-key (kbd "C-c r") 'org-capture)

(setq org-hide-leading-stars nil)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-startup-indented t)

(setq org-cycle-separator-lines 2)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

(setq org-log-done (quote time))
(setq org-log-into-drawer "LOGBOOK")

(setq org-clock-sound "c:/Windows/Media/chord.wav")


                                        ; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode 1)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-"))))

(setq org-tags-match-list-sublevels t)
                                        ; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))


(setq org-startup-folded 'content)

(setq org-alphabetical-lists t)

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

(setq org-use-sub-superscripts nil)

(setq org-odd-levels-only nil)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-enable-priority-commands t)
(setq org-default-priority ?E)
(setq org-lowest-priority ?E)

(setq org-export-allow-BIND t)


;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(setq require-final-newline t)

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link t)


(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
                 Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change nil)

;; (add-to-list 'Info-default-directory-list "~/git/org-mode/doc")



(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

                                        ; (require 'org-mime)


(setq org-table-use-standard-references (quote from))

(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

                                        ; Overwrite the current window with the agenda

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)


;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

;; Bibliography and bibliography notes handling.
;; http://www-public.telecom-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
(require 'reftex)
(require 'reftex-vars)
(defun org-reftex-search ()
  "Jump to the notes for the paper pointed to at from reftex search."
  (interactive)
  (org-open-link-from-string (format "[[bibtex:%s]]" (car (reftex-citation t)))))

;; (defun org-reftex-citation ()
;;   "Insert citation for bibtex references."
;;   (interactive)
;;   (reftex-citation nil ?b))


(defun org-reftex-setup ()
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
                                        ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)
                                        ;add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bibtex:%l]]")
            (?c . "\[_{}[[#%l][%l]]]")
            (?n . "[[note:%l]]")
            (?h . "%t\n:PROPERTIES:\n:Custom_ID: %l\n:BIB: [[bib::%l]]\n:PAPER: [[chm:%l][%l]]\n:END:\n")
            (?p . "%t\n:PROPERTIES:\n:Custom_ID: %l\n:BIB: [[bib::%l]]\n:PAPER: [[paper:%l][%l]]\n:END:\n")
            ;; (?n . "[[notes:%l][%l-notes]]")
            ;; (?p . "[[papers:%l][%l-paper]]")
            ;; (?t . "%t")
            ;; (?h . "** %t\n:PROPERTIES:\n:Custom_ID: ;;%l\n:END:\n[[papers:%l][%l-paper]]")
            ))))
  (define-key org-mode-map (kbd "C-c )") 'org-reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-reftex-search))

(add-hook 'org-mode-hook 'org-reftex-setup)

(setq org-bibtex-autogen-keys t)

(defun my-bibtex-export-handler (path desc format)
  ;; (message "my-rtcite-export-handler is called : path = %s, desc = %s, format ;;= %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc)
                   (equal 0 (search "bibtex:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search))))))

(setf (caddr (assoc "bibtex" org-link-protocols))
      'my-bibtex-export-handler)


(defadvice org-latex-keyword (around org-latex-keyword-bibliography)
  "Exports `bibliography' keyword with LaTeX backend."
  (let ((key (org-element-property :key (ad-get-arg 0)))
        (value (org-element-property :value (ad-get-arg 0))))
    (if (and (string= (org-element-property :key (ad-get-arg 0)) "BIBLIOGRAPHY")
             (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\([^\r\n]*\\)" value))
        (let ((bibfile (match-string 1 value))
              (bibstyle (match-string 2 value)))
          (message "file: %s\nstyle: %s\n" bibfile bibstyle)
          (setq ad-return-value (concat "\\bibliography{"
                                        bibfile
                                        "}\n\\bibliographystyle{"
                                        bibstyle
                                        "}\n")))
      ad-do-it)))

(ad-activate 'org-latex-keyword)


;; (setq org-link-abbrev-alist
;;       '(("bib" . "~/Papers/bibliography.bib::%s")
;;         ("notes" . "~/Papers/bibliography.org::#%s")
;;         ("papers" . "~/research/papers/%s.pdf")))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; ;; splantuml - PlantUML Source block
;; (define-skeleton skel-org-block-plantuml
;;   "Insert a org plantuml block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src plantuml :file " str ".png :cache yes\n"
;;   _ - \n
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; ;; sditaa - Ditaa source block
;; (define-skeleton skel-org-block-ditaa
;;   "Insert a org ditaa block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src ditaa :file " str ".png :cache yes\n"
;;   _ - \n
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

(setq org-capture-templates-contexts nil)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      `(
        ("t" "todo" entry (file+headline "~/Org/notes.org" "Tasks")
         "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:" :clock-in t :clock-resume t)
        ("d" "Done task" entry
         (file+headline "~/Org/notes.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?\n")
        ("q" "Quick task" entry
         (file+headline "~/Org/notes.org" "Tasks")
         "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n"
         :immediate-finish t)
        ("r" "respond" entry (file+headline "~/Org/notes.org" "Respond")
         "* TODO Respond to %:from on %:subject\n%U\n%A\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file+headline "~/Org/notes.org" "Notes")
         "* %? :NOTE:\n%U\n%A\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree "~/Org/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/Org/ledger")
         "%(org-read-date) %^{Payee}\nLiabilities:MBNA\nExpenses:%^{Account}  $%^{Amount}\n" :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/Org/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills\nLiabilities:MBNA\nAssets:Wayne:Groceries  $%^{Amount}\n" :immediate-finish)
        ("lc" "Cash" plain
         (file "~/Org/ledger")
         "%(org-read-date) * %^{Payee}\nExpenses:Cash\nExpenses:%^{Account}  %^{Amount}\n")
        ("b" "BibTeX entries")
        ("ba" "Articles" entry
         (file "~/Projets/Papers/Bibliography.org")
         "* READ %?\n\n%a\n\n%:author (%:year): %:title\n\nIn %:journal, %:pages.")
        ("a" "Articles entries" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
 %i
 *Author(s):* %^{Author} \\\\
 *ISBN:* %^{ISBN}

 %?

 *Review on:* %^t \\
 %a
 %U"
         :clock-in :clock-resume)
        ("o" "Templates for capturing urls in the Web browser.")
        ("on" "org-protocol" entry (file+headline "~/Org/notes.org" "Org")
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("ol" "Lisp" entry (file+headline "~/Org/Lisp.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("oe" "Emacs" entry (file+headline "~/Org/Emacs.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("og" "Georges" entry (file+headline "~/Projets/Georges/Georges-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("oy" "Youssef" entry (file+headline "~/Projets/Youssef/Youssef-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("oj" "Jean-Paul" entry (file+headline "~/Projets/PsyGolog/PsyGolog-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("ox" "AIXI" entry (file+headline "~/Projets/AIXI/AIXI-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("of" "FISDA" entry (file+headline "~/Cours/FCS-DSA/org/FCS-DSA-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("oc" "CQP-ArTech" entry (file+headline "~/Cours/CQP-ArTech/org/CQP-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")
        ("oi" "Maj-InfoTh" entry (file+headline "~/Cours/Maj-InfoTh/org/InfoTh-Progress.org" "Readings")
         "* IDEA %c\n%U\n%A\n")
        ("oa" "Min-IA" entry (file+headline "~/Cours/Min-IA/org/Min-IA-Progress.org" "Readings")
         "* TODO Review %c\n%U\n%A\n")

        ("p" "Phone call" entry (file+headline "~/Org/notes.org" "Phone")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("u"
         "Default template"
         entry
         (file+headline "~/org/notes.org" "Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)
        ("h" "Habit" entry (file+headline "~/Org/notes.org" "Habit")
         "* NEXT %?\n%U\n%A\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("w" "Work in progress")
        ("wl" "Lisp" entry (file "~/Org/Lisp.org")
         "* IDEA %?\n%U\n%A\n")
        ("we" "Emacs" entry (file "~/Org/Emacs.org")
         "* IDEA %?\n%U\n%A\n")
        ("(widget-get  )" "Georges" entry (file "~/Projets/Georges/Georges-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wy" "Youssef" entry (file "~/Projets/Youssef/Youssef-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wj" "Jean-Paul" entry (file "~/Projets/PsyGolog/PsyGolog-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wx" "AIXI" entry (file "~/Projets/AIXI/AIXI-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wf" "FISDA" entry (file "~/Cours/FCS-DSA/org/FCS-DSA-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wc" "CQP-ArTech" entry (file "~/Cours/CQP-ArTech/org/CQP-Progress.org")
         "* IDEA %?\n%U\n%A\n")
        ("wi" "Maj-InfoTh" entry (file "~/Cours/Maj-InfoTh/org/InfoTh-Progress.org")
         "* IDEA %?\n%Ubn%A\n")
        ("wa" "Min-IA" entryG(file "~/Cours/Min-IA/org/Min-IA-Progress.org")
         "* IDEA %?\n%Uen%A\n")

        ("W" "Websites" checkitem (file+olp "~/Org/notes.org" "Websites" "To view")
         "| %c | %^{Description}")
        ))

;;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 1 )
                                 ("notes.org" :regexp . "Viewed")
                                 (org-agenda-files :maxlevel . 2))))

;;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
;;; Refile settings

;;; Exclude DONE state tasks from refile targets
(defun my/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (save-excursion (org-goto-first-child)))
(setq org-refile-target-verify-function 'my/verify-refile-target)
                                        ; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;; org-agenda-files in the variable org-user-agenda-files
;;
;; Agenda files are located either in Projects or in Cours
(setq org-agenda-files
      `("~/Org/"
        ,@(mapcan #'(lambda (dir)
                      (remove-if #'(lambda (f) (or (not (file-directory-p f))
                                                   (string-match "\\.\\.?$" f)))
                                 (directory-files dir t))) '("~/Projets/" "~/Cours/"))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "REVIEW(r)" "DELVE(n)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("REVIEW" :foreground "gold" :weight bold)
              ("DELVE" :foreground "DarkOrchid" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "NavajoWhite1" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-enforce-todo-dependencies t)

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags-todo "-WAITING-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-todo-ignore-with-date t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-todo-ignore-with-date t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-agenda-skip-function 'bh/skip-stuck-projects)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")
                (org-tags-match-list-sublevels nil)))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                (org-agenda-todo-ignore-scheduled t)
                (org-agenda-todo-ignore-deadlines t)
                (org-agenda-todo-ignore-with-date t)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
               ((org-agenda-overriding-header "Tasks")
                (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-tags-match-list-sublevels nil))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                (org-tags-match-list-sublevels nil))))))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
                             (org-agenda-set-restriction-lock restriction-type)))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
                             (org-agenda-set-restriction-lock restriction-type))))))))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/Org/diary.org")

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (830 1000 1200 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

(setq org-agenda-span 'day)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

                                        ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
                                        ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
                                        ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
                                        ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
                                        ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map "q" 'bury-buffer))
          'append)

(setq org-agenda-persistent-filter t)

(setq org-agenda-skip-additional-timestamps-same-entry t)
(setq org-agenda-window-setup 'current-window)

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when (equal mystate "NEXT")
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)
;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

                                        ; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 36)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; save file
(setq org-clock-persist-file (expand-file-name "org-clock-save.el" prelude-savefile-dir))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
                       (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
                               (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
                     (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
                     (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (org-with-point-at clock-in-to-task
                       (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))

(defun bh/clock-in-bzflagt-task ()
  (interactive)
  (bh/clock-in-task-by-id "dcf55180-2a18-460e-8abb-a9f02f0893be"))

(defun bh/resume-clock ()
  (interactive)
  (if (marker-buffer org-clock-interrupted-task)
      (org-with-point-at org-clock-interrupted-task
                         (org-clock-in))
    (org-clock-out)))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

                                        ; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (state)))

                                        ; Tags with fast selection keys
(setq org-tag-persistent-alist (quote ((:startgroup)
                                       ("@georges"   . ?g)
                                       ("@youssef"   . ?y)
                                       ("@jps"       . ?j)
                                       ("@lisp"      . ?l)
                                       ("@emacs"     . ?e)
                                       ("@fisda"     . ?f)
                                       ("@infoth"    . ?i)
                                       ("@minia"     . ?a)
                                       ("@cqp"       . ?c)
                                       ("@deptinfo"  . ?d)
                                       ("@supelec"   . ?s)
                                       ("@home"      . ?h)
                                       (:endgroup)
                                       (:newline)
                                       (:startgroup)
                                       ("slide"      . ?s)
                                       ("poly"       . ?p)
                                       ("exercise"   . ?e)
                                       ("example"    . ?x)
                                       (:endgroup)
                                       (:newline)
                                       ("phone"      . ?p)
                                       ("waiting"    . ?w)
                                       ("hold"       . ?h)
                                       ("personal"   . ?P)
                                       ("work"       . ?W)
                                       ("org"        . ?O)
                                       ("mark"       . ?M)
                                       ("note"       . ?n)
                                       ("obsolete"   . ?o)
                                       ("deprecated" . ?c)
                                       ("flagged"    . ??))))

                                        ; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

                                        ; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
                                        ; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

                                        ; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

                                        ; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

                                        ; Activate appointments so we get notifications
(appt-activate t)

                                        ; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-agenda-remove-restriction-lock)
    (widen)
    (org-agenda-remove-restriction-lock)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (if (equal major-mode 'org-agenda-mode)
        (bh/set-agenda-restriction-lock 4)
      (widen))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (org-get-at-bol 'org-hd-marker)
                         (bh/narrow-to-org-subtree))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                         (bh/narrow-up-one-org-level))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                         (bh/narrow-to-org-project))
    (bh/narrow-to-org-project)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/current-view-project nil)

(defun bh/view-next-project ()
  (interactive)
  (unless (marker-position org-agenda-restrict-begin)
    (goto-char (point-min))
    (setq bh/current-view-project (point)))
  (bh/widen)
  (goto-char bh/current-view-project)
  (forward-visible-line 1)
  (while (and (< (point) (point-max))
              (or (not (org-get-at-bol 'org-hd-marker))
                  (org-with-point-at (org-get-at-bol 'org-hd-marker)
                                     (or (not (bh/is-project-p))
                                         (bh/is-project-subtree-p)))))
    (forward-visible-line 1))
  (setq bh/current-view-project (point))
  (if (org-get-at-bol 'org-hd-marker)
      (bh/narrow-to-project)
    (message "All projects viewed.")
    (ding)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(defun bh/show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))

(defun bh/toggle-truncate-lines ()
  "Toggle setting truncate-lines between t and nil"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display))

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-ditaa-jar-path "~/java/ditaa0_6b.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

                                        ; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
                                        ;         (ditaa . t)
                                        ;         (R . t)
         (lisp . t)
         (python . t)
                                        ;         (ruby . t)
                                        ;         (gnuplot . t)
                                        ;         (clojure . t)
                                        ;         (sh . t)
                                        ;         (ledger . t)
         (org . t)
                                        ;         (plantuml . t)
         (latex . t))))

                                        ; Do not prompt to confirm evaluation
                                        ; This may be dangerous - make sure you understand the consequences
                                        ; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

                                        ; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(setq org-publish-timestamp-directory
      (expand-file-name  "org-timestamps/" prelude-savefile-dir))

(setq org-export-coding-system 'utf-8)

(defun publish-prepare ()
  (setq org-html-coding-system 'utf-8)
  (setq make-backup-files nil)
  (setq org-html-inline-images t)
  (setq org-export-allow-BIND t)
  (setq org-confirm-babel-evaluate nil))

(defun publish-complete ()
  (setq make-backup-files t)
  (setq org-confirm-babel-evaluate t))

;; (defun publish-preamble (options)
;;                                         ;  (message "%s\n" options)
;;   )

;; (defun publish-postamble (options)
;;   " </div><!--/row-->
;;      <hr>

;;       <footer>
;;         <p>&copy; Company 2012</p>
;;       </footer>

;;     </div><!--/.fluid-container-->

;;     <!-- Le javascript
;;     ================================================== -->
;;     <!-- Placed at the end of the document so the pages load faster -->
;;     <script src=\"js/jquery.js\"></script>
;;     <script src=\"js/bootstrap-transition.js\"></script>
;;     <script src=\"js/bootstrap-alert.js\"></script>
;;     <script src=\"js/bootstrap-modal.js\"></script>
;;     <script src=\"js/bootstrap-dropdown.js\"></script>
;;     <script src=\"js/bootstrap-scrollspy.js\"></script>
;;     <script src=\"js/bootstrap-tab.js\"></script>
;;     <script src=\"js/bootstrap-tooltip.js\"></script>
;;     <script src=\"js/bootstrap-popover.js\"></script>
;;     <script src=\"js/bootstrap-button.js\"></script>
;;     <script src=\"js/bootstrap-collapse.js\"></script>
;;     <script src=\"js/bootstrap-carousel.js\"></script>
;;     <script src=\"js/bootstrap-typeahead.js\"></script>")
;; (with-temp-buffer
;;   (insert-file-contents "~/org/html/postamble.html")
;;   (buffer-string)))

(defun bh/save-then-publish ()
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers)
  (org-publish-current-project))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(setq org-latex-emphasis-alist
      '(("*" "\\textbf{%s}" nil)
        ("/" "\\emph{%s}" nil)
        ("_" "\\underline{%s}" nil)
        ("+" "\\texttt{%s}" nil)
        ("=" "\\verb=%s=" nil)
        ("~" "\\verb~%s~" t)
        ("@" "\\alert{%s}" nil)))
(setq org-latex-classes
      '(("beamer" "\\documentclass[presentation]{beamer}\n     [NO-DEFAULT-PACKAGES]\n     [PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("fpsupelec" "\\documentclass[poly,french]{fpsupelec}\n     [NO-DEFAULT-PACKAGES]\n     [PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("td" "\\documentclass{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[EXTRA]"
         ("\\fpremark{%s}\\fpnewtd" . "")
         ("\\Exercise %% %s" . "")
         ("\\begin{questiion} %% %s" "\\end{questiion}")
         ("\\begin{solution} %% %s" "\\end{solution}"))
        ("IEEEtran" "\\documentclass{IEEEtran}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("article" "\\documentclass[11pt]{memoir}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("report" "\\documentclass[11pt]{memoir}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("book" "\\documentclass[11pt]{memoir}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;  (setq org-latex-default-packages-alist
;;        '("\\usepackage[poly,french]{fpsupelec}\\tolerance=1000"))
(setq org-latex-default-packages-alist
      '( ; ("AUTO" "inputenc" t)
                                        ; ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("normalem" "ulem" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amstext" nil)
        ("" "hyperref" nil)
        "\\tolerance=1000"))
;; Better hit the right web site ... or use a local version
(setq org-export-html-mathjax-options
      (quote ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
              (scale "100") (align "center") (indent "2em") (mathml nil))))

;; Do not insert default beamer theme
(setq org-beamer-theme nil)

(setq org-latex-listings t)

(setq org-latex-pdf-process
      '("lualatex --interaction nonstopmode --shell-escape --output-directory %o %f"
                                        ; "bibtex %b"
        "lualatex --interaction nonstopmode --shell-escape --output-directory %o %f"
        "lualatex --interaction nonstopmode --shell-escape --output-directory %o %f"))

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . system)
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . "SumatraPDF -reuse-instance %s"))
      )

(defun latex-buffer-caption-to-caption* ()
  (when org-beamer-export-is-beamer-p
    (replace-regexp "\\(\\\\caption\\)\\([[{]\\)" "\\1*\\2" nil
                    (point-min) (point-max))))

(add-hook 'org-export-latex-final-hook
          'latex-buffer-caption-to-caption* 'append)
(setq org-export-with-LaTeX-fragments t)
;; Beamer
(add-to-list 'org-beamer-environments-extra
             '("only" "o" "\\only%a{%h%x" "}"))

(setq org-html-inline-images t)
(setq org-html-htmlize-output-type 'css)

(setq org-html-special-string-regexps
      (append '(
                ("\\<LuaLaTeX\\>" . "LUA<span class=\"TEX\">L<span class=\"A\">A</span>T<span class=\"E\">E</span>X</span> ")
                ("\\<LaTeX\\>" . "<span class=\"TEX\">L<span class=\"A\">A</span>T<span class=\"E\">E</span>X</span> ")
                ("\\<LuaTeX\\>" . "LUA<span class=\"TEX\">T<span class=\"E\">E</span>X</span> ")
                ("\\<ConTeXt\\>" . "CON<span class=\"TEX\">T<span class=\"E\">E</span>X</span>T ")
                ("\\<TeX\\>" . "<span class=\"TEX\">T<span class=\"E\">E</span>X</span> "))
              org-html-special-string-regexps))

;; (push '("\\<LaTeX\\>" . "<span class=\"TEX\">L<span class=\"A\">A</span>T<span class=\"E\">E</span>X</span> ")
;;      org-html-special-string-regexps
;;      )

;; (push '("\\<TeX\\>" . "<span class=\"TEX\">T<span class=\"E\">E</span>X</span> ")
;;      org-html-special-string-regexps
;;      )

(defadvice org-html-convert-special-strings (around org-html-convert-special-strings-around)
  "Take case into account in `org-html-convert-special-strings'."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'org-html-convert-special-strings)

(setq org-publish-project-alist
                                        ;
                                        ; http://www.norang.ca/  (norang website)
                                        ; norang-org are the org-files that generate the content
                                        ; norang-extra are images and css files that need to be included
                                        ; norang is the top-level project that gets published
      (quote
       (
        ;; ("fisda"
        ;;  :base-directory "~/Cours/FISDA/org"
        ;;  :base-extension "org"
        ;;  :publishing-directory "~/cours/FISDA/poly"
        ;;  :publishing-function org-latex-publish-to-latex
        ;;  :select-tags     ("@POLY")
        ;;  :title "FCS-DSA Course Notes"
        ;;  :include ("index.org")
        ;;  :exclude "\\.org$"
        ;;  )
        ("Web-inherit"
         :base-directory "~/Web/"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif"
         :publishing-directory "c:/Web/"
         :publishing-function org-publish-attachment
         )

        ("FCS-DSA-org"
         :base-directory "~/Cours/FCS-DSA/org/web"
         :auto-index t
         :index-filename "index.org"
         :index-title "Index"
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :recursive t
         :base-extension "org"
         :body-only t
         :publishing-directory "c:/Web/FCS-DSA/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :htmlized-source t
         :auto-preamble nil
         :auto-postamble nil
         :style-include-default nil
         :style-include-scripts nil
         :LaTeX-fragments nil
         :html-preamble publish-preamble
         :html-postamble publish-postamble
         :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
         )
        ("CQP-org"
         :base-directory "~/Cours/CQP-ArTech/org"
         :auto-index t
         :index-filename "index.org"
         :index-title "Index"
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :recursive t
         :base-extension "org"
         :body-only t
         :publishing-directory "c:/Web/CQP/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :htmlized-source t
         :auto-preamble nil
         :auto-postamble nil
         :style-include-default nil
         :style-include-scripts nil
         :LaTeX-fragments nil
         :html-preamble nil
         :html-postamble nil
         :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
         )
        ("Min-IA-org"
         :base-directory "~/Cours/Min-IA/org/web"
         :auto-index t
         :index-filename "index.org"
         :index-title "Index"
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :recursive t
         :base-extension "org"
         :body-only t
         :publishing-directory "c:/Web/Min-IA/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :htmlized-source t
         :auto-preamble nil
         :auto-postamble nil
         :style-include-default nil
         :style-include-scripts nil
         :LaTeX-fragments nil
         :html-preamble nil
         :html-postamble nil
         :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
         )
        ("Maj-ModCal-org"
         :base-directory "~/Cours/Maj-ModCal/Org"
         :auto-index t
         :index-filename "index.org"
         :index-title "Index"
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :recursive t
         :base-extension "org"
         :body-only t
         :publishing-directory "c:/Web/Maj-ModCal/"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :htmlized-source t
         :auto-preamble nil
         :auto-postamble nil
         :style-include-default nil
         :style-include-scripts nil
         :LaTeX-fragments nil
         :html-preamble nil
         :html-postamble nil
         :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
         )
        ("FCS-DSA-static"
         :base-directory "~/Cours/FCS-DSA/org/"
         :recursive t
         :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
         :publishing-directory "c:/Web/FCS-DSA/"
         :publishing-function org-publish-attachment
         :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
         )
        ("CQP-static"
         :base-directory "~/Cours/CQP-ArTech/org/"
         :recursive t
         :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
         :publishing-directory "c:/Web/CQP/"
         :publishing-function org-publish-attachment
         :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
         )
        ("Min-IA-static"
         :base-directory "~/Cours/Min-IA/org/"
         :recursive t
         :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
         :publishing-directory "c:/Web/Min-IA/"
         :publishing-function org-publish-attachment
         :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
         )
        ("Maj-ModCal-static"
         :base-directory "~/Cours/Maj-ModCal/org/"
         :recursive t
         :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
         :publishing-directory "c:/Web/Maj-ModCal/"
         :publishing-function org-publish-attachment
         :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
         )

        ("FCS-DSA-site" :components ("Web-inherit" "FCS-DSA-org" "FCS-DSA-static"))
        ("CQP-site" :components ("Web-inherit" "CQP-org" "CQP-static"))
        ("Min-IA-site" :components ("Web-inherit" "Min-IA-org" "Min-IA-static"))
        ("Maj-ModCal-site" :components ("Web-inherit" "Maj-ModCal-org" "Maj-ModCal-static"))
        ("Personal-site" :components ("Web-inherit" "Personal-org" "Personal-static"))
        ("Personal-org"
                                        ; :select-tags     ("@WEB")
         :title "Fabrice Popineau Personal Web Page"
                                        ; :include ("index.org")
                                        ; :exclude "\\.org$"
         :base-directory "~/Web/org/"
         :auto-index t
         :index-filename "index.org"
         :index-title "Index"
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :recursive t
         :base-extension "org"
         :body-only t
         :publishing-directory "c:/Web/Personal"
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :htmlized-source t
         :auto-preamble nil
         :auto-postamble nil
         :style-include-default nil
         :style-include-scripts nil
         :LaTeX-fragments nil
         :html-preamble publish-preamble
         :html-postamble publish-postamble
         :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
         )
        ("Personal-static"
         :base-directory "~/Web/org/"
         :recursive t
         :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
         :publishing-directory "c:/Web/Personal/"
         :publishing-function org-publish-attachment
         :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
         )
        )))

(defun my-src-block-filter (text backend props)
  (when (eq backend 'e-html)
    (when (string-match "<pre class=\\\"src src-\\([a-z]*\\)\\\">" text)
      (setq text
            (replace-match (format "<script type=\"syntaxhighlighter\" class=\"src src-%s brush: %s\"><![CDATA[" (match-string 1 text) (match-string 1 text)) nil nil text)))
    (when (string-match "</pre>" text)
      (setq text (replace-match "]]></script>" nil nil text)))
    ;; (when (string-match "#\\+END_SRC.*$" text)
    ;;   (setq text (replace-match "" nil nil text)))
    ;; (when (string-match "#\\+BEGIN_SRC.*" text)
    ;;   (setq text (replace-match "" nil nil text)))
    text))

;; (add-to-list 'org-export-filter-src-block-functions #'my-src-block-filter)
(setq org-export-filter-src-block-functions nil)

(provide 'fp-org)
