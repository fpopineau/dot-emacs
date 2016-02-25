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

;; Code:

;;
;; * General
;;
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
                                        ;                       org-favtable
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

;; Other packages you should have installed
                                        ; 1. htmlize
                                        ;    Good for syntax highlighting code blocks when exporting to HTML
                                        ; 2. org-checklist
                                        ;    Good for repeating tasks where you've used checkboxes
                                        ;    and want them de-selected when marking the task as done.
                                        ;    http://orgmode.org/worg/org-contrib/#repofile-contrib-lisp-org-checklist.el
;; (load-file (expand-file-name "~/.emacs.d/org-checklist.el"))

;; org-jira
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/org-jira/"))
;; (require 'org-jira)
;; (setq jiralib-url "https://crd-jira.qualcomm.com/jira")



;; * Org initialization and hooks

;; Hook to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                              'org-update-all-dblocks t t)))

;; Hook to display dormant article in Gnus
(add-hook 'org-follow-link-hook
          (lambda ()
            (if (eq major-mode 'gnus-summary-mode)
                (gnus-summary-insert-dormant-articles))))

(add-hook 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

(add-hook 'org-mode-hook 'org-display-inline-images)

(setq org-startup-indented t)

(require 'org-cua-dwim)
(org-cua-dwim-activate)

(setq org-pretty-entities t)
(setq org-fast-tag-selection-single-key 'expert)
(setq org-fontify-done-headline t)
(setq org-fontify-emphasized-text t)
(setq org-footnote-auto-label 'confirm)
(setq org-footnote-auto-adjust t)
(setq org-footnote-define-inline nil)
(setq org-hide-emphasis-markers nil)
(setq org-icalendar-include-todo 'all)
(setq org-list-indent-offset 0)
(setq org-link-frame-setup '((gnus . gnus) (file . find-file-other-window)))
(setq org-link-mailto-program '(browse-url-mail "mailto:%a?subject=%s"))
(setq org-log-done (quote time))
(setq org-log-note-headings
      '((done . "CLOSING NOTE %t") (state . "State %-12s %t") (clock-out . "")))
(setq org-priority-start-cycle-with-default nil)
(setq org-refile-targets '((nil :maxlevel . 1)
                           ("notes.org" :regexp . "Viewed")
                           (org-agenda-files :maxlevel . 3)))

;; ;;; Exclude DONE state tasks from refile targets
;; (defun my/verify-refile-target ()
;;   "Exclude todo keywords with a DONE state from refile targets"
;;   (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
;;   (save-excursion (org-goto-first-child)))
;; (setq org-refile-target-verify-function 'my/verify-refile-target)
;;                                         ; Allow refile to create parent tasks with confirmation
(setq org-refile-use-outline-path t)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache t)
(setq org-return-follows-link t)
(setq org-reverse-note-order t)
(setq org-scheduled-past-days 100)
(setq org-show-following-heading '((default nil) (occur-tree t)))
(setq org-show-hierarchy-above '((default nil) (tags-tree . t)))
(setq org-show-siblings '((default) (isearch t) (bookmark-jump t)))
(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-stuck-projects '("+LEVEL=1" ("NEXT" "TODO" "DONE")))
(setq org-tag-persistent-alist '(("Write" . ?w) ("Read" . ?r)))
(setq org-tag-alist
      '((:startgroup . nil)
        ("Write" . ?w) ("Trad" . ?t) ("Read" . ?r) ("Proofread" . ?f) ("RDV" . ?R)
        ("View" . ?v) ("Listen" . ?l)
        (:endgroup . nil)
        (:startgroup . nil) ("@Online" . ?O) ("@Offline" . ?F)
        (:endgroup . nil)
        ("Print" . ?P) ("Code" . ?c) ("Patch" . ?p) ("Bug" . ?b)
        ("Twit" . ?i) ("Tel" . ?T) ("Buy" . ?B) ("Doc" . ?d) ("Mail" . ?@)))
;; Display tags farther right
(setq org-agenda-tags-column -102)
;; (setq org-tags-column -74)
(setq org-tags-match-list-sublevels t)
;; (setq org-todo-keywords '((type "STRT" "NEXT" "TODO" "WAIT" "|" "DONE" "DELEGATED" "CANCELED")))
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "DELEGATED(d@)" "WAITING(w@)" "|" "DONE(o@)" "CANCELED(c@)")))
(setq org-use-property-inheritance t)
(setq org-use-fast-todo-selection t)

;; (setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; save file
(setq org-clock-persist-file (expand-file-name "org-clock-save.el" prelude-savefile-dir))
;; Enable auto clock resolution for finding open clocks
;; (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
(setq org-clock-sound t)
(setq org-insert-heading-respect-content nil)
(setq org-id-method 'uuidgen)
(setq org-combined-agenda-icalendar-file "~/Org/FPO.ics")
(setq org-icalendar-combined-name "Fabrice Popineau ORG")
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "Europe/Paris")
(setq org-icalendar-store-UID t)

;; * Bibliography
(setq bog-notes-directory (expand-file-name "./")
      bog-bib-directory (expand-file-name "c:/temp/bibs")
      bog-file-directory (expand-file-name "c:/temp/citekey-files")
      bog-stage-directory (expand-file-name "c:/temp/stage"))

;;
;; * Org-GCal
;;

(setq org-gcal-down-days 1200)

(setq org-gcal-up-days 1200)

(setq org-gcal-client-id "306710974378-6ckee8kop51cj49ih9utbngucsq5bbq3.apps.googleusercontent.com")

(setq org-gcal-client-secret "56IAVEQFYqV7LTar6bf36pKI")

(setq org-gcal-dir (expand-file-name "~/Org/"))

(setq org-gcal-token-file (concat prelude-savefile-dir "/org-gcal-token"))

(setq org-gcal-file-alist '(("fabrice.popineau@gmail.com" . "CalendrierCS.org")
                            ("rknat5om4veh52cm6i60avu2b8@group.calendar.google.com" . "CalendrierPerso.org")))

(setq org-gcal-auto-archive nil)


;; * Agenda

(setq org-timer-default-timer 20)
(setq org-confirm-babel-evaluate nil)
(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

(setq org-clock-idle-time 15)
;; (setq org-time-stamp-rounding-minutes (quote (1 1)))

;; (setq org-agenda-clock-consistency-checks
;;       (quote (:max-duration "4:00"
;;                             :min-duration 0
;;                             :max-gap 0
;;                             :gap-ok-around ("4:00"))))

;; Agenda clock report parameters
;; (setq org-agenda-clockreport-parameter-plist
;;       (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Agenda log mode items to display (closed and state changes by default)
;; (setq org-agenda-log-mode-items (quote (state)))

(setq org-id-uuid-program "uuidgen")
;;    (setq org-modules '(org-bbdb org-bibtex org-docview org-gnus org-id org-protocol org-info org-jsinfo org-irc org-w3m org-taskjuggler org-learn))
(setq org-modules '(org-bbdb org-bibtex org-docview org-gnus org-protocol org-info org-jsinfo org-irc org-w3m org-taskjuggler org-learn))
(setq org-use-speed-commands t)
;; (setq org-use-speed-commands
;;       (lambda nil
;;         (and (looking-at org-outline-regexp-bol)
;;              (not (org-in-src-block-p t)))))
(setq org-src-tab-acts-natively t)
;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'org-src-lang-modes (quote ("lisp" . lisp)))
;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)

(setq org-speed-commands-user '(("P" . org-property-action)
				("z" . org-add-note)
				("N" . org-narrow-to-subtree)
				("W" . widen)))

(setq org-hide-block-startup t)
(setq org-highlight-latex-and-related '(latex))
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer t)
;; (setq org-clock-sound "c:/Windows/Media/chord.wav")

(setq org-goto-auto-isearch nil)
(setq org-image-actual-width 600)

;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("HOLD" ("WAITING" . t) ("HOLD" . t))
;;               (done ("WAITING") ("HOLD"))
;;               ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;               ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "steel blue" :weight bold)
              ("INFO" :foreground "blue" :weight bold)
              ("DELEGATED" :background "red" :foreground "black" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELED" :foreground "yellow" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "red" :weight bold)
;;               ("NEXT" :foreground "blue" :weight bold)
;;               ("REVIEW" :foreground "gold" :weight bold)
;;               ("DELVE" :foreground "DarkOrchid" :weight bold)
;;               ("DONE" :foreground "forest green" :weight bold)
;;               ("WAITING" :foreground "orange" :weight bold)
;;               ("HOLD" :foreground "magenta" :weight bold)
;;               ("CANCELLED" :foreground "NavajoWhite1" :weight bold)
;;               ("PHONE" :foreground "forest green" :weight bold))))

(setq org-plantuml-jar-path "~/bin/plantuml.jar")
(setq org-link-abbrev-alist
      '(("bugzilla" . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("google"   . "http://www.google.com/search?q=%s")
        ("gnugol"   . "shell:gnugol -o org %s")
        ("gmap"     . "http://maps.google.com/maps?q=%s")
        ("omap"     . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))

(setq org-attach-directory "~/Org/data/")
(setq org-link-display-descriptive nil)
(setq org-loop-over-headlines-in-active-region t)
(setq org-latex-create-formula-image-program 'imagemagick) ;; imagemagick
(setq org-allow-promoting-top-level-subtree t)
(setq org-description-max-indent 5)
(setq org-gnus-prefer-web-links nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
(setq org-contacts-files '("~/Org/Contacts.org"))
(setq org-crypt-key "Fabrice Popineau")
(setq org-enforce-todo-dependencies t)
; (setq org-mobile-directory "~/Dropbox/Org/")
; (setq org-mobile-files '("~/Dropbox/Org/" "~/Org/from-mobile.org"))
(setq org-fontify-whole-heading-line t)
;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.mm\\'" . default)
;;         ("\\.x?html?\\'" . default)
;;         ("\\.pdf\\'" . "mupdf %s")))
;; Generic / unsorted
(setq org-reveal-theme "night")
;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      '(("Effort_ALL" .
         "0 0:10 0:20 0:30 0:40 0:50 1:00 1:30 2:00 2:30 3:00 4:00 5:00 6:00 7:00 8:00")
        ("Progress_ALL" . "10% 20% 30% 40% 50% 60% 70% 80% 90%")
        ("Status_ALL" . "Work Leisure GTD WOT")))

;; columns shown in column mode (defaults to "%25ITEM %TODO %3PRIORITY %TAGS")
(setq org-columns-default-format "%80ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM %TAGS"
      org-tags-column -100)

(setq org-confirm-elisp-link-function nil)
(setq org-confirm-shell-link-function nil)
(setq org-context-in-file-links t)
(setq org-cycle-include-plain-lists nil)
;; Hide all blank lines inside folded contents of headings
(setq org-cycle-separator-lines 0)
(setq org-deadline-warning-days 7)
(setq org-default-notes-file "~/Org/notes.org")
(setq org-directory "~/Org/")
(setq org-ellipsis nil)
(setq org-email-link-description-format "%c: %.50s")
(setq org-support-shift-select t)
(setq org-completion-use-ido t)

;; Allow alphabetic lists to be recognized for lists
(setq org-alphabetical-lists t)

                                        ; Don't remove the highlighting after an occur search (C-c / /)
(setq org-remove-highlights-with-change nil)

(setq org-hide-leading-stars nil)
(setq org-yank-adjusted-subtrees t)

;; (setq org-list-demote-modify-bullet (quote (("+" . "-")
;;                                             ("*" . "-")
;;                                             ("1." . "-")
;;                                             ("1)" . "-"))))


;; flyspell mode for spell checking everywhere
;; FIXME: flyspell is killing org-archive on C-c $
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(setq org-ditaa-jar-path "/usr/share/java/ditaa.jar")
(setq org-plantuml-jar-path "~/bin/java/plantuml.jar")

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

;; Tags with fast selection keys
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

;;                                         ; Allow setting single tags without the menu
;; (setq org-fast-tag-selection-single-key (quote expert))

;; Hack to change the appearance of the checkboxes [X] via the ML
(font-lock-add-keywords
 'org-mode `(("\\[X\\]"
              (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                        "☑") ; ✔ ☐ ☑
                        nil)))
             ("\\[ \\]"
              (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                        "☐")
                        nil)))))



;; Org Keys

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-occur-link-in-agenda-files)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)

;; And allow ace-jump-mode within my org files
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)



;; Org Babel


;; Clocking

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)



;; Org Agenda

(setq org-agenda-bulk-mark-char "*")
(setq org-agenda-diary-file "~/Org/Diary.org")
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-entry-text-maxlines 10)
(setq org-agenda-file-regexp "\\.org\\'")
(setq org-agenda-files
      `("~/Org/"
        ; ,@(mapcan #'(lambda (dir)
        ;               (remove-if #'(lambda (f) (or (not (file-directory-p f))
        ;                                            (string-match "\\.\\.?$" f)))
        ;                          (directory-files dir t))) '("~/Projets/" "~/Cours/"))
	))

(setq org-agenda-include-diary nil)
(setq org-agenda-insert-diary-extract-time t)
;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
;; ;; Compact the block agenda view
;; (setq org-agenda-compact-blocks t)

;; ;; Sorting order for tasks on the agenda
;; (setq org-agenda-sorting-strategy
;;       (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
;;               (todo category-up priority-down effort-up)
;;               (tags category-up priority-down effort-up)
;;               (search category-up))))

;; ;; Enable display of the time grid so we can see the marker for the current time
;; (setq org-agenda-time-grid (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (830 1000 1200 1300 1500 1700))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-14t%s")
        (timeline . "  % s")
        (todo . " %i %-14:c")
        (tags . " %i %-14:c")
        (search . " %i %-14:c")))
(setq org-agenda-remove-tags t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-inherited-tags nil)
(setq org-agenda-skip-additional-timestamps-same-entry t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-sorting-strategy
      '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-sticky nil)
;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-window-frame-fractions '(0.0 . 0.5))
(setq org-agenda-deadline-faces
      '((1.0001 . org-warning)              ; due yesterday or before
        (0.0    . org-upcoming-deadline)))  ; due today or later
(org-agenda-to-appt)

(setq org-agenda-span 'day)

;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (define-key org-agenda-mode-map "q" 'bury-buffer))
;;           'append)

(setq org-agenda-persistent-filter t)

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

(setq org-agenda-window-setup 'current-window)



;; Org Agenda commands

;; quick tag keys
(setq org-agenda-custom-commands
      (quote (("d" todo "DONE|DEFERRED|CANCELLED" nil)
              ("w" todo "WAITING" nil)
              ("l" todo "DELEGATED" nil)
              ("W" agenda "" ((org-agenda-ndays 21)))
              ("A" agenda ""
                   ((org-agenda-skip-function
                     (lambda ()
                       (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
                    (org-agenda-ndays 1)
                    (org-agenda-overriding-header "Today's Priority #A tasks: ")))
              ("u" alltodo ""
                   ((org-agenda-skip-function
                     (lambda ()
                       (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "<[^>\n]+>")))
                    (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

;; (setq org-agenda-custom-commands
;;       `(
;;         ;; list of WP tasks for today
;;         (" " "Aujourd'hui" agenda "List of rendez-vous and tasks for today"
;;          ((org-agenda-span 1)
;;           (org-agenda-files '("~/Org/rdv.org" "~/Org/bzg.org"))
;;           (org-deadline-warning-days 10)
;;           (org-agenda-sorting-strategy
;;            '(todo-state-up time-up priority-up))))

;;         ;; list of WP tasks for today
;;         ("c" "Aujourd'hui" tags "+LEVEL=1+SCHEDULED=\"<today>\""
;;          ((org-agenda-span 1)
;;           (org-agenda-files '("~/Org/bzg.org" "~/Org/org.org" "~/Org/libre.org"))
;;           (org-deadline-warning-days 10)
;;           (org-agenda-sorting-strategy
;;            '(todo-state-up time-up priority-up))))

;;         ;; list of WP tasks for today
;;         ("%" "Rendez-vous" agenda* "Week RDV"
;;          ((org-agenda-span 'week)
;;           (org-agenda-files '("~/Org/rdv.org"))
;;           (org-deadline-warning-days 10)
;;           (org-agenda-sorting-strategy
;;            '(todo-state-up time-up priority-up))))

;;         ("n" todo "NEXT" ((org-agenda-sorting-strategy '(timestamp-up))))
;;         ("d" todo "TODO" ((org-agenda-sorting-strategy '(timestamp-up))))
;;         ("s" todo "STRT" ((org-agenda-sorting-strategy '(timestamp-up))))

;;         ("x" "Scheduled all" agenda "List of scheduled tasks for today"
;;          ((org-agenda-span 1)
;;           (org-agenda-entry-types '(:timestamp :scheduled))
;;           (org-agenda-sorting-strategy
;;            '(time-up todo-state-up priority-up))))

;;         ;; list of WP tasks for today
;;         ("X" "Upcoming deadlines" agenda "List of past and upcoming deadlines"
;;          ((org-agenda-span 1)
;;           (org-deadline-warning-days 15)
;;           (org-agenda-entry-types '(:deadline))
;;           (org-agenda-sorting-strategy
;;            '(time-up todo-state-up priority-up))))

;;         ;; list of Old deadlines
;;         ("Y" tags-todo "+SCHEDULED<=\"<now>\"")
;;         ("Z" tags-todo "+DEADLINE<=\"<now>\"")

;;         ("R" tags-todo "+Read+TODO={NEXT}" nil)
;;         ;; Everything that has a "Read" tag
;;         ("r" . "Read")
;;         ("rn" tags-todo "+Read+TODO={NEXT}" nil)
;;         ("rt" tags-todo "+Read+TODO={TODO}" nil)
;;         ("rs" tags-todo "+Read+TODO={STRT}" nil)
;;         ("rF" tags "+Read+@Offline" nil)

;;         ("W" tags-todo "+Write+TODO={NEXT}" nil)

;;         ;; Everything that has a "Read" tag
;;         ("w" . "Write")
;;         ("wn" tags-todo "+Write+TODO={NEXT}" nil)
;;         ("wt" tags-todo "+Write+TODO={TODO}" nil)
;;         ("ws" tags-todo "+Write+TODO={STRT}" nil)
;;         ("wF" tags "+Write+@Offline" nil)

;;         ))

;; ;; Custom agenda command definitions
;; (setq org-agenda-custom-commands
;;       (quote (("N" "Notes" tags "NOTE"
;;                ((org-agenda-overriding-header "Notes")
;;                 (org-tags-match-list-sublevels t)))
;;               ("h" "Habits" tags-todo "STYLE=\"habit\""
;;                ((org-agenda-overriding-header "Habits")
;;                 (org-agenda-sorting-strategy
;;                  '(todo-state-down effort-up category-keep))))
;;               (" " "Agenda"
;;                ((agenda "" nil)
;;                 (tags "REFILE"
;;                       ((org-agenda-overriding-header "Tasks to Refile")
;;                        (org-tags-match-list-sublevels nil)))
;;                 (tags-todo "-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Stuck Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
;;                 (tags-todo "-WAITING-CANCELLED/!NEXT"
;;                            ((org-agenda-overriding-header "Next Tasks")
;;                             (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
;;                             (org-agenda-todo-ignore-scheduled t)
;;                             (org-agenda-todo-ignore-deadlines t)
;;                             (org-agenda-todo-ignore-with-date t)
;;                             (org-tags-match-list-sublevels t)
;;                             (org-agenda-sorting-strategy
;;                              '(todo-state-down effort-up category-keep))))
;;                 (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
;;                            ((org-agenda-overriding-header "Tasks")
;;                             (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
;;                             (org-agenda-todo-ignore-scheduled t)
;;                             (org-agenda-todo-ignore-deadlines t)
;;                             (org-agenda-todo-ignore-with-date t)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-HOLD-CANCELLED/!"
;;                            ((org-agenda-overriding-header "Projects")
;;                             (org-agenda-skip-function 'bh/skip-non-projects)
;;                             (org-agenda-sorting-strategy
;;                              '(category-keep))))
;;                 (tags-todo "-CANCELLED+WAITING/!"
;;                            ((org-agenda-overriding-header "Waiting and Postponed Tasks")
;;                             (org-agenda-skip-function 'bh/skip-stuck-projects)
;;                             (org-tags-match-list-sublevels nil)
;;                             (org-agenda-todo-ignore-scheduled 'future)
;;                             (org-agenda-todo-ignore-deadlines 'future)))
;;                 (tags "-REFILE/"
;;                       ((org-agenda-overriding-header "Tasks to Archive")
;;                        (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
;;                        (org-tags-match-list-sublevels nil))))
;;                nil)
;;               ("r" "Tasks to Refile" tags "REFILE"
;;                ((org-agenda-overriding-header "Tasks to Refile")
;;                 (org-tags-match-list-sublevels nil)))
;;               ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
;;                ((org-agenda-overriding-header "Stuck Projects")
;;                 (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
;;               ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
;;                ((org-agenda-overriding-header "Next Tasks")
;;                 (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
;;                 (org-agenda-todo-ignore-scheduled t)
;;                 (org-agenda-todo-ignore-deadlines t)
;;                 (org-agenda-todo-ignore-with-date t)
;;                 (org-tags-match-list-sublevels t)
;;                 (org-agenda-sorting-strategy
;;                  '(todo-state-down effort-up category-keep))))
;;               ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
;;                ((org-agenda-overriding-header "Tasks")
;;                 (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
;;                 (org-agenda-sorting-strategy
;;                  '(category-keep))))
;;               ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
;;                ((org-agenda-overriding-header "Projects")
;;                 (org-agenda-skip-function 'bh/skip-non-projects)
;;                 (org-agenda-sorting-strategy
;;                  '(category-keep))))
;;               ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
;;                ((org-agenda-overriding-header "Waiting and Postponed tasks"))
;;                (org-tags-match-list-sublevels nil))
;;               ("A" "Tasks to Archive" tags "-REFILE/"
;;                ((org-agenda-overriding-header "Tasks to Archive")
;;                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
;;                 (org-tags-match-list-sublevels nil))))))



;; * Capture - Refile - Archive

;; Org Capture templates

(setq org-capture-templates-contexts nil)

(setq org-capture-templates
      ;; for org/rdv.org
      '(

        ;; Mise, put it on top of my main .org file
        (" " "Misc" entry (file "~/Org/notes.org")
         "* TODO %a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?"
         :prepend t :immediate-finish t)

        ("t" "Todo Item" entry (file+headline "~/Org/tasks.org" "Tasks")
         "* TODO %^{Action oriented description}\n	DEADLINE: %^{What is your Deadline for this?}t\n	%U (/Task created/) \\\n	Via %K\n%?" :clock-in t :clock-resume t)
        ("v" "Vendor or Product" entry (file+olp "~/Org/Projects.org" "Vendors" "Misc Vendor or Product")
         (file "~/Org/vendor_product.tmplt") :clock-in t :clock-resume t)
        ("p" "Projects or Repeating" entry (file+headline "~/Org/projects.org" "Projects")
         (file "~/Org/projects.tmplt") :clock-in t :clock-resume t)

        ("m" "Meeting or Consultation" entry (file+headline "~/Org/Meetings.org" "Meetings")
         "* INFO %?	:needsrefile:\n        :PROPERTIES:\n        :ProjectManager:\n        :Attendees:\n        :Location:\n:END:\n        %U (/Meeting Started/) \\\n        Via %K\n\n        Notes\n\n	Action Items")

        ("s" "Support Production or Oncall Consultation" entry (file+headline "~/Org/projects.org" "Support Production")
         (file "~/Org/support.tmplt") :clock-in t :clock-resume t)
        ("i" "Information or Ideas" entry (file+headline "~/Org/info.org" "Incoming Ideas")
         (file "~/Org/info.tmplt") :clock-in t :clock-resume t)
        ("k" "Kudos to You" entry (file+olp "~/Org/info.org" "Development Planning" "Kudos")
         (file "~/Org/kudos.tmplt") )
        ("h" "Home Personal Item" entry (file+headline "~/Org/Personal.org" "Personal")
         (file "~/Org/personal.tmplt") :clock-in t :clock-resume t :kill-buffer t)

        ;; for org/rdv.org
        ("r" "Bzg RDV" entry (file+headline "~/Org/rdv.org" "RDV")
         "* %a :RDV:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?" :prepend t)

        ;; Basement et garden
        ("g" "Garden" entry (file+headline "~/Org/garden.org" "Garden")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ;; for org/rdv.org
        ("B" "Blog" entry (file+headline "~/Org/bzg.org" "Blog")
         "* %a :Write:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i%?" :prepend t)

        ;; Basement et garden
        ("b" "Basement" entry (file+headline "~/Org/bzg.org" "Basement")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ;; Boite (lml) et cours
        ("b" "Boîte" entry (file+headline "~/Org/bzg.org" "Boîte")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("O" "OLPC" entry (file+headline "~/Org/libre.org" "OLPC")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("e" "Emacs" entry (file+headline "~/Org/libre.org" "Emacs")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend nil)

        ("w" "Wikipedia" entry (file+headline "~/Org/libre.org" "Wikipedia")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("i" "ITIC" entry (file+headline "~/Org/libre.org" "itic")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("j" "jecode" entry (file+headline "~/Org/libre.org" "jecode")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("k" "Kickhub" entry (file+headline "~/Org/bzg.org" "Kickhub")
         "* NEXT %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ("s" "ShareLex" entry (file+headline "~/Org/libre.org" "ShareLex")
         "* TODO %?%a\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ;; Informations
        ("I" "Information")
        ("Ir" "Information read" entry
         (file+headline "~/Org/garden.org" "Infos")
         "* TODO %?%a :Read:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i"
         :prepend t)

        ("IR" "Information read (!)" entry
         (file+headline "~/Org/garden.org" "Infos")
         "* TODO %?%a :Read:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i"
         :prepend t :immediate-finish t)

        ("Ic" "Information read (clocking)" entry
         (file+headline "~/Org/garden.org" "Infos")
         "* TODO %?%a :Read:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i"
         :prepend t :clock-in t)

        ("IC" "Information read (keep clocking)" entry
         (file+headline "~/Org/garden.org" "Infos")
         "* TODO %?%a :Read:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i"
         :prepend t :clock-in t :immediate-finish t :clock-keep t :jump-to-captured t)

        ;; ("o" "Org")
        ;; ("ot" "Org Test" entry (file+headline "~/Org/org.org" "To test")
        ;;  "* TODO %?%a :Code:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)
        ;; ("of" "Org FR" entry (file+headline "~/Org/org.org" "Current ideas")
        ;;  "* TODO %?%a :Code:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)
        ;; ("ob" "Org Bug" entry (file+headline "~/Org/org.org" "Mailing list")
        ;;  "* NEXT %?%a :Bug:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)
        ;; ("op" "Org Patch" entry (file+headline "~/Org/org.org" "Mailing list")
        ;;  "* NEXT [#A] %?%a :Patch:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)
        ;; ("ow" "Worg" entry (file+headline "~/Org/org.org" "Worg")
        ;;  "* TODO [#A] %?%a :Worg:\n  :PROPERTIES:\n  :CAPTURED: %U\n  :END:\n\n%i" :prepend t)

        ;;         ("t" "todo" entry (file+headline "~/Org/notes.org" "Tasks")
;;          "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:" :clock-in t :clock-resume t)
;;         ("d" "Done task" entry
;;          (file+headline "~/Org/notes.org" "Tasks")
;;          "* DONE %^{Task}\nSCHEDULED: %^t\n%?\n")
;;         ("q" "Quick task" entry
;;          (file+headline "~/Org/notes.org" "Tasks")
;;          "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n"
;;          :immediate-finish t)
;;         ("r" "respond" entry (file+headline "~/Org/notes.org" "Respond")
;;          "* TODO Respond to %:from on %:subject\n%U\n%A\n" :clock-in t :clock-resume t :immediate-finish t)
;;         ("n" "note" entry (file+headline "~/Org/notes.org" "Notes")
;;          "* %? :NOTE:\n%U\n%A\n" :clock-in t :clock-resume t)
;;         ("j" "Journal" entry (file+datetree "~/Org/diary.org")
;;          "* %?\n%U\n" :clock-in t :clock-resume t)
;;         ("l" "Ledger entries")
;;         ("lm" "MBNA" plain
;;          (file "~/Org/ledger")
;;          "%(org-read-date) %^{Payee}\nLiabilities:MBNA\nExpenses:%^{Account}  $%^{Amount}\n" :immediate-finish)
;;         ("ln" "No Frills" plain
;;          (file "~/Org/ledger")
;;          "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills\nLiabilities:MBNA\nAssets:Wayne:Groceries  $%^{Amount}\n" :immediate-finish)
;;         ("lc" "Cash" plain
;;          (file "~/Org/ledger")
;;          "%(org-read-date) * %^{Payee}\nExpenses:Cash\nExpenses:%^{Account}  %^{Amount}\n")
;;         ("b" "BibTeX entries")
        ;;         ("ba" "Articles" entry
        ;;          (file "~/Projets/Papers/Bibliography.org")
        ;;          "* READ %?\n\n%a\n\n%:author (%:year): %:title\n\nIn %:journal), %:pages."
;;         ("a" "Articles entries" entry
;;          (file+datetree "~/personal/books.org" "Inbox")
;;          "* %^{Title}  %^g
;;  %i
;;  *Author(s):* %^{Author} \\\\
;;  *ISBN:* %^{ISBN}

;;  %?

;;  *Review on:* %^t \\
;;  %a
;;  %U"
;;          :clock-in :clock-resume)
;;         ("o" "Templates for capturing urls in the Web browser.")
;;         ("on" "org-protocol" entry (file+headline "~/Org/notes.org" "Org")
;;          "* TODO Review %c\n%U\n" :immediate-finish t)
;;         ("ol" "Lisp" entry (file+headline "~/Org/Lisp.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("oe" "Emacs" entry (file+headline "~/Org/Emacs.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("og" "Georges" entry (file+headline "~/Projets/Georges/Georges-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("oy" "Youssef" entry (file+headline "~/Projets/Youssef/Youssef-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("oj" "Jean-Paul" entry (file+headline "~/Projets/PsyGolog/PsyGolog-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("ox" "AIXI" entry (file+headline "~/Projets/AIXI/AIXI-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("of" "FISDA" entry (file+headline "~/Cours/FCS-DSA/Org/FCS-DSA-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("oc" "CQP-ArTech" entry (file+headline "~/Cours/CQP-ArTech/Org/CQP-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")
;;         ("oi" "Maj-InfoTh" entry (file+headline "~/Cours/Maj-InfoTh/Org/InfoTh-Progress.org" "Readings")
;;          "* IDEA %c\n%U\n%A\n")
;;         ("oa" "Min-IA" entry (file+headline "~/Cours/Min-IA/Org/Min-IA-Progress.org" "Readings")
;;          "* TODO Review %c\n%U\n%A\n")

;;         ("p" "Phone call" entry (file+headline "~/Org/notes.org" "Phone")
;;          "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;         ("u"
;;          "Default template"
;;          entry
;;          (file+headline "~/Org/notes.org" "Notes")
;;          "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
;;          :empty-lines 1)
;;         ("h" "Habit" entry (file+headline "~/Org/notes.org" "Habit")
;;          "* NEXT %?\n%U\n%A\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
;;         ("w" "Work in progress")
;;         ("wl" "Lisp" entry (file "~/Org/Lisp.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("we" "Emacs" entry (file "~/Org/Emacs.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("(widget-get  )" "Georges" entry (file "~/Projets/Georges/Georges-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wy" "Youssef" entry (file "~/Projets/Youssef/Youssef-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wj" "Jean-Paul" entry (file "~/Projets/PsyGolog/PsyGolog-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wx" "AIXI" entry (file "~/Projets/AIXI/AIXI-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wf" "FISDA" entry (file "~/Cours/FCS-DSA/Org/FCS-DSA-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wc" "CQP-ArTech" entry (file "~/Cours/CQP-ArTech/Org/CQP-Progress.org")
;;          "* IDEA %?\n%U\n%A\n")
;;         ("wi" "Maj-InfoTh" entry (file "~/Cours/Maj-InfoTh/Org/InfoTh-Progress.org")
;;          "* IDEA %?\n%Ubn%A\n")
;;         ("wa" "Min-IA" entryG(file "~/Cours/Min-IA/Org/Min-IA-Progress.org")
;;          "* IDEA %?\n%Uen%A\n")

;;         ("W" "Websites" checkitem (file+olp "~/Org/notes.org" "Websites" "To view")
;;          "| %c | %^{Description}")

        ))



;; Org Export

(setq org-export-default-language "fr")
(setq org-export-coding-system 'utf-8)
(setq org-export-backends '(latex odt icalendar html ascii rss koma-letter))
(setq org-export-highlight-first-table-line t)
(setq org-export-html-extension "html")
(setq org-export-html-with-timestamp nil)
(setq org-export-skip-text-before-1st-heading nil)
(setq org-export-with-LaTeX-fragments t)
(setq org-export-with-archived-trees nil)
(setq org-export-with-drawers '("HIDE"))
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts '{})
(setq org-export-with-tags 'not-in-toc)
(setq org-export-with-timestamps t)
(setq org-export-with-toc nil)
(setq org-export-with-priority t)
(setq org-export-dispatch-use-expert-ui nil)
(setq org-export-babel-evaluate t)
(setq org-taskjuggler-default-project-duration 2000)
(setq org-taskjuggler-target-version 3.0)
(setq org-publish-timestamp-directory
      (expand-file-name  "org-timestamps/" prelude-savefile-dir))


;; * HTML exporter

(setq org-html-head "")
(setq org-html-head-include-default-style nil)
(setq org-html-inline-images t)
(setq org-html-htmlize-output-type 'css)

(setq org-html-special-string-regexps
      (append '(
                ("\\<LuaLaTeX\\>" . "LUA<span class=\"TypoLATEX\">L<span class=\"A\">A</span><span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span></span>")
                ("\\<LaTeX\\>" . "<span class=\"TypoLATEX\">L<span class=\"A\">A</span><span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span></span>")
                ("\\<LuaTeX\\>" . "LUA<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>")
                ("\\<ConTeXt\\>" . "CON<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>t")
                ("\\<TeX\\>" . "<span class=\"TypoTEX\">T<span class=\"E\">E</span>X</span>")
                )
              org-html-special-string-regexps))


(defadvice org-html-convert-special-strings (around org-html-convert-special-strings-around)
  "Take case into account in `org-html-convert-special-strings'."
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'org-html-convert-special-strings)

;; (setq org-table-export-default-format "orgtbl-to-csv")


;; * LaTeX exporter


(setq org-latex-listings 'minted)
(setq org-latex-listings 't)
;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

;; (setq org-latex-pdf-process
;;       '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f" "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

(plist-put org-format-latex-options :scale 1.0)

;; (setq org-latex-pdf-process
;;       '("latexmk -outdir=./pdf -lualatex -pv %f")
;;       )

(setq org-create-formula-image-latex-command "pdflatex")
(setq org-create-formula-image-convert-command "c:/MSys2/mingw64/bin/convert.exe")

(setq org-latex-pdf-process
      '("latexmk -cd -lualatex -bibtex -pv %f")
      )

(add-to-list
 'org-latex-packages-alist
 '("" "fpsupelec-snippet" t)
 )

;; (setq org-latex-create-formula-image-program 'imagemagick)

(setq org-export-allow-bind-keywords t)
(setq org-publish-list-skipped-files nil)

(setq org-html-table-row-tags
      (cons '(cond (top-row-p "<tr class=\"tr-top\">")
                   (bottom-row-p "<tr class=\"tr-bottom\">")
                   (t (if (= (mod row-number 2) 1)
                          "<tr class=\"tr-odd\">"
                        "<tr class=\"tr-even\">")))
            "</tr>"))

(defun publish-prepare ()
  (setq org-html-coding-system 'utf-8)
  (setq make-backup-files nil)
  (setq org-html-inline-images t)
  (setq org-export-allow-bind-keywords t)
  (setq org-confirm-babel-evaluate nil))

(defun publish-complete ()
  (setq make-backup-files t)
  (setq org-confirm-babel-evaluate t))

(defvar fp-inside-questions nil)

(defun fp-reset-inside-questions (&rest args)
  (setq fp-inside-questions ()))

(add-hook 'org-export-before-parsing-hook 'fp-reset-inside-questions)
(defun fp-org-latex-compute-exam-section (level numbered)
  ; (message "level %s numbered %s" level numbered)
  (case level
    (1 (setq fp-inside-questions nil)
       '("\\section{%s}" "\\end{questions}"))
    (2 (if fp-inside-questions
           '("\\myquestion{%s}" . "")
         (progn
           (setq fp-inside-questions t)
           '("\\begin{questions}\n\\myquestion{%s}" . ""))))
    (3 '("\\begin{solution} %% %s" "\\end{solution}"))
    (t '("%s" "")))
  )

(defun fp-org-latex-compute-examshort-section (level numbered)
                                        ; (message "level %s numbered %s" level numbered)
  (case level
    (1 '("\\begin {questions} %% %s" "\\end{questions}"))
    (2 '("\\myquestion{%s}" . ""))
    (3 '("\\begin{solution} %% %s" "\\end{solution}"))
    (t '("%s" "")))
  )

;;; Fix the \question[] "section" header of the exam class.
;;; At this point, it would almost make sense to derive a new backend
;;; specifically for this case.

 (defun fp-get-org-headline-string-element  (headline backend info)
  "Return the org element representation of an element.  Does not
    work with verbatim only headlines, e.g. \"* ~Verb~.\""
  (let ((prop-point (next-property-change 0 headline)))
    (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

(defun fp-latex-filter-headline-exam (headline backend info)
  "Replace question sections with \\question[] sections"
  ;;; Get the element we are looking at.
  (let* ((e (fp-get-org-headline-string-element headline backend info))
         (level (org-export-get-relative-level e info)))
    ;; (message "level = %s" level)
    (when (and (org-export-derived-backend-p backend 'latex)
               (= 2 level))
      ;; (message "headline = %s" headline)
      (if (string-match  "\\\\question\\[[^][]*\\]" headline)
          (progn
            ;; (message "headline1 = <%s>" headline)
            ;; (message "headline1 replaced = <%s>" (replace-regexp-in-string
            ;;                                         "\\\\question\\[[^][]*\\]" "\\\\question" headline))
                 (replace-regexp-in-string
                  "\\\\question\\[[^][]*\\]" "\\\\question" headline))

        ;; (message "headline2 = <%s>" headline)
        ;; (message "headline2 replaced = <%s>" (replace-regexp-in-string "\\\\question\\[[^[]*\\[\\([0-9][0-9]*\\)[^]]*\\]\s*\\]"
        ;;                                                               "\\\\question[\\1]"
        ;;                                                               headline))
        (replace-regexp-in-string "\\\\question\\[[^[]*\\[\\([0-9][0-9]*\\)[^]]*\\]\s*\\]"
                                  "\\\\question[\\1]"
                                  headline)))))

(defun fp-latex-filter-headline-exam (headline backend info)
  (message "********************\ninfo = %s" info)
  (message "type = %s" (org-element-type info))
  (message "level = %s" (plist-get info :level))
;;  (when (eq 'headline (org-element-type info))
;;    (message "headline = %s\nlevel = %s" headline
;;             (org-export-get-relative-level headline info)))
)
;(setq org-export-filter-headline-functions nil)
;; (add-to-list 'org-export-filter-headline-functions
;;             'fp-latex-filter-headline-exam)

(setq org-latex-classes
      '(("beamer" "\\documentclass[presentation]{beamer}\n     [NO-DEFAULT-PACKAGES]\n     [PACKAGES]\n     [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("poly" "\\documentclass[poly,french]{fpsupelec}\n     [NO-DEFAULT-PACKAGES]\n     [NO-PACKAGES]\n     [EXTRA]"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("td" "\\documentclass{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\fpremark{%s}\\fpnewtd" . "")
         ("\\Exercise %% %s" . "")
         ("\\begin{questiion} %% %s" "\\end{questiion}")
         ("\\begin{solution} %% %s" "\\end{solution}"))
        ("exam" "\\documentclass[addpoints,solution,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         fp-org-latex-compute-exam-section
         )
        ("examshort" "\\documentclass[addpoints,solution,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         fp-org-latex-compute-examshort-section
         )
        ("slides" "\\documentclass[presentation,compress,french,slides]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ; ("\\paragraph{%s}" . "\\paragraph*{%s}")
         )
        ("lecture" "\\documentclass[9pt,presentation,compress,french,lecture,slides]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ; ("\\paragraph{%s}" . "\\paragraph*{%s}")
         )
        ("IEEEtran" "\\documentclass{IEEEtran}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("llncs" "\\documentclass{llncs}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("acmproc" "\\documentclass{acm_proc_article-sp}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
        ("article" "\\documentclass[a4paper,french,article]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
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

;; Beamer
;; (setq org-beamer-outline-frame-title "Survol")
(add-to-list 'org-beamer-environments-extra
             '("only" "o" "\\only%a{%h%x" "}"))
(add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

;; ** Frame (beamer_act, not quite right)
;; *** The problem on this frame
;; I want Block 1 and Block 2 to occupy the same space on successive
;; slides.
;; *** OnlyEnv 1
;; :B_onlyenv:
;; :PROPERTIES:
;; :BEAMER_env: onlyenv
;; :BEAMER_ACT: <1>
;; :END:
;; **** Block 1
;; Text
;; *** OnlyEnv 2
;; :B_onlyenv:
;; :PROPERTIES:
;; :BEAMER_env: onlyenv
;; :BEAMER_ACT: <2>
;; :END:
;; **** Block 2
;; Text 2


;; (defun latex-buffer-caption-to-caption* ()
;;   (when org-beamer-export-is-beamer-p
;;     (replace-regexp "\\(\\\\caption\\)\\([[{]\\)" "\\1*\\2" nil
;;                     (point-min) (point-max))))

;; (add-hook 'org-export-latex-final-hook
;;           'latex-buffer-caption-to-caption* 'append)

;; This can be useful for exporting to Restas.
;; I like to export to html and immediately open in a browser tab
;;   Most of the time it is with a subtree. A tadbit annoying that
;;   certain properties are used only when you select the region
;;   vs. simplying having it narrowed.
;; See http://orgmode.org/manual/Export-options.html for other properties
(defun jsm/org-export-subtree-as-html-and-open ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (org-export-as-html-and-open 3)))
;; (define-key org-mode-map (kbd "<f12>") 'jsm/org-export-subtree-as-html-and-open)

(defun jsm/org-export-subtree-as-html-with-subtree-name-and-open (arg)
  (interactive "P")
  (let ((new-file-name (concat
                        (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (substring-no-properties (org-get-heading t t)))
                        ".html") ))
    (save-excursion
      (org-mark-subtree)
      (org-export-as-html arg 'hidden nil new-file-name nil "~/published")
      (switch-to-buffer new-file-name)
      (write-file "~/published" nil)
      (kill-buffer))
    new-file-name))
(define-key org-mode-map (kbd "<f9>") 'jsm/org-export-subtree-as-html-with-subtree-name-and-open)

(defun jsm/org-export-subtree-attach-to-email (arg)
  (interactive "P")
  (let ((exported-file-name
         (expand-file-name
          (concat "~/published/"
                  (jsm/org-export-subtree-as-html-with-subtree-name-and-open 3)))))
    ;; Need to create a plist with :path to file and :subject for file
    (mu4e-action-capture-message
     (list :path exported-file-name
           :subject "Exported by Org"))
    (mu4e-compose-attach-captured-message)))


;; Habits
;; position the habit graph on the agenda to the right of the default
;; (setq org-habit-graph-column 50)

;; (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))



;; --------------------------------------------------
;; Additional Hacks
;; --------------------------------------------------

;; Fold current subtree
;;  I like to fold a piece of text right from the middle of it...
;;  Manually, it would be: C-c C-p TAB
(defun org-fold-here()
  "Fold current subtree"
  (interactive)
  (outline-previous-visible-heading 1)
  (org-cycle))
(define-key org-mode-map (kbd "C-S-f") 'org-fold-here)

;; Thanks norang - Exactly what I like to do
;;  http://doc.norang.ca/org-mode.html#sec-15-21
(defun jsm/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun jsm/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (jsm/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'jsm/insert-heading-inactive-timestamp 'append)

;; Want to be able to narrow to subtree and clockin at the same time,
;;  then be able to do the opposite with a clockout and widen
(defun org-work-checkin ()
  (interactive)
  (org-clock-in)
  (org-narrow-to-subtree))
(defun org-work-checkout ()
  (interactive)
  (widen)
  (org-clock-out))
(define-key org-mode-map "\C-ci" 'org-work-checkin)
(define-key org-mode-map "\C-co" 'org-work-checkout)



;; (add-to-list 'Info-default-directory-list "c:/mirror/elisp/org-mode/doc")

;; (defun fp-org-hide-other ()
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading 'invisible-ok)
;;     (hide-other)
;;     (org-cycle)
;;     (org-cycle)
;;     (org-cycle)))

;; (defun fp-org-set-truncate-lines ()
;;   "Toggle value of truncate-lines and refresh window display."
;;   (interactive)
;;   (setq truncate-lines (not truncate-lines))
;;   ;; now refresh window display (an idiom from simple.el):
;;   (save-excursion
;;     (set-window-start (selected-window)
;;                       (window-start (selected-window)))))

;; (defun fp-org-make-org-scratch ()
;;   (interactive)
;;   (find-file "/tmp/publish/scratch.org")
;;   (gnus-make-directory "/tmp/publish"))


;; (setq org-speed-commands-user (quote (("0" . ignore)
;;                                       ("1" . ignore)
;;                                       ("2" . ignore)
;;                                       ("3" . ignore)
;;                                       ("4" . ignore)
;;                                       ("5" . ignore)
;;                                       ("6" . ignore)
;;                                       ("7" . ignore)
;;                                       ("8" . ignore)
;;                                       ("9" . ignore)

;;                                       ("a" . ignore)
;;                                       ("d" . ignore)
;;                                       ("h" . bh/hide-other)
;;                                       ("i" progn
;;                                        (forward-char 1)
;;                                        (call-interactively 'org-insert-heading-respect-content))
;;                                       ("k" . org-kill-note-or-show-branches)
;;                                       ("l" . ignore)
;;                                       ("m" . ignore)
;;                                       ("q" . bh/show-org-agenda)
;;                                       ("r" . ignore)
;;                                       ("s" . org-save-all-org-buffers)
;;                                       ("w" . org-refile)
;;                                       ("x" . ignore)
;;                                       ("y" . ignore)
;;                                       ("z" . org-add-note)

;;                                       ("A" . ignore)
;;                                       ("B" . ignore)
;;                                       ("E" . ignore)
;;                                       ("F" . bh/restrict-to-file-or-follow)
;;                                       ("G" . ignore)
;;                                       ("H" . ignore)
;;                                       ("J" . org-clock-goto)
;;                                       ("K" . ignore)
;;                                       ("L" . ignore)
;;                                       ("M" . ignore)
;;                                       ("N" . bh/narrow-to-subtree)
;;                                       ("P" . bh/narrow-to-project)
;;                                       ("Q" . ignore)
;;                                       ("R" . ignore)
;;                                       ("S" . ignore)
;;                                       ("T" . bh/org-todo)
;;                                       ("U" . bh/narrow-up-one-level)
;;                                       ("V" . ignore)
;;                                       ("W" . bh/widen)
;;                                       ("X" . ignore)
;;                                       ("Y" . ignore)
;;                                       ("Z" . ignore))))

;; ;; Custom Key Bindings
;; ; (global-set-key (kbd "<f12>") 'org-agenda)
;; (global-set-key (kbd "<f5>") 'bh/org-todo)
;; (global-set-key (kbd "<S-f5>") 'bh/widen)
;; (global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;; (global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;; (global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
;; (global-set-key (kbd "<f9> b") 'bbdb)
;; (global-set-key (kbd "<f9> c") 'calendar)
;; (global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;; (global-set-key (kbd "<f9> g") 'gnus)
;; (global-set-key (kbd "<f9> h") 'bh/hide-other)
;; (global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
;; (global-set-key (kbd "<f9> w") 'widen)
;; (global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)

;; (global-set-key (kbd "<f9> I") 'bh/punch-in)
;; (global-set-key (kbd "<f9> O") 'bh/punch-out)

;; (global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

;; (global-set-key (kbd "<f9> r") 'boxquote-region)
;; (global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;; (global-set-key (kbd "<f9> T") 'tabify)
;; (global-set-key (kbd "<f9> U") 'untabify)

;; (global-set-key (kbd "<f9> v") 'visible-mode)
;; (global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
;; (global-set-key (kbd "C-<f9>") 'previous-buffer)
;; (global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
;; (global-set-key (kbd "C-<f10>") 'next-buffer)
;; ; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; ; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; ; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; ;; I use C-M-r to start capture mode
;; (global-set-key (kbd "C-M-r") 'org-capture)
;; ;; I use C-c r to start capture mode when using SSH from my Android phone
;; (global-set-key (kbd "C-c r") 'org-capture)






;; (setq org-link-frame-setup (quote ((vm . vm-visit-folder)
;;                                    (gnus . org-gnus-no-new-news)
;;                                    (file . find-file))))





;; (global-auto-revert-mode 1)


;; (setq org-structure-template-alist
;;       (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
;;               ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
;;               ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
;;               ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
;;               ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
;;               ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
;;               ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
;;               ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
;;               ("H" "#+html: " "<literal style=\"html\">?</literal>")
;;               ("a" "#+begin_ascii\n?\n#+end_ascii")
;;               ("A" "#+ascii: ")
;;               ("i" "#+index: ?" "#+index: ?")
;;               ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))


;; (setq org-startup-folded 'content)

;; (setq org-alphabetical-lists t)

;; (setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
;;                                  ("/" italic "<i>" "</i>")
;;                                  ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
;;                                  ("=" org-code "<code>" "</code>" verbatim)
;;                                  ("~" org-verbatim "<code>" "</code>" verbatim))))

;; (setq org-use-sub-superscripts nil)

;; (setq org-odd-levels-only nil)

;; (run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; (setq org-enable-priority-commands t)
;; (setq org-default-priority ?E)
;; (setq org-lowest-priority ?E)

;; (setq org-export-allow-BIND t)


;; ;; Enable abbrev-mode
;; (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; (setq require-final-newline t)

;; (defun bh/insert-inactive-timestamp ()
;;   (interactive)
;;   (org-insert-time-stamp nil t t nil nil nil))

;; (defun bh/insert-heading-inactive-timestamp ()
;;   (save-excursion
;;     (org-return)
;;     (org-cycle)
;;     (bh/insert-inactive-timestamp)))

;; (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

;; (setq org-return-follows-link t)


;; (defun bh/prepare-meeting-notes ()
;;   "Prepare meeting notes for email
;;                  Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
;;   (interactive)
;;   (let (prefix)
;;     (save-excursion
;;       (save-restriction
;;         (narrow-to-region (region-beginning) (region-end))
;;         (untabify (point-min) (point-max))
;;         (goto-char (point-min))
;;         (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
;;           (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
;;         (goto-char (point-min))
;;         (kill-ring-save (point-min) (point-max))))))

;; (setq org-remove-highlights-with-change nil)

;; ;; (add-to-list 'Info-default-directory-list "~/git/org-mode/doc")



;; (setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

;;                                         ; (require 'org-mime)


;; (setq org-table-use-standard-references (quote from))

;; (setq org-file-apps (quote ((auto-mode . emacs)
;;                             ("\\.mm\\'" . system)
;;                             ("\\.x?html?\\'" . system)
;;                             ("\\.pdf\\'" . system))))

;;                                         ; Overwrite the current window with the agenda

;; (setq org-clone-delete-id t)

;; (setq org-cycle-include-plain-lists t)



;; ;; Don't enable this because it breaks access to emacs from my Android phone
;; (setq org-startup-with-inline-images nil)


;;
;; Bibliography and bibliography notes handling.
;; http://www-public.telecom-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
;; http://blog.modelworks.ch/?p=379
;; https://github.com/vikasrawal/orgpaper/
;;

;; The scheme is as follows:
;; - database is stored in a big org file
;; - bibtex file is extracted by org-bibtex whenever the bib file is older than the org file
;; - only 2 kind of links: bibtex and papers (more suitable to tasks)

(require 'reftex)
(require 'reftex-vars)
(require 'reftex-cite)
(require 'org-bibtex)
(require 'ox-bibtex)

(setq org-bibtex-autogen-keys t)

;; Export bibliography keyword

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
                                        (expand-file-name bibfile)
                                        "}\n\\bibliographystyle{"
                                        bibstyle
                                        "}\n")))
      ad-do-it)))

(ad-activate 'org-latex-keyword)

;; Re-define bibtex link type

(defun fp-bibtex-cite-export-handler (path desc format)
  ;; (message "my-rtcite-export-handler is called : path = %s, desc = %s, format ;;= %s" path desc format)
  (cond ((eq format 'latex)
         (if (or (not desc)
                 (equal 0 (search "bibtex:" desc)))
             (format "\\cite{%s}" path)
           (format "\\cite[%s]{%s}" desc path)))
        ((eq format 'html)
         (format "[%s]"
                 (mapconcat
                  (lambda (key)
                    (format "<a href=\"#%s\">%s</a>"
                            key
                            (or (cdr (assoc key org-bibtex-html-entries-alist))
                                key)))
                  (org-split-string path ",") ","))
         )))

(defun fp-strip-string (string)
  "Strip leading and trailing whitespace from the string."
  (replace-regexp-in-string
   (concat search-whitespace-regexp "$" ) ""
   (replace-regexp-in-string
    (concat "^" search-whitespace-regexp ) "" string)))

(defun fp-open-key-under-cursor (link)
  "Open key under the bibtex cursor. We search forward from
point to get a comma, or the end of the link, and then backwards
to get a comma, or the beginning of the link. that delimits the
keyword we clicked on. We then open link from bibtex file."
  (interactive)
  (let* ((object (org-element-context))
         (link-string (org-element-property :path object)))

    ;; we need the link path start and end
    (save-excursion
      (goto-char (org-element-property :begin object))
      (search-forward link-string nil nil 1)
      (setq link-string-beginning (match-beginning 0))
      (setq link-string-end (match-end 0)))

    ;; The key is the text between commas, or the link boundaries
    (save-excursion
      (if (search-forward "," link-string-end t 1)
          (setq key-end (- (match-end 0) 1)) ; we found a match
        (setq key-end link-string-end))) ; no comma found so take the end
    ;; and backward to previous comma from point which defines the start character
    (save-excursion
      (if (search-backward "," link-string-beginning 1 1)
          (setq key-beginning (+ (match-beginning 0) 1)) ; we found a match
        (setq key-beginning link-string-beginning))) ; no match found
    ;; save the key we clicked on.
    (setq bibtex-key (fp-strip-string (buffer-substring key-beginning key-end)))
    (set-text-properties 0 (length bibtex-key) nil bibtex-key)
    (org-open-link-from-string (format "[[file:%s::#%s]]" org-bibtex-file bibtex-key))
    ))

;; add functions to complete papers and bibtex entries
(defun org-bibtex-complete-link (&optional arg)
  "Create a bibtex link using reftex autocompletion."
  (concat "bibtex:"
          (mapconcat 'identity (reftex-do-citation nil t nil) ",")))

(org-add-link-type "bibtex" 'fp-open-key-under-cursor 'fp-bibtex-cite-export-handler)


;; Define papers link type

(defvar *fp-papers-directory*
  (expand-file-name "~/Papers/"))

(defun fp-open-paper-under-cursor (link)
  (org-open-link-from-string (format "[[file:%s%s]]" *fp-papers-directory* link)))

(defun fp-paper-cite-export-handler (path desc format)
  (cond ((eq format 'latex)
         (format "\\url{%s%s}" *fp-papers-directory* path))
        ((eq format 'html)
         (format "<a href=\"%s%s\">%s</a>" *fp-papers-directory* path (or desc path)))
        ))
(defun org-papers-complete-link (&optional arg)
  "Create a papers link using ido autocompletion."
  (concat "papers:"
          (substring
           (ido-read-file-name "papers: " "~/Papers/")
           (length *fp-papers-directory*))
          )
  )

(org-add-link-type "papers" 'fp-open-paper-under-cursor 'fp-paper-cite-export-handler)

;; Setup org-reftex when entering a new org document

(defvar *fp-default-org-bibtex-file*
  (expand-file-name "~/Papers/Qiqqa.org"))

(defun fp-find-org-bibtex-file ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((case-fold-search t)
            (re "^[ \t]*#\\+BIBLIOGRAPHY:[ \t]+\\([^ \t\n]+\\)"))
        (if (save-excursion
              (or (re-search-forward re nil t)
                  (re-search-backward re nil t)))
            (concat (match-string 1) ".org"))))))

;; TODO: something wrong here. How to decide that we are writing a paper?
;; Either insert a template of headers or define a minor mode.

(defun org-reftex-setup ()
  (interactive)
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (let ((paper-p (fp-find-org-bibtex-file)))
         (when paper-p
                                        ; Reftex should use the org file as master file. See C-h v TeX-master for infos.
           (setq TeX-master t)
           (turn-on-reftex)
                                        ;enable auto-revert-mode to update reftex when bibtex file changes on disk
           (global-auto-revert-mode t)

           (setq org-bibtex-file
                 (or (fp-find-org-bibtex-file)
                     *fp-default-org-bibtex-file*))
           (setq reftex-default-bibliography
                 (list (replace-regexp-in-string "\.org$" ".bib" org-bibtex-file)))

           (reftex-parse-all)
                                        ;add a custom reftex cite format to insert links
           (reftex-set-cite-format '((?b . "[[bibtex:%l]]")
                                     (?p . "[[papers:%U][%t]]")))
           ;; org-reftex-citation prevents to change reftex-cite-format
           ;; besides, it doesn't do much more than verifying bibliography
           ;; is set up.
           (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
           ;; Useless. Click on the link.
           ;; (define-key org-mode-map (kbd "C-c (") 'org-reftex-search))
           ))))

(add-hook 'org-mode-hook 'org-reftex-setup)


;; ;; Skeletons
;; ;;
;; ;; sblk - Generic block #+begin_FOO .. #+end_FOO
;; (define-skeleton skel-org-block
;;   "Insert an org block, querying for type."
;;   "Type: "
;;   "#+begin_" str "\n"
;;   _ - \n
;;   "#+end_" str "\n")

;; (define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; ;; ;; splantuml - PlantUML Source block
;; ;; (define-skeleton skel-org-block-plantuml
;; ;;   "Insert a org plantuml block, querying for filename."
;; ;;   "File (no extension): "
;; ;;   "#+begin_src plantuml :file " str ".png :cache yes\n"
;; ;;   _ - \n
;; ;;   "#+end_src\n")

;; ;; (define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

;; ;; sdot - Graphviz DOT block
;; (define-skeleton skel-org-block-dot
;;   "Insert a org graphviz dot block, querying for filename."
;;   "File (no extension): "
;;   "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
;;   "graph G {\n"
;;   _ - \n
;;   "}\n"
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; ;; ;; sditaa - Ditaa source block
;; ;; (define-skeleton skel-org-block-ditaa
;; ;;   "Insert a org ditaa block, querying for filename."
;; ;;   "File (no extension): "
;; ;;   "#+begin_src ditaa :file " str ".png :cache yes\n"
;; ;;   _ - \n
;; ;;   "#+end_src\n")

;; ;; (define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; ;; selisp - Emacs Lisp source block
;; (define-skeleton skel-org-block-elisp
;;   "Insert a org emacs-lisp block"
;;   ""
;;   "#+begin_src emacs-lisp\n"
;;   _ - \n
;;   "#+end_src\n")

;; (define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)




;; ;;; Use full outline paths for refile targets - we file directly with IDO
;; (setq org-refile-use-outline-path t)
;; ;;; Refile settings

;; ;; The following setting is different from the document so that you
;; ;; can override the document org-agenda-files by setting your
;; ;; org-agenda-files in the variable org-user-agenda-files
;; ;;
;; ;; Agenda files are located either in Projects or in Cours









;; (defun bh/org-auto-exclude-function (tag)
;;   "Automatic task exclusion in the agenda with / RET"
;;   (and (cond
;;         ((string= tag "hold")
;;          t)
;;         ((string= tag "farm")
;;          t))
;;        (concat "-" tag)))

;; (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
;;           'append)


;;           '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
;;           'append)

;; (defun bh/set-agenda-restriction-lock (arg)
;;   "Set restriction lock to current task subtree or file if prefix is specified"
;;   (interactive "p")
;;   (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
;;          (tags (org-with-point-at pom (org-get-tags-at))))
;;     (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
;;       (save-restriction
;;         (cond
;;          ((and (equal major-mode 'org-agenda-mode) pom)
;;           (org-with-point-at pom
;;                              (org-agenda-set-restriction-lock restriction-type)))
;;          ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
;;           (org-agenda-set-restriction-lock 'file))
;;          (pom
;;           (org-with-point-at pom
;;                              (org-agenda-set-restriction-lock restriction-type))))))))













;; ;;
;; ;; Agenda sorting functions
;; ;;
;; (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

;; (defun bh/agenda-sort (a b)
;;   "Sorting strategy for agenda items.
;; Late deadlines first, then scheduled, then non-late deadlines"
;;   (let (result num-a num-b)
;;     (cond
;;                                         ; time specific items are already sorted first by org-agenda-sorting-strategy

;;                                         ; non-deadline and non-scheduled items next
;;      ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

;;                                         ; deadlines for today next
;;      ((bh/agenda-sort-test 'bh/is-due-deadline a b))

;;                                         ; late deadlines next
;;      ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

;;                                         ; scheduled items for today next
;;      ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

;;                                         ; late scheduled items next
;;      ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

;;                                         ; pending deadlines last
;;      ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

;;                                         ; finally default to unsorted
;;      (t (setq result nil)))
;;     result))

;; (defmacro bh/agenda-sort-test (fn a b)
;;   "Test for agenda sort"
;;   `(cond
;;                                         ; if both match leave them unsorted
;;     ((and (apply ,fn (list ,a))
;;           (apply ,fn (list ,b)))
;;      (setq result nil))
;;                                         ; if a matches put a first
;;     ((apply ,fn (list ,a))
;;      (setq result -1))
;;                                         ; otherwise if b matches put b first
;;     ((apply ,fn (list ,b))
;;      (setq result 1))
;;                                         ; if none match leave them unsorted
;;     (t nil)))

;; (defmacro bh/agenda-sort-test-num (fn compfn a b)
;;   `(cond
;;     ((apply ,fn (list ,a))
;;      (setq num-a (string-to-number (match-string 1 ,a)))
;;      (if (apply ,fn (list ,b))
;;          (progn
;;            (setq num-b (string-to-number (match-string 1 ,b)))
;;            (setq result (if (apply ,compfn (list num-a num-b))
;;                             -1
;;                           1)))
;;        (setq result -1)))
;;     ((apply ,fn (list ,b))
;;      (setq result 1))
;;     (t nil)))

;; (defun bh/is-not-scheduled-or-deadline (date-str)
;;   (and (not (bh/is-deadline date-str))
;;        (not (bh/is-scheduled date-str))))

;; (defun bh/is-due-deadline (date-str)
;;   (string-match "Deadline:" date-str))

;; (defun bh/is-late-deadline (date-str)
;;   (string-match "In *\\(-.*\\)d\.:" date-str))

;; (defun bh/is-pending-deadline (date-str)
;;   (string-match "In \\([^-]*\\)d\.:" date-str))

;; (defun bh/is-deadline (date-str)
;;   (or (bh/is-due-deadline date-str)
;;       (bh/is-late-deadline date-str)
;;       (bh/is-pending-deadline date-str)))

;; (defun bh/is-scheduled (date-str)
;;   (or (bh/is-scheduled-today date-str)
;;       (bh/is-scheduled-late date-str)))

;; (defun bh/is-scheduled-today (date-str)
;;   (string-match "Scheduled:" date-str))

;; (defun bh/is-scheduled-late (date-str)
;;   (string-match "Sched\.\\(.*\\)x:" date-str))


;; (defun bh/mark-next-parent-tasks-todo ()
;;   "Visit each parent task and change NEXT states to TODO"
;;   (let ((mystate (or (and (fboundp 'state)
;;                           state)
;;                      (nth 2 (org-heading-components)))))
;;     (when (equal mystate "NEXT")
;;       (save-excursion
;;         (while (org-up-heading-safe)
;;           (when (member (nth 2 (org-heading-components)) (list "NEXT"))
;;             (org-todo "TODO")))))))

;; (add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
;; (add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)
;; ;; Remove empty LOGBOOK drawers on clock out
;; (defun bh/remove-empty-drawer-on-clock-out ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line 0)
;;     (org-remove-empty-drawer-at "LOGBOOK" (point))))

;; (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)



;; ;; Change tasks to NEXT when clocking in
;; (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)



;; (setq bh/keep-clock-running nil)

;; (defun bh/clock-in-to-next (kw)
;;   "Switch a task from TODO to NEXT when clocking in.
;; Skips capture tasks, projects, and subprojects.
;; Switch projects and subprojects from NEXT back to TODO"
;;   (when (not (and (boundp 'org-capture-mode) org-capture-mode))
;;     (cond
;;      ((and (member (org-get-todo-state) (list "TODO"))
;;            (bh/is-task-p))
;;       "NEXT")
;;      ((and (member (org-get-todo-state) (list "NEXT"))
;;            (bh/is-project-p))
;;       "TODO"))))

;; (defun bh/find-project-task ()
;;   "Move point to the parent (project) task if any"
;;   (save-restriction
;;     (widen)
;;     (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
;;       (while (org-up-heading-safe)
;;         (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
;;           (setq parent-task (point))))
;;       (goto-char parent-task)
;;       parent-task)))

;; (defun bh/punch-in (arg)
;;   "Start continuous clocking and set the default task to the
;; selected task.  If no task is selected set the Organization task
;; as the default task."
;;   (interactive "p")
;;   (setq bh/keep-clock-running t)
;;   (if (equal major-mode 'org-agenda-mode)
;;       ;;
;;       ;; We're in the agenda
;;       ;;
;;       (let* ((marker (org-get-at-bol 'org-hd-marker))
;;              (tags (org-with-point-at marker (org-get-tags-at))))
;;         (if (and (eq arg 4) tags)
;;             (org-agenda-clock-in '(16))
;;           (bh/clock-in-organization-task-as-default)))
;;     ;;
;;     ;; We are not in the agenda
;;     ;;
;;     (save-restriction
;;       (widen)
;;                                         ; Find the tags on the current task
;;       (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
;;           (org-clock-in '(16))
;;         (bh/clock-in-organization-task-as-default)))))

;; (defun bh/punch-out ()
;;   (interactive)
;;   (setq bh/keep-clock-running nil)
;;   (when (org-clock-is-active)
;;     (org-clock-out))
;;   (org-agenda-remove-restriction-lock))

;; (defun bh/clock-in-default-task ()
;;   (save-excursion
;;     (org-with-point-at org-clock-default-task
;;                        (org-clock-in))))

;; (defun bh/clock-in-parent-task ()
;;   "Move point to the parent (project) task if any and clock in"
;;   (let ((parent-task))
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (while (and (not parent-task) (org-up-heading-safe))
;;           (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
;;             (setq parent-task (point))))
;;         (if parent-task
;;             (org-with-point-at parent-task
;;                                (org-clock-in))
;;           (when bh/keep-clock-running
;;             (bh/clock-in-default-task)))))))

;; (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

;; (defun bh/clock-in-organization-task-as-default ()
;;   (interactive)
;;   (org-with-point-at (org-id-find bh/organization-task-id 'marker)
;;                      (org-clock-in '(16))))

;; (defun bh/clock-out-maybe ()
;;   (when (and bh/keep-clock-running
;;              (not org-clock-clocking-in)
;;              (marker-buffer org-clock-default-task)
;;              (not org-clock-resolving-clocks-due-to-idleness))
;;     (bh/clock-in-parent-task)))

;; (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; (require 'org-id)
;; (defun bh/clock-in-task-by-id (id)
;;   "Clock in a task by id"
;;   (org-with-point-at (org-id-find id 'marker)
;;                      (org-clock-in nil)))

;; (defun bh/clock-in-last-task (arg)
;;   "Clock in the interrupted task if there is one
;; Skip the default task and get the next one.
;; A prefix arg forces clock in of the default task."
;;   (interactive "p")
;;   (let ((clock-in-to-task
;;          (cond
;;           ((eq arg 4) org-clock-default-task)
;;           ((and (org-clock-is-active)
;;                 (equal org-clock-default-task (cadr org-clock-history)))
;;            (caddr org-clock-history))
;;           ((org-clock-is-active) (cadr org-clock-history))
;;           ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
;;           (t (car org-clock-history)))))
;;     (org-with-point-at clock-in-to-task
;;                        (org-clock-in nil))))


;; (defun bh/clock-in-bzflagt-task ()
;;   (interactive)
;;   (bh/clock-in-task-by-id "dcf55180-2a18-460e-8abb-a9f02f0893be"))

;; (defun bh/resume-clock ()
;;   (interactive)
;;   (if (marker-buffer org-clock-interrupted-task)
;;       (org-with-point-at org-clock-interrupted-task
;;                          (org-clock-in))
;;     (org-clock-out)))





;;                                         ; Erase all reminders and rebuilt reminders for today from the agenda
;; (defun bh/org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))

;;                                         ; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

;;                                         ; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (bh/org-agenda-to-appt)

;;                                         ; Activate appointments so we get notifications
;; (appt-activate t)

;;                                         ; If we leave Emacs running overnight - reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; (global-set-key (kbd "<f5>") 'bh/org-todo)

;; (defun bh/org-todo (arg)
;;   (interactive "p")
;;   (if (equal arg 4)
;;       (save-restriction
;;         (bh/narrow-to-org-subtree)
;;         (org-show-todo-tree nil))
;;     (bh/narrow-to-org-subtree)
;;     (org-show-todo-tree nil)))

;; (global-set-key (kbd "<S-f5>") 'bh/widen)

;; (defun bh/widen ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (org-agenda-remove-restriction-lock)
;;     (widen)
;;     (org-agenda-remove-restriction-lock)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "W" 'bh/widen))
;;           'append)

;; (defun bh/restrict-to-file-or-follow (arg)
;;   "Set agenda restriction to 'file or with argument invoke follow mode.
;; I don't use follow mode very often but I restrict to file all the time
;; so change the default 'F' binding in the agenda to allow both"
;;   (interactive "p")
;;   (if (equal arg 4)
;;       (org-agenda-follow-mode)
;;     (if (equal major-mode 'org-agenda-mode)
;;         (bh/set-agenda-restriction-lock 4)
;;       (widen))))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
;;           'append)

;; (defun bh/narrow-to-org-subtree ()
;;   (widen)
;;   (org-narrow-to-subtree)
;;   (save-restriction
;;     (org-agenda-set-restriction-lock)))

;; (defun bh/narrow-to-subtree ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (org-with-point-at (org-get-at-bol 'org-hd-marker)
;;                          (bh/narrow-to-org-subtree))
;;     (bh/narrow-to-org-subtree)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
;;           'append)

;; (defun bh/narrow-up-one-org-level ()
;;   (widen)
;;   (save-excursion
;;     (outline-up-heading 1 'invisible-ok)
;;     (bh/narrow-to-org-subtree)))

;; (defun bh/get-pom-from-agenda-restriction-or-point ()
;;   (or (org-get-at-bol 'org-hd-marker)
;;       (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
;;       (and (equal major-mode 'org-mode) (point))
;;       org-clock-marker))

;; (defun bh/narrow-up-one-level ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
;;                          (bh/narrow-up-one-org-level))
;;     (bh/narrow-up-one-org-level)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
;;           'append)

;; (defun bh/narrow-to-org-project ()
;;   (widen)
;;   (save-excursion
;;     (bh/find-project-task)
;;     (bh/narrow-to-org-subtree)))

;; (defun bh/narrow-to-project ()
;;   (interactive)
;;   (if (equal major-mode 'org-agenda-mode)
;;       (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
;;                          (bh/narrow-to-org-project))
;;     (bh/narrow-to-org-project)))

;; (add-hook 'org-agenda-mode-hook
;;           '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
;;           'append)

;; (defvar bh/current-view-project nil)

;; (defun bh/view-next-project ()
;;   (interactive)
;;   (unless (marker-position org-agenda-restrict-begin)
;;     (goto-char (point-min))
;;     (setq bh/current-view-project (point)))
;;   (bh/widen)
;;   (goto-char bh/current-view-project)
;;   (forward-visible-line 1)
;;   (while (and (< (point) (point-max))
;;               (or (not (org-get-at-bol 'org-hd-marker))
;;                   (org-with-point-at (org-get-at-bol 'org-hd-marker)
;;                                      (or (not (bh/is-project-p))
;;                                          (bh/is-project-subtree-p)))))
;;     (forward-visible-line 1))
;;   (setq bh/current-view-project (point))
;;   (if (org-get-at-bol 'org-hd-marker)
;;       (bh/narrow-to-project)
;;     (message "All projects viewed.")
;;     (ding)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

;; (defun bh/show-org-agenda ()
;;   (interactive)
;;   (switch-to-buffer "*Org Agenda*")
;;   (delete-other-windows))

;; (setq org-stuck-projects (quote ("" nil nil "")))

;; (defun bh/is-project-p ()
;;   "Any task with a todo keyword subtask"
;;   (save-restriction
;;     (widen)
;;     (let ((has-subtask)
;;           (subtree-end (save-excursion (org-end-of-subtree t)))
;;           (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
;;       (save-excursion
;;         (forward-line 1)
;;         (while (and (not has-subtask)
;;                     (< (point) subtree-end)
;;                     (re-search-forward "^\*+ " subtree-end t))
;;           (when (member (org-get-todo-state) org-todo-keywords-1)
;;             (setq has-subtask t))))
;;       (and is-a-task has-subtask))))

;; (defun bh/is-project-subtree-p ()
;;   "Any task with a todo keyword that is in a project subtree.
;; Callers of this function already widen the buffer view."
;;   (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
;;                               (point))))
;;     (save-excursion
;;       (bh/find-project-task)
;;       (if (equal (point) task)
;;           nil
;;         t))))

;; (defun bh/is-task-p ()
;;   "Any task with a todo keyword and no subtask"
;;   (save-restriction
;;     (widen)
;;     (let ((has-subtask)
;;           (subtree-end (save-excursion (org-end-of-subtree t)))
;;           (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
;;       (save-excursion
;;         (forward-line 1)
;;         (while (and (not has-subtask)
;;                     (< (point) subtree-end)
;;                     (re-search-forward "^\*+ " subtree-end t))
;;           (when (member (org-get-todo-state) org-todo-keywords-1)
;;             (setq has-subtask t))))
;;       (and is-a-task (not has-subtask)))))

;; (defun bh/is-subproject-p ()
;;   "Any task which is a subtask of another project"
;;   (let ((is-subproject)
;;         (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
;;     (save-excursion
;;       (while (and (not is-subproject) (org-up-heading-safe))
;;         (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
;;           (setq is-subproject t))))
;;     (and is-a-task is-subproject)))

;; (defun bh/list-sublevels-for-projects-indented ()
;;   "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
;;   This is normally used by skipping functions where this variable is already local to the agenda."
;;   (if (marker-buffer org-agenda-restrict-begin)
;;       (setq org-tags-match-list-sublevels 'indented)
;;     (setq org-tags-match-list-sublevels nil))
;;   nil)

;; (defun bh/list-sublevels-for-projects ()
;;   "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
;;   This is normally used by skipping functions where this variable is already local to the agenda."
;;   (if (marker-buffer org-agenda-restrict-begin)
;;       (setq org-tags-match-list-sublevels t)
;;     (setq org-tags-match-list-sublevels nil))
;;   nil)

;; (defun bh/skip-stuck-projects ()
;;   "Skip trees that are not stuck projects"
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       (if (bh/is-project-p)
;;           (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;                  (has-next ))
;;             (save-excursion
;;               (forward-line 1)
;;               (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
;;                 (unless (member "WAITING" (org-get-tags-at))
;;                   (setq has-next t))))
;;             (if has-next
;;                 nil
;;               next-headline)) ; a stuck project, has subtasks but no next task
;;         nil))))

;; (defun bh/skip-non-stuck-projects ()
;;   "Skip trees that are not stuck projects"
;;   (bh/list-sublevels-for-projects-indented)
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       (if (bh/is-project-p)
;;           (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;                  (has-next ))
;;             (save-excursion
;;               (forward-line 1)
;;               (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
;;                 (unless (member "WAITING" (org-get-tags-at))
;;                   (setq has-next t))))
;;             (if has-next
;;                 next-headline
;;               nil)) ; a stuck project, has subtasks but no next task
;;         next-headline))))

;; (defun bh/skip-non-projects ()
;;   "Skip trees that are not projects"
;;   (bh/list-sublevels-for-projects-indented)
;;   (if (save-excursion (bh/skip-non-stuck-projects))
;;       (save-restriction
;;         (widen)
;;         (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;           (cond
;;            ((and (bh/is-project-p)
;;                  (marker-buffer org-agenda-restrict-begin))
;;             nil)
;;            ((and (bh/is-project-p)
;;                  (not (marker-buffer org-agenda-restrict-begin))
;;                  (not (bh/is-project-subtree-p)))
;;             nil)
;;            (t
;;             subtree-end))))
;;     (save-excursion (org-end-of-subtree t))))

;; (defun bh/skip-project-trees-and-habits ()
;;   "Skip trees that are projects"
;;   (save-restriction
;;     (widen)
;;     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;       (cond
;;        ((bh/is-project-p)
;;         subtree-end)
;;        ((org-is-habit-p)
;;         subtree-end)
;;        (t
;;         nil)))))

;; (defun bh/skip-projects-and-habits-and-single-tasks ()
;;   "Skip trees that are projects, tasks that are habits, single non-project tasks"
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       (cond
;;        ((org-is-habit-p)
;;         next-headline)
;;        ((bh/is-project-p)
;;         next-headline)
;;        ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
;;         next-headline)
;;        (t
;;         nil)))))

;; (defun bh/skip-project-tasks-maybe ()
;;   "Show tasks related to the current restriction.
;; When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
;; When not restricted, skip project and sub-project tasks, habits, and project related tasks."
;;   (save-restriction
;;     (widen)
;;     (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;            (next-headline (save-excursion (or (outline-next-heading) (point-max))))
;;            (limit-to-project (marker-buffer org-agenda-restrict-begin)))
;;       (cond
;;        ((bh/is-project-p)
;;         next-headline)
;;        ((org-is-habit-p)
;;         subtree-end)
;;        ((and (not limit-to-project)
;;              (bh/is-project-subtree-p))
;;         subtree-end)
;;        ((and limit-to-project
;;              (bh/is-project-subtree-p)
;;              (member (org-get-todo-state) (list "NEXT")))
;;         subtree-end)
;;        (t
;;         nil)))))

;; (defun bh/skip-projects-and-habits ()
;;   "Skip trees that are projects and tasks that are habits"
;;   (save-restriction
;;     (widen)
;;     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;       (cond
;;        ((bh/is-project-p)
;;         subtree-end)
;;        ((org-is-habit-p)
;;         subtree-end)
;;        (t
;;         nil)))))

;; (defun bh/skip-non-subprojects ()
;;   "Skip trees that are not projects"
;;   (let ((next-headline (save-excursion (outline-next-heading))))
;;     (if (bh/is-subproject-p)
;;         nil
;;       next-headline)))


;; (defun bh/skip-non-archivable-tasks ()
;;   "Skip trees that are not available for archiving"
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       ;; Consider only tasks with done todo headings as archivable candidates
;;       (if (member (org-get-todo-state) org-done-keywords)
;;           (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;                  (daynr (string-to-int (format-time-string "%d" (current-time))))
;;                  (a-month-ago (* 60 60 24 (+ daynr 1)))
;;                  (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
;;                  (this-month (format-time-string "%Y-%m-" (current-time)))
;;                  (subtree-is-current (save-excursion
;;                                        (forward-line 1)
;;                                        (and (< (point) subtree-end)
;;                                             (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
;;             (if subtree-is-current
;;                 next-headline ; Has a date in this month or last month, skip it
;;               nil))  ; available to archive
;;         (or next-headline (point-max))))))

;; (defun bh/toggle-truncate-lines ()
;;   "Toggle setting truncate-lines between t and nil"
;;   (interactive)
;;   (setq truncate-lines (not truncate-lines))
;;   (redraw-display))



;; (defun bh/display-inline-images ()
;;   (condition-case nil
;;       (org-display-inline-images)
;;     (error nil)))

;; (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)






;; ;;  (setq org-latex-default-packages-alist
;; ;;        '("\\usepackage[poly,french]{fpsupelec}\\tolerance=1000"))
;; (setq org-latex-default-packages-alist
;;       '( ; ("AUTO" "inputenc" t)
;;                                         ; ("T1" "fontenc" t)
;;         ("" "fixltx2e" nil)
;;         ("" "graphicx" t)
;;         ("" "longtable" nil)
;;         ("" "float" nil)
;;         ("" "wrapfig" nil)
;;         ("normalem" "ulem" t)
;;         ("" "textcomp" t)
;;         ("" "marvosym" t)
;;         ("" "wasysym" t)
;;         ("" "latexsym" t)
;;         ("" "amssymb" t)
;;         ("" "amstext" nil)
;;         ("" "hyperref" nil)
;;         "\\tolerance=1000"))
;; ;; Better hit the right web site ... or use a local version
;; (setq org-export-html-mathjax-options
;;       (quote ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
;;               (scale "100") (align "center") (indent "2em") (mathml nil))))

;; (setq org-reveal-root "c:/Source/Javascript/reveal.js")

;; (setq org-deck-directories '("c:/source/Javascript/deck.js.twitwi"))

;; ;; Do not insert default beamer theme
;; (setq org-beamer-theme nil)



;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.mm\\'" . system)
;;         ("\\.x?html?\\'" . system)
;;         ("\\.pdf\\'" . "SumatraPDF -reuse-instance %s"))
;;       )





;; (setq org-publish-project-alist
;;                                         ;
;;                                         ; http://www.norang.ca/  (norang website)
;;                                         ; norang-org are the org-files that generate the content
;;                                         ; norang-extra are images and css files that need to be included
;;                                         ; norang is the top-level project that gets published
;;       (quote
;;        (
;;         ;; ("fisda"
;;         ;;  :base-directory "~/Cours/FISDA/org"
;;         ;;  :base-extension "org"
;;         ;;  :publishing-directory "~/cours/FISDA/poly"
;;         ;;  :publishing-function org-latex-publish-to-latex
;;         ;;  :select-tags     ("@POLY")
;;         ;;  :title "FCS-DSA Course Notes"
;;         ;;  :include ("index.org")
;;         ;;  :exclude "\\.org$"
;;         ;;  )
;;         ("Web-inherit"
;;          :base-directory "~/Web/"
;;          :recursive t
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif"
;;          :publishing-directory "c:/Web/"
;;          :publishing-function org-publish-attachment
;;          )

;;         ("FCS-DSA-org"
;;          :base-directory "~/Cours/FCS-DSA/Org/web"
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Index"
;;          :auto-sitemap t                ; Generate sitemap.org automagically...
;;          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;;          :recursive t
;;          :base-extension "org"
;;          :body-only t
;;          :publishing-directory "c:/Web/FCS-DSA/"
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 3
;;          :htmlized-source t
;;          :auto-preamble nil
;;          :auto-postamble nil
;;          :style-include-default nil
;;          :style-include-scripts nil
;;          :LaTeX-fragments nil
;;          :html-preamble publish-preamble
;;          :html-postamble publish-postamble
;;          :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
;;          )
;;         ("CQP-org"
;;          :base-directory "~/Cours/CQP-ArTech/org"
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Index"
;;          :auto-sitemap t                ; Generate sitemap.org automagically...
;;          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;;          :recursive t
;;          :base-extension "org"
;;          :body-only t
;;          :publishing-directory "c:/Web/CQP/"
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 3
;;          :htmlized-source t
;;          :auto-preamble nil
;;          :auto-postamble nil
;;          :style-include-default nil
;;          :style-include-scripts nil
;;          :LaTeX-fragments nil
;;          :html-preamble nil
;;          :html-postamble nil
;;          :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
;;          )
;;         ("Min-IA-org"
;;          :base-directory "~/Cours/Min-IA/Org/web"
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Index"
;;          :auto-sitemap t                ; Generate sitemap.org automagically...
;;          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;;          :recursive t
;;          :base-extension "org"
;;          :body-only t
;;          :publishing-directory "c:/Web/Min-IA/"
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 3
;;          :htmlized-source t
;;          :auto-preamble nil
;;          :auto-postamble nil
;;          :style-include-default nil
;;          :style-include-scripts nil
;;          :LaTeX-fragments nil
;;          :html-preamble nil
;;          :html-postamble nil
;;          :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
;;          )
;;         ("Maj-ModCal-org"
;;          :base-directory "~/Cours/Maj-ModCal/Org"
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Index"
;;          :auto-sitemap t                ; Generate sitemap.org automagically...
;;          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;;          :recursive t
;;          :base-extension "org"
;;          :body-only t
;;          :publishing-directory "c:/Web/Maj-ModCal/"
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 3
;;          :htmlized-source t
;;          :auto-preamble nil
;;          :auto-postamble nil
;;          :style-include-default nil
;;          :style-include-scripts nil
;;          :LaTeX-fragments nil
;;          :html-preamble nil
;;          :html-postamble nil
;;          :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
;;          )
;;         ("FCS-DSA-static"
;;          :base-directory "~/Cours/FCS-DSA/Org/"
;;          :recursive t
;;          :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
;;          :publishing-directory "c:/Web/FCS-DSA/"
;;          :publishing-function org-publish-attachment
;;          :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
;;          )
;;         ("CQP-static"
;;          :base-directory "~/Cours/CQP-ArTech/Org/"
;;          :recursive t
;;          :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
;;          :publishing-directory "c:/Web/CQP/"
;;          :publishing-function org-publish-attachment
;;          :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
;;          )
;;         ("Min-IA-static"
;;          :base-directory "~/Cours/Min-IA/Org/"
;;          :recursive t
;;          :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
;;          :publishing-directory "c:/Web/Min-IA/"
;;          :publishing-function org-publish-attachment
;;          :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
;;          )
;;         ("Maj-ModCal-static"
;;          :base-directory "~/Cours/Maj-ModCal/Org/"
;;          :recursive t
;;          :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
;;          :publishing-directory "c:/Web/Maj-ModCal/"
;;          :publishing-function org-publish-attachment
;;          :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
;;          )

;;         ("FCS-DSA-site" :components ("Web-inherit" "FCS-DSA-org" "FCS-DSA-static"))
;;         ("CQP-site" :components ("Web-inherit" "CQP-org" "CQP-static"))
;;         ("Min-IA-site" :components ("Web-inherit" "Min-IA-org" "Min-IA-static"))
;;         ("Maj-ModCal-site" :components ("Web-inherit" "Maj-ModCal-org" "Maj-ModCal-static"))
;;         ("Personal-site" :components ("Web-inherit" "Personal-org" "Personal-static"))
;;         ("Personal-org"
;;                                         ; :select-tags     ("@WEB")
;;          :title "Fabrice Popineau Personal Web Page"
;;                                         ; :include ("index.org")
;;                                         ; :exclude "\\.org$"
;;          :base-directory "~/Web/Org/"
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Index"
;;          :auto-sitemap t                ; Generate sitemap.org automagically...
;;          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;;          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;;          :recursive t
;;          :base-extension "org"
;;          :body-only t
;;          :publishing-directory "c:/Web/Personal"
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 2
;;          :htmlized-source t
;;          :auto-preamble nil
;;          :auto-postamble nil
;;          :style-include-default nil
;;          :style-include-scripts nil
;;          :LaTeX-fragments nil
;;          :html-preamble publish-preamble
;;          :html-postamble publish-postamble
;;          :exclude "/attic/\\|.metadata$\\|/exam[^/]*/\\|/team/\\|^#.*#$\\|/slides/"
;;          )
;;         ("Personal-static"
;;          :base-directory "~/Web/Org/"
;;          :recursive t
;;          :base-extension "css\\|js\\|html\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|java\\|cs\\|ml\\|fs\\|py\\|grf\\|txt"
;;          :publishing-directory "c:/Web/Personal/"
;;          :publishing-function org-publish-attachment
;;          :exclude "/attic/\\|.metadata$\\|/poly/\\|/slides/\\|/web/\\|/exam[^/]*/"
;;          )
;;         )))

;; (defun my-src-block-filter (text backend props)
;;   (when (eq backend 'e-html)
;;     (when (string-match "<pre class=\\\"src src-\\([a-z]*\\)\\\">" text)
;;       (setq text
;;             (replace-match (format "<script type=\"syntaxhighlighter\" class=\"src src-%s brush: %s\"><![CDATA[" (match-string 1 text) (match-string 1 text)) nil nil text)))
;;     (when (string-match "</pre>" text)
;;       (setq text (replace-match "]]></script>" nil nil text)))
;;     ;; (when (string-match "#\\+END_SRC.*$" text)
;;     ;;   (setq text (replace-match "" nil nil text)))
;;     ;; (when (string-match "#\\+BEGIN_SRC.*" text)
;;     ;;   (setq text (replace-match "" nil nil text)))
;;     text))

;; ;; (add-to-list 'org-export-filter-src-block-functions #'my-src-block-filter)
;; (setq org-export-filter-src-block-functions nil)





;; * HTML exporter

;; * Publishing

;; * Outshine mode

(require 'outshine)
(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'lisp-mode-hook 'outline-minor-mode)
;; (add-hook 'clojure-mode-hook 'outline-minor-mode)
;; (add-hook 'ess-mode-hook 'outline-minor-mode)
;; (add-hook 'ledger-mode-hook 'outline-minor-mode)
(add-hook 'message-mode-hook 'outline-minor-mode)
(defvar outline-minor-mode-prefix "\M-#")
(setq outshine-use-speed-commands t)


;;; * Fix outshine version outshine-20141221.1805

(defun outline-hide-sublevels (keep-levels)
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive "p")
  (if (< keep-levels 1)
      (error "Must keep at least one level of headers"))
  (setq keep-levels (1- keep-levels))
  (save-excursion
    (goto-char (point-min))
    ;; Skip the prelude, if any.
    (unless (outline-on-heading-p t) (outline-next-heading))
    (hide-subtree)
    (show-children keep-levels)
    (condition-case err
        (while (outline-get-next-sibling)
          (hide-subtree)
          (show-children keep-levels))
      (error nil))))


;;; * Conclusion
(provide 'fp-org)
;;; fp-org ends here
