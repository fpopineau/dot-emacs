;;; fp-org.el --- Emacs Prelude: Fabrice Popineau Org mode configuration.
;;
;; Copyright © 2014 Fabrice Popineau
;;
;; Author: Fabrice Popineau <fabrice.popineau@gmail.com>
;; URL: https://github.com/fpopineau/.emacs.d
;; Heavily based on http://doc.norang.ca/org-mode.html
;; Should also take ideas from http://pages.sachachua.com/.emacs.d/Sacha.html
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

;; * General

(setq org-modules
      '(org-annotate-file
                                        ; org-bbdb
        org-bibtex
        ;; org-bibtex-extras
        org-checklist
        org-contacts
                                        ; org-crypt
                                        ; org-docview
        org-drill
        org-eval
        org-expiry
                                        ; org-gnus
                                        ; org-favtable
                                        ; org-gnus
        org-habit
        org-id
        org-info
        org-inlinetask
        org-interactive-query
                                        ; org-irc
                                        ; org-jsinfo
        org-learn
                                        ; org-man
                                        ; org-mew
        org-mime
                                        ; org-mhe
        org-mouse
        org-panel
        org-protocol
                                        ; org-rmail
        org-screen
                                        ; org-taskjuggler
        org-timer
        org-toc
                                        ; org-vm
                                        ; org-w3m
        org-wl
        ob-ipython
        ox
        ox-reveal
        ox-latex
        ox-beamer
        ox-html
        ox-koma-letter
        ox-reveal
        ox-publish
        ))

(require 'htmlize)
                                        ; (require 'org-install)
(require 'org)
(org-load-modules-maybe t)

;; Beware: depends on Org!
(require 'org-google)


;; * Org initialization

(setq org-directory "~/Org/")

;; Hook to update all blocks before saving
(add-hook 'org-mode-hook
          (lambda() (add-hook 'before-save-hook
                              'org-update-all-dblocks t t)))

(add-hook 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

;; Some of them are too large
;; (add-hook 'org-mode-hook 'org-display-inline-images)

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

(setq org-startup-indented t)

;; Costly!
;; (setq org-use-property-inheritance t)

;; ** footnotes
(setq org-footnote-auto-label 'confirm)
(setq org-footnote-auto-adjust t)
(setq org-footnote-define-inline nil)

;; To make org show leading stars use
;; (setq org-hide-leading-stars nil)

;; This removes the indentation in the org-file but displays it as if it
;; was indented while you are working on the org file buffer.
(setq org-startup-indented t)

(setq org-hide-block-startup t)

;; The following setting hides blank lines between headings which keeps folded view nice and compact.
(setq org-cycle-separator-lines 0)

;; The following setting prevents creating blank lines before headings
;; but allows list items to adapt to existing blank lines around the
;; items:
(setq org-blank-before-new-entry '((heading)
                                   (plain-list-item . auto)))

;; To create new headings in a project file it is really convenient to
;; use C-RET, C-S-RET, M-RET, and M-S-RET. This inserts a new headline
;; possibly with a TODO keyword. With the following setting
(setq org-insert-heading-respect-content nil)
;; org inserts the heading at point for the M- versions and respects
;; content for the C- versions. The respect content setting is
;; temporarily turned on for the C- versions which adds the new
;; heading after the content of the current item. This lets you hit
;; C-S-RET in the middle of an entry and the new heading is added
;; after the body of the current entry but still allow you to split an
;; entry in the middle with M-S-RET.

;; Notes at the top
(setq org-reverse-note-order nil)

(setq org-show-context-detail '((isearch . lineage)
                                (bookmark-jump . lineage)
                                (default . canonical)))

;; ** Attachments
(setq org-id-method 'uuidgen)

;; ** Open files externally

(setq org-file-apps
      '(("\\.mm\\'" . emacs)
        ("\.pdf::\(\d+\)\'" . "c:/Local/SumatraPDF/SumatraPDF -reuse-instance -page %1 %s")
        ("\\.pdf\\'" . "c:/Local/SumatraPDF/SumatraPDF -reuse-instance %s")
        (auto-mode . emacs)
        (remote . emacs)
        (t w32-shell-execute "open" file)
        (system w32-shell-execute "open" file)))

;; * org-id
(setq org-id-locations-file
      (expand-file-name "org-id-locations" prelude-savefile-dir))

;; ** Exporting tables
(setq org-table-export-default-format "orgtbl-to-csv")

;; ** Insert inactive timestamps and exclude from export
(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)

;; To prevent the timestamps from being exported in documents I use the following setting
(setq org-export-with-timestamps nil)

;; * Keys & UI

; (require 'org-cua-dwim)
; (org-cua-dwim-activate)

(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-support-shift-select t)

(setq org-use-speed-commands t)
;; (setq org-use-speed-commands
;;       (lambda nil
;;         (and (looking-at org-outline-regexp-bol)
;;              (not (org-in-src-block-p t)))))

(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))

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
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))
(setq org-fast-tag-selection-single-key 'expert)

;; Disable keys in org-mode
;;    C-c [
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             ; (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cL" 'org-occur-link-in-agenda-files)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)

;; And allow ace-jump-mode within my org files
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)

(setq org-completion-use-ido t)

;; The following setting make RET insert a new line instead of opening
;; links. This setting is a love-hate relationship for me. When it
;; first came out I immediately turned it off because I wanted to
;; insert new lines in front of my links and RET would open the link
;; instead which at the time I found extremely annoying. Since then
;; I've retrained my fingers to hit RET at the end of the previous
;; line.
(setq org-return-follows-link t)

;; Needed by org-goto so that motion keys work.
(setq org-goto-auto-isearch nil)

(setq org-loop-over-headlines-in-active-region t)

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)


;; * Display

(setq org-pretty-entities t)

(setq org-highlight-latex-and-related '(latex))

;; Don't remove the highlighting after an occur search (C-c / /)
;; (setq org-remove-highlights-with-change nil)

;; Not mandatory
; (setq org-fontify-done-headline t)

(setq org-fontify-emphasized-text t)
;; Only needed if setting a background face for headlines
; (setq org-fontify-whole-heading-line t)
(setq org-hide-emphasis-markers nil)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

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

;; Highlight clock when running overtime
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

;; ** Minimize Emacs frames
(setq org-link-frame-setup '((vm . vm-visit-folder)
                             (gnus . org-gnus-no-new-news)
                             (file . find-file)))

(setq org-link-mailto-program '(browse-url-mail "mailto:%a?subject=%s"))

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

;; Allow alphabetic lists to be recognized for lists
(setq org-alphabetical-lists t)

(setq org-allow-promoting-top-level-subtree t)


;; * TODO Keywords

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "REVIEW(r)" "DELVE(d)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("REVIEW" :foreground "gold" :weight bold)
              ("DELVE" :foreground "DarkOrchid" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "NavajoWhite1" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
;; Fast todo selection allows changing from any task todo state to any
;; other state directly by selecting the appropriate key from the fast
;; todo selection key menu.
(setq org-use-fast-todo-selection t)

;; The setting allows changing todo states with S-left and
;; S-right skipping all of the normal processing when entering or
;; leaving a todo state. This cycles through the todo states but
;; skips setting timestamps and entering notes which is very
;; convenient when all you want to do is fix up the status of an
;; entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
;; * Capture
(setq org-default-notes-file "~/Org/notes.org")

(setq org-capture-templates-contexts nil)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Org/notes.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Org/notes.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/Org/notes.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Org/Diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Org/notes.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ;; FIXME: shouldn't meeting be appointments? -> Diary.org?
              ("m" "Meeting" entry (file "~/Org/notes.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Org/notes.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Org/notes.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
              ;; ("J" "Journal Entry" plain
              ;;  (file+datetree "~/Org/Diary.org")
              ;;  "%U\n\n%?" :empty-lines-before 1)
              ("W" "Log Work Task" entry
               (file+datetree "~/Org/Worklog.org")
               "* TODO %^{Description}  %^g\n%?\n\nAdded: %U"
               :clock-in t
               :clock-keep t)
              )))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; * Clocking

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

(setq org-clock-idle-time 15)


;; * Calendar

;; In case it might be needed someday

(setq org-icalendar-include-todo 'all)
(setq org-icalendar-combined-name "Fabrice Popineau ORG")
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "Europe/Paris")
(setq org-icalendar-store-UID t)


;; * GCal

(setq google/oauth-client-id "306710974378-6ckee8kop51cj49ih9utbngucsq5bbq3.apps.googleusercontent.com")
(setq google/oauth-client-id "306710974378-6l4hcnvjc11l1eth827ss7ctas6u325e.apps.googleusercontent.com")
(setq google/oauth-client-secret "56IAVEQFYqV7LTar6bf36pKI")
(setq google/oauth-client-secret "g-tEm_Hd9in7fsZXoyE9-iN8")
(setq google/expire-time 5)

(setq google-calendar/down-days 120)
(setq google-calendar/up-days 120)
(setq google-calendar/calendar-files (concat prelude-savefile-dir "/google-calendars.org"))

(setq google-calendar/diary-file (expand-file-name "~/Org/Diary.org"))
(setq google-calendar/tag-associations '(("fabrice.popineau@gmail.com" "CS")
                                         ("rknat5om4veh52cm6i60avu2b8@group.calendar.google.com" "perso")))
(setq google-calendar/auto-archive nil)



;; * Agenda

(setq org-agenda-include-diary nil)
;; (setq org-agenda-insert-diary-extract-time t)
(setq org-agenda-diary-file "~/Org/Diary.org")

(setq org-agenda-start-on-weekday 1)

(setq org-agenda-span 'day)

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-bulk-mark-char "*")

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; (setq org-agenda-dim-blocked-tasks t)

;; (setq org-agenda-entry-text-maxlines 10)
;; (setq org-agenda-file-regexp "\\.org\\'")
(setq org-agenda-files
      `("~/Org/"
                                        ; ,@(mapcan #'(lambda (dir)
                                        ;               (remove-if #'(lambda (f) (or (not (file-directory-p f))
                                        ;                                            (string-match "\\.\\.?$" f)))
                                        ;                          (directory-files dir t))) '("~/Projets/" "~/Cours/"))
	))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; ;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Sorting order for tasks on the agenda
;; (setq org-agenda-sorting-strategy
;;       (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
;;               (todo category-up effort-up)
;;               (tags category-up effort-up)
;;               (search category-up))))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up category-keep)
        (todo category-up time-up)
        (tags category-up time-up)
        (search time-up)))

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-14t%s")
        (timeline . "  % s")
        (todo . " %i %-14:c")
        (tags . " %i %-14:c")
        (search . " %i %-14:c")))

;; Display tags farther right
(setq org-agenda-tags-column -102)

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

;; (setq org-agenda-remove-tags t)
;; (setq org-agenda-restore-windows-after-quit t)
;; (setq org-agenda-show-inherited-tags nil)
;; (setq org-agenda-skip-additional-timestamps-same-entry t)
;; (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;; Use agenda persistent filters
(setq org-agenda-persistent-filter t)

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
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;; Focus on current work

;; (global-set-key (kbd "<f5>") 'bh/org-todo)

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
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

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
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
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
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
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
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

;; ** Org task structure and presentation
;; *** Handling blocked tasks
;; I tend to forget that tasks are everything that has a headline
;; report that setting somehere else where it belongs.
(setq org-enforce-todo-dependencies t)


;; * Reminders
;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
;; (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
;; (bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
;; (appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
;; (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; By default I want to see deadlines in the agenda 30 days before the due date.
(setq org-deadline-warning-days 30)


;; * Time clocking

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
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

(require 'org-id)

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
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

;; * Time reporting and tracking

;; Sometimes I change tasks I'm clocking quickly - this removes
;; clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Set default column view headings: Task Effort Clock_Summary
;; (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-columns-default-format "%80ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM %TAGS"
      org-tags-column -100)

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("Progress_ALL" . "10% 20% 30% 40% 50% 60% 70% 80% 90%")
        ("Status_ALL" . "Work Leisure GTD WOT")
        ("STYLE_ALL" . "habit")))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items '(closed state))


;; * Tags

;; Tags with fast selection keys
(setq org-tag-alist
      '((:startgroup)
        ("@errand" . ?e) ("@office" . ?o) ("@home" . ?H)
        ("@farm" . ?f)
        (:endgroup)
        ("WAITING" . ?w)
        ("HOLD" . ?h)
        ("PERSONAL" . ?P)
        ("WORK" . ?W)
        ("FARM" . ?F)
        ("ORG" . ?O)
        ("NORANG" . ?N)
        ("crypt" . ?E)
        ("NOTE" . ?n)
        ("CANCELLED" . ?c)
        ("FLAGGED" . ??)))

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

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key 'expert)

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; To make all of the matched headings for a tag show at the same
;; level in the agenda set the following variable
(setq org-tags-match-list-sublevels t)


;; * Refile

(setq org-refile-targets '((nil :maxlevel . 9)
                           ("notes.org" :regexp . "Viewed")
                           (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; Annoying because requires to C-0 C-C C-w often
; (setq org-refile-use-cache t)

;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; * Archive

(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")


;; * Logging

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

;; * GTD stuff

(setq org-stuck-projects '("" nil nil ""))

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

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

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
  ;; (bh/list-sublevels-for-projects-indented)
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
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
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
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
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

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
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

;; * Babel

;; (setq org-ditaa-jar-path "~/java/ditaa.jar")
;; (setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (latex . t)
    (lisp . t)
    (dot . t)
    (org . t)
    (python . t)
    (gnuplot . t)
    (sh . t)
    ;; (ditaa . t)
    ;; (R . t)
    ;; (ruby . t)
    ;; (clojure . t)
    ;; (ledger . t)
    ;; (plantuml . t)
    ))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
(add-to-list 'org-src-lang-modes '("pseudocode" . pseudocode))
(add-to-list 'org-src-lang-modes '("specif" . specif))

;; * Citations

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


;; * Exporter
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
(setq org-export-allow-bind-keywords t)


;; * LaTeX

(setq org-latex-listings 'minted)
(setq org-latex-listings 't)

(setq org-latex-create-formula-image-program 'imagemagick) ;; imagemagick

(setq org-create-formula-image-latex-command "pdflatex")
(setq org-create-formula-image-convert-command "imconvert.exe")

(setq org-latex-pdf-process
      '("c:/Msys64/usr/bin/perl.exe c:/Local/TeXLive/texmf-dist/scripts/latexmk/latexmk.pl -cd -pdf -bibtex -pv- %f"))


(add-to-list
 'org-latex-packages-alist
 '("" "fpsupelec-snippet" t)
 )

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq org-latex-preview-ltxpng-directory
      (expand-file-name "c:/temp/ltxpng/"))

(plist-put org-format-latex-options :scale 1.0)

(defvar fp-inside-questions nil)

(defun fp-reset-inside-questions (&rest args)
  (setq fp-inside-questions ()))

(add-hook 'org-export-before-parsing-hook 'fp-reset-inside-questions)

(defun fp-org-latex-compute-exam-section (level numbered)
  ; (message "level %s numbered %s" level numbered)
  (case level
    (1 (if fp-inside-questions
           (progn (setq fp-inside-questions nil)
                  '("\\section{%s}" "\\end{questions}"))
         (progn (setq fp-inside-questions nil)
                '("\\section{%s}" ""))))
    (2 (if fp-inside-questions
           '("\\myquestion{%s}" . "")
         (progn
           (setq fp-inside-questions t)
           '("\\begin{questions}\n\\myquestion{%s}" . ""))))
    (3 '("\\begin{soluce} %% %s" "\\end{soluce}"))
    (t '("%s" "")))
  )

(defun fp-org-latex-compute-examshort-section (level numbered)
                                        ; (message "level %s numbered %s" level numbered)
  (case level
    (1 '("\\begin {questions} %% %s" "\\end{questions}"))
    (2 '("\\myquestion{%s}" . ""))
    (3 '("\\begin{soluce} %% %s" "\\end{soluce}"))
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
  (message "********************\nheadline = %s\ninfo = %s" info)
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
         ("\\Exercise{%s}" . "\\Exercise{%s}"))
        ("td" "\\documentclass{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         ("\\fpremark{%s}\\fpnewtd" . "\\fpremark{%s}\\fpnewtd")
         ("\\Exercise{%s}" . "\\Exercise{%s}")
         ("\\begin{questiion} %% %s" "\\end{questiion}")
         ("\\begin{soluce} %% %s" "\\end{soluce}"))
        ("exam" "\\documentclass[addpoints,solutions,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
         fp-org-latex-compute-exam-section
         )
        ("examshort" "\\documentclass[addpoints,solutions,french,exam]{fpsupelec}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
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
        ("springer" "\\documentclass{svjour3}\n[NO-DEFAULT-PACKAGES]\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}"))
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
        ("my-letter"
         "\\documentclass\{scrlttr2\}\n\\usepackage[english]{babel}\n\\setkomavar{frombank}{(1234)\\,567\\,890}\n\[DEFAULT-PACKAGES]\n\[PACKAGES]\n\[EXTRA]")
        ("my-letterCS"
         "\\documentclass\{scrlttr2\}\n\\usepackage[english]{babel}\n\\graphicspath{{.}{c:/Home/Administratif/Logos/}}\n\\AddToShipoutPictureBG{\\includegraphics{lettreCS.pdf}}\n\\def\\Put(#1,#2)#3{\\leavevmode\\makebox(0,0){\\put(#1,#2){#3}}}\n\\setkomavar{frombank}{(1234)\\,567\\,890}\n\[DEFAULT-PACKAGES]\n\[PACKAGES]\n\[EXTRA]")
        ("ma-lettre"
         "\\documentclass\{scrlttr2\}\n\\usepackage[]{babel}\n\\setkomavar{frombank}{(1234)\\,567\\,890}\n\[NO-DEFAULT-PACKAGES]\n\[NO-PACKAGES]\n\[EXTRA]")
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


;; * Beamer

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


;; * HTML

(setq org-html-head "")
(setq org-html-inline-images t)
(setq org-html-head-include-scripts nil)
(setq org-html-head-include-default-style nil)

(setq org-html-htmlize-output-type 'css)

(setq org-html-table-row-open-tag
      '(lambda (number group-number start-group-p end-group-p top-p bottom-p)
         (cond (top-p "<tr class=\"tr-top\">")
               (bottom-p "<tr class=\"tr-bottom\">")
               (t (if (= (mod number 2) 1)
                      "<tr class=\"tr-odd\">"
                    "<tr class=\"tr-even\">")))))

(setq org-html-table-row-close-tag
      "</tr>")

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



;; * Publish
(setq org-publish-timestamp-directory
      (expand-file-name  "org-timestamps/" prelude-savefile-dir))

;; Taken from o-blog
(defun ob:eval-lisp()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
                       when publishing a page."
  (save-excursion
    (save-restriction
      (save-match-data
        ;; needed for thing-at-point
        (html-mode)
        (beginning-of-buffer)
        (let ((open-tag "<lisp>\\|{lisp}\\|\\[lisp\\]")
              (close-tag "</lisp>\\|{/lisp}\\|\\[/lisp\\]")
              beg end sexp)
          (while (search-forward-regexp open-tag nil t)
            (setq beg (- (point) (length  (match-string 0))))
            (when (search-forward-regexp close-tag nil t)
              (setq end (point))
              (backward-char (length (match-string 0)))
              (backward-sexp)
              (setq sexp (substring-no-properties (thing-at-point 'sexp)))
              ;; In some exporters (pandoc) " are replaced with &quot; which
              ;; breaks lisp interpolation.
              (with-temp-buffer
                (insert sexp)
                (goto-char (point-min))
                (while (search-forward "&quot;" nil t)
                  (replace-match "\"" nil t))
                (setq sexp (buffer-string)))
              (narrow-to-region beg end)
              (delete-region (point-min) (point-max))
              (insert
               (save-match-data
                 (condition-case err
                     (let ((object (eval (read sexp))))
                       (cond
                        ;; result is a string
                        ((stringp object) object)
                        ;; a list
                        ((and (listp object)
                              (not (eq object nil)))
                         (let ((string (pp-to-string object)))
                           (substring string 0 (1- (length string)))))
                        ;; a number
                        ((numberp object)
                         (number-to-string object))
                        ;; nil
                        ((eq object nil) "")
                        ;; otherwise
                        (t (pp-to-string object))))
                   ;; error handler
                   (error ; (debug error)
                    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
              (goto-char (point-min))
              (widen))))))))

;; Interesting filter. Simplify it to keep ignoreheading only for html.
(defun tsd-filter-headline-tags (contents backend info)
  "Ignore headlines with tag `ignoreheading' and/or start LaTeX
                section with `newpage' or `clearpage' command."
  (cond ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\`.*newpage.*\n" (downcase contents))
              ;; if you want to get rid of labels use the string
              ;; "\\`.*ignoreheading.*\n.*\n"
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "\\\\newpage\n" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\`.*clearpage.*\n" (downcase contents))
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "\\\\clearpage\n" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex 'html 'ascii)
              (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
         (replace-match "" nil nil contents))
        ((and (org-export-derived-backend-p backend 'latex)
              (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?newpage.*?}\\(.*\n\\)"
                            (downcase contents)))
         (replace-match "\\\\newpage\n\\1\\2"  nil nil contents))
        ((and c(org-export-derived-backend-p backend 'latex)
              (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?clearpage.*?}\\(.*\n\\)" (downcase contents)))
         (replace-match "\\\\clearpage\n\\1\\2"  nil nil contents))))

;; (add-to-list 'org-export-filter-headline-functions 'tsd-filter-headline-tags)

;;  (org-export-to-file 'html (org-export-output-file-name ".html" t) nil t nil t)

(defun fp-org-pdf-export-function()
  (let* ((file (org-export-output-file-name ".tex" t))
         (outdir (expand-file-name "pdf/" (file-name-directory file)))
         (outfile (org-export-output-file-name ".tex" t outdir))
         )
    (message "outdir = %s\noutfile = %s" outdir outfile)
    (org-export-to-file 'latex outfile nil t nil nil nil
                        (lambda (file) (org-latex-compile file)))))

(defun fp-export-pdf ()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (org-map-entries #'fp-org-pdf-export-function "pdf" 'file)))

(defun fp-export-body ()
  (with-current-buffer org-buffer
    (org-export-as 'html t nil t)))

(defun fp-org-html-export-function()
  (let ((org-buffer (current-buffer))
        (file (org-export-output-file-name ".html" t)))
    (when file
      (message "%s" file)
      (with-current-buffer (find-file-noselect file t)
        (erase-buffer)
        (insert-buffer-substring (find-file-noselect "~/Org/templates/template-page-normale.html" t))
        (ob:eval-lisp)
        (save-buffer)
        ))))


(defun fp-export-current-document()
  (interactive)
  (let ((org-use-tag-inheritance nil)
        (fp-html-menu-string nil))
    (org-map-entries #'fp-org-html-export-function "web" 'file)))

(defun fp-export-current-slides()
  (interactive)
  (let ((org-use-tag-inheritance nil))
    (org-map-entries
     #'(lambda ()
         (org-export-to-file
             'reveal
             (org-export-output-file-name ".html" t) nil t)) "slides" 'file)))

(local-set-key (kbd "<f5>") #'fp-export-current-document)

(defvar fp-html-menu-string nil)

(defun fp-export-menu ()
  (or fp-html-menu-string
      (setq fp-html-menu-string
            (let ((org-use-tag-inheritance nil))
              (let (menu (previous-level 1))
                (with-current-buffer org-buffer
                  (org-map-entries #'fp-build-menu "webmenu|web-nopublic" 'file)
                  (message "%s" menu)
                  (fp-build-menu-format-menu menu)))))))

(defun fp-build-menu-format-menu (menu)
  (apply #'concatenate 'string
         (loop for entry in menu
               collect (fp-build-menu-format-entry entry))))

(defun fp-build-menu-format-entry (entry)
  (let ((submenu (and (member :submenu entry)
                      (fp-build-menu-format-menu (cadr (member :submenu entry))))))
    (concatenate 'string
                 (concatenate 'string
                              "<li><a href=\""
                              (or (and (not submenu) (cadr (member :link entry))) "#")
                              "\""
                              (when submenu
                                " data-toggle=\"collapse\" data-target=\"#submenu\"")
                              ">"
                              (when (member :icon entry)
                                (concatenate 'string "<i class=\"fa fa-2x " (cadr (member :icon entry)) "\"></i>"))
                              (or (cadr (member :text entry)) "")
                              ;;(when submenu
                              ;;  "<span class=\"fa fa-2x fa-carret-down\"/>")
                              "</a>"
                              (when submenu
                                (concatenate 'string "<ul class=\"nav nav-second-level collapse\" id=\"submenu\">"
                                             submenu "</ul>"))
                              "</li>"
                              ))))

(defun fp-build-menu ()
  (let* ((current-level (org-current-level))
         (text (car (org-property--local-values "EXPORT_HTML_MENU_TEXT" nil)))
         (icon (car (org-property--local-values "EXPORT_HTML_MENU_ICON" nil)))
         (link (concat "/" (car (org-property--local-values "EXPORT_FILE_NAME" nil)))))
    (setq menu  (fp-build-menu-push-at-right-place menu (list (list :text text :icon icon :link link)) current-level previous-level))
    (setq previous-level current-level)))

(defun fp-get-variable(var)
  (save-mark-and-excursion
   (goto-char (point-min))
   (when (re-search-forward (concat "^#\\+" var ":\\s-*\\(.*\\)\\s-*$") nil t)
     (match-string 1))))

(defun fp-export-profile ()
  (with-current-buffer org-buffer
    (concat  (format "<div class=\"course hidden-xs\" style=\"background-image:url(/images/%s)\"></div>\n"
                     (fp-get-variable "HTML_ICON"))
             (format "<div class=\"title\">\n
                                 <h2>%s</h2>\n
                                       <img src=\"/images/Logo-CentraleSupelec-small.png\"/>\n
                                  </div>\n"
                     (fp-get-variable "HTML_TITLE")
                     ))))

(defun fp-build-menu-push-at-right-place (l new current previous)
  (let ((last (last l))
        (butlast nil))
    (while (and last (member :submenu (car last)))
      (setq butlast last)
      (setq last (last  (cadr (member :submenu (car last))))))
    (cond ((null l) new)
          ((> current previous)
           (nconc (car last) (list :submenu new))
           l)
          ((< current previous)
           (nconc  butlast new)
           l)
          (t (nconc last new)
             l))))

(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))



;; * Outshine
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


;; * Conclusion
(provide 'fp-org)
;; fp-org ends here
