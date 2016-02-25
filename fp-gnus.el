;;; fp-org.el --- Emacs Prelude: Fabrice Popineau Org mode configuration.
;;
;; Copyright © 2014 Fabrice Popineau
;;
;; Author: Fabrice Popineau <fabrice.popineau@gmail.com>
;;         based on work by Bastien Guerry <bzg@gnu.org>
;; URL: https://github.com/fpopineau/.emacs.d
;; Version: 0.0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs.

(require 'mail-extr)
(require 'message)

(require 'gnus)
(require 'starttls)
(require 'epg)
(require 'epa)

                                        ; (require 'nnml)
(require 'nnimap)
(require 'nndiary)
(require 'nndoc)
(require 'nneething)
(require 'nnir)
(require 'nnrss)

(require 'nnweb)
(require 'gnus-agent)
(require 'gnus-win)
(require 'browse-url)
(require 'gnus-diary)
(require 'gnus-art)
                                        ; (require 'gnus-alias)
                                        ; (require 'gnus-highlight)
                                        ; (require 'gnus-eyecandy)
(require 'ietf-drums)
(require 'gnus-fun)
(require 'gnus-sum)
(require 'gnus-delay)
(require 'gnus-group)
(require 'gnus-ml)
(require 'gnus-picon)
(require 'gnus-logic)
(require 'gnus-registry)
(require 'mm-decode)

(require 'smtpmail)

(setq epa-popup-info-window nil)

(require 'smtpmail)
;; (require 'spam)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "fabrice.popineau@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "supelec.fr")

;; (setq spam-use-spamassassin t)
;; (setq spam-spamassassin-path "/usr/bin/vendor_perl/spamassassin")
;; (setq spam-use-spamassassin-headers t)
(setq smiley-style 'medium)


;; Set sendmail function and Gnus methods
(require 'boxquote)

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

(setq use-dialog-box nil)
(setq user-full-name "Fabrice Popineau")
(setq user-mail-address "fabrice.popineau@gmail.com")

(setq mail-header-separator "----")
(setq mail-specify-envelope-from t)
(setq mail-use-rfc822 nil)

(setq message-cite-function (quote message-cite-original-without-signature))
(setq message-default-charset (quote utf-8))
(setq message-generate-headers-first t)

;; Attachments
(setq mm-content-transfer-encoding-defaults
      (quote
       (("text/x-patch" 8bit)
        ("text/.*" 8bit)
        ("message/rfc822" 8bit)
        ("application/emacs-lisp" 8bit)
        ("application/x-emacs-lisp" 8bit)
        ("application/x-patch" 8bit)
        (".*" base64))))
(setq mm-default-directory "c:/Users/Fabrice/Downloads/")
(setq mm-url-program (quote eww))
(setq mm-url-use-external nil)

(setq nnmail-extra-headers
      '(X-Diary-Time-Zone X-Diary-Dow X-Diary-Year
        X-Diary-Month X-Diary-Dom X-Diary-Hour X-Diary-Minute To Newsgroups Cc))

;; Sources and methods
(setq mail-sources nil
      ;; '((file :path "/var/mail/guerry")
      ;;                (maildir :path "~/Maildir/" :subdirs ("cur" "new")))
      gnus-select-method '(nnimap "gmail"
                           (nnimap-address "imap.gmail.com")   ; it could also be imap.googlemail.com if that's your server.
                           (nnimap-server-port 993)
                           (nnimap-stream ssl)
                           (nnir-search-engine imap)
                           (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                           (nnmail-expiry-wait 90))
      gnus-secondary-select-methods
      '((nntp "gmane" (nntp-address "news.gmane.org"))))


;; (setq gnus-refer-article-method
;;       '(current
;;         (nntp "localhost"
;;          (nntp-address "127.0.0.1"))
;;         ;; (nntp "free"
;;         ;;       (nntp-address "news.free.fr"))
;;         (nntp "gmane"
;;          (nntp-address "news.gmane.org"))
;;         (nnweb "refer" (nnweb-type google))))

(setq gnus-check-new-newsgroups nil)
(setq gnus-read-active-file 'some)
(setq gnus-agent t)
(setq gnus-agent-consider-all-articles t)
(setq gnus-agent-enable-expiration 'disable)
;; Set basics
(setq read-mail-command 'gnus
      message-mail-user-agent 'gnus-user-agent
      message-kill-buffer-on-exit t
      user-mail-address "fabrice.popineau@gmail.com"
      mail-envelope-from "fabrice.popineau@gmail.com"
      mail-user-agent 'gnus-user-agent
      mail-specify-envelope-from nil
      gnus-directory "~/News/"
      gnus-novice-user nil
      gnus-inhibit-startup-message t
      gnus-play-startup-jingle nil
      gnus-interactive-exit nil
      gnus-no-groups-message "No news, good news."
      gnus-show-all-headers nil
      gnus-use-correct-string-widths nil
      gnus-use-cross-reference nil
      gnus-asynchronous t
      gnus-interactive-catchup nil
      gnus-inhibit-user-auto-expire t
      gnus-gcc-mark-as-read t
      gnus-verbose 6
      gnus-backup-startup-file t
      gnus-use-tree t
      gnus-use-header-prefetch t
      gnus-large-newsgroup 10000
      nnmail-expiry-wait 'never
      nnimap-expiry-wait 'never
      nnmail-crosspost nil
      nnmail-expiry-target "nnml:expired"
      nnmail-split-methods 'nnmail-split-fancy
      nnmail-treat-duplicates 'delete
      nnml-marks nil
      gnus-nov-is-evil nil
      nnml-marks-is-evil t
      nntp-marks-is-evil t)

(setq gnus-ignored-from-addresses
      (regexp-opt '("fabrice.popineau@supelec.fr"
                    "fabrice.popineau@free.fr"
                    "fabrice.popineau@gmail.com")))

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Replace [ and ] with _ in ADAPT file names
(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

(setq message-dont-reply-to-names gnus-ignored-from-addresses)

;; Start the topic view
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Levels and subscription
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
      gnus-group-default-list-level 3
      gnus-level-default-subscribed 3
      gnus-level-default-unsubscribed 7
      gnus-level-subscribed 6
      gnus-level-unsubscribed 7
      gnus-activate-level 5)

(setq gnus-posting-styles
      '(((header "to" "fabrice.popineau@gmail.com")
         (address "fabrice.popineau@gmail.com"))
	((header "to" "fabrice.popineau@supelec.fr")
         (address "fabrice.popineau@supelec.fr"))
	((header "cc" "fabrice.popineau@gmail.com")
         (address "fabrice.popineau@gmail.com"))
	((header "cc" "fabrice.popineau@supelec.fr")
         (address "fabrice.popineau@supelec.fr"))))

;; Archives
(setq gnus-message-archive-group
      '((if (message-news-p)
            (concat "nnfolder+archive:" (format-time-string "%Y-%m")
                    "-divers-news")
          (concat "nnfolder+archive:" (format-time-string "%Y-%m")
                  "-divers-mail"))))

;; Delete mail backups older than 1 days
(setq mail-source-delete-incoming 1)

;; Select the first mail when entering a group
(setq gnus-auto-select-first t)
(setq gnus-auto-select-subject 'unread)

;; Group sorting
(setq gnus-group-sort-function
      '(gnus-group-sort-by-unread
        gnus-group-sort-by-alphabet
        gnus-group-sort-by-score
        gnus-group-sort-by-level))

;; Thread sorting (from Gnus master branch as of 2013-07-30)
;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-most-recent-date
;;         gnus-thread-sort-by-number
;;         gnus-thread-sort-by-total-score)
;;       gnus-subthread-sort-functions
;;       '(gnus-thread-sort-by-date
;;         gnus-thread-sort-by-number)
;;       gnus-sort-gathered-threads-function
;;       'gnus-thread-sort-by-date)

;; Display the thread by default
(setq gnus-thread-hide-subtree nil)

;; Headers we wanna see:
(setq gnus-visible-headers
      "^From:\\|^Subject:\\|^X-Mailer:\\|^X-Newsreader:\\|^Date:\\|^To:\\|^Cc:\\|^User-agent:\\|^Newsgroups:\\|^Comments:")

;;; [En|de]coding
(setq mm-body-charset-encoding-alist
      '((utf-8 . 8bit)
        (iso-8859-1 . 8bit)
        (iso-8859-15 . 8bit)))

(setq mm-coding-system-priorities
      '(iso-8859-1 iso-8859-9 iso-8859-15 utf-8
                   iso-2022-jp iso-2022-jp-2 shift_jis))

;; bbdb
(setq gnus-use-generic-from t
      gnus-use-bbdb t)

;;; Trier les mails
(setq nnmail-split-abbrev-alist
      '((any . "From\\|To\\|Cc\\|Sender\\|Apparently-To\\|Delivered-To\\|X-Apparently-To\\|Resent-From\\|Resent-To\\|Resent-Cc")
        (mail . "Mailer-Daemon\\|Postmaster\\|Uucp")
        (to . "To\\|Cc\\|Apparently-To\\|Resent-To\\|Resent-Cc\\|Delivered-To\\|X-Apparently-To")
        (from . "From\\|Sender\\|Resent-From")
        (nato . "To\\|Cc\\|Resent-To\\|Resent-Cc\\|Delivered-To\\|X-Apparently-To")
        (naany . "From\\|To\\|Cc\\|Sender\\|Resent-From\\|Resent-To\\|Delivered-To\\|X-Apparently-To\\|Resent-Cc")))

;; Load nnmail-split-fancy (private)
;; (load "/home/guerry/elisp/config/gnus_.el")

;; Simplify the subject lines
(setq gnus-simplify-subject-functions
      '(gnus-simplify-subject-re
        gnus-simplify-whitespace))

;; Display faces
(setq gnus-treat-display-face 'head)

;; Thread by Xref, not by subject
(setq gnus-thread-ignore-subject t)
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-references)

(when window-system
  (setq
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "├► "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "╰► "
   gnus-sum-thread-tree-vertical "│"))

;; Dispkay a button for MIME parts
(setq gnus-buttonized-mime-types '("multipart/alternative"))

;; Use w3m to display HTML mails
(setq mm-text-html-renderer 'shr
      mm-inline-text-html-with-images t
      mm-inline-large-images nil
      mm-attachment-file-modes 420)

;; Avoid spaces when saving attachments
(setq mm-file-name-rewrite-functions
      '(mm-file-name-trim-whitespace
        mm-file-name-collapse-whitespace
        mm-file-name-replace-whitespace))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "     %k:%M")
        ((+ 86400 (gnus-seconds-today)) . "hier %k:%M")
        ((+ 604800 (gnus-seconds-today)) . "%a  %k:%M")
        ((gnus-seconds-month) . "%a  %d")
        ((gnus-seconds-year) . "%b %d")
        (t . "%b %d '%y")))

;; Add a time-stamp to a group when it is selected
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; Format group line
(setq gnus-group-line-format
      ;;      "%M\%S\%p\%P\%5T>%5y: %(%-40,40g%) %ud\n")
      ;;      "%M\%S\%p\%P\%y: %(%-40,40g%) %T/%i\n")
      ;;      "%M\%S\%p\%P %(%-30,30G%) %-3y %-3T %-3I\n")
      "%M\%S\%p\%P %(%-40,40G%)\n")

(setq gnus-topic-indent-level 3)

(defun bzg-gnus-toggle-group-line-format ()
  (interactive)
  (if (equal gnus-group-line-format
             "%M\%S\%p\%P %(%-40,40G%) %-3y %-3T %-3I\n")
      (setq gnus-group-line-format
             "%M\%S\%p\%P %(%-40,40G%)\n")
    (setq gnus-group-line-format
          "%M\%S\%p\%P %(%-40,40G%) %-3y %-3T %-3I\n")))

;; (defun bzg-gnus-add-gmane ()
;;   (add-to-list 'gnus-secondary-select-methods
;;                '(nntp "news" (nntp-address "news.gmane.org"))))

(define-key gnus-group-mode-map "x"
  (lambda () (interactive) (bzg-gnus-toggle-group-line-format) (gnus)))

;; (define-key gnus-group-mode-map "X"
;;   (lambda () (interactive) (bzg-gnus-add-gmane) (gnus)))

(define-key gnus-summary-mode-map "$" 'gnus-summary-mark-as-spam)

;; Scoring
(setq gnus-use-adaptive-scoring 'line
      ;; gnus-score-expiry-days 14
      gnus-default-adaptive-score-alist
      '((gnus-dormant-mark (from 20) (subject 100))
        (gnus-ticked-mark (subject 30))
        (gnus-read-mark (subject 30))
        (gnus-del-mark (subject -150))
        (gnus-catchup-mark (subject -150))
        (gnus-killed-mark (subject -1000))
        (gnus-expirable-mark (from -1000) (subject -1000)))
      gnus-score-decay-constant 1    ;default = 3
      gnus-score-decay-scale 0.03    ;default = 0.05
      gnus-decay-scores t)           ;(gnus-decay-score 1000)

;; (setq gnus-face-0 '((t (:foreground "grey60"))))
;; (setq gnus-face-1 '((t (:foreground "grey30"))))
;; (setq gnus-face-2 '((t (:foreground "grey90"))))

;; Prompt for the right group
(setq gnus-group-jump-to-group-prompt
      '((0 . "nnml:mail.")
        (1 . "nnfolder+archive:2013-")
        (2 . "nnfolder+archive:2012-")
        (3 . "nntp+news:gmane.")))

(setq gnus-summary-line-format
      (concat "%*%0{%U%R%z%}"
              "%0{ %}(%2t)"
              "%2{ %}%-23,23n"
              "%1{ %}%1{%B%}%2{%-102,102s%}%-140="
              "\n"))

(setq gnus-summary-line-format (concat
                                "%*%4{%U%R%z%}"
                                "%3{|%}"
                                "%1{%-10&user-date;%}"
                                "%3{|%}"
                                "%3{|%}"
                                "%1{ %}%(%-24,24n"
                                "%3{|%}"
                                "%1{%6i%}"
                                "%3{|%}"
                                "%1{%6k %}%)"
                                "%3{|%}"
                                "%1{ %}%2{%B%}%{%s%}\n")
      gnus-summary-mode-line-format "%V  %p [%A] %Z"
      )

(setq gnus-summary-thread-gathering-function  'gnus-gather-threads-by-references ; Use the References: header to thread
      )
(setq gnus-topic-display-empty-topics nil
      gnus-topic-line-format "%i%(%[%{ %n -- %A %}%]%)%v\n"
      )

(require 'nnir)

;; Then add the following line to the secondary-method in .gnus.el
(setq nnir-search-engine 'imap)

;; It should look like this:
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                              (nnimap-stream ssl)
                                              (nnimap-address "imap.gmail.com")
                                              (nnimap-server-port 993)
                                              (nnir-search-engine imap)))

(require 'ecomplete)
(setq message-mail-alias-type 'ecomplete)

(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)

(require 'gnus-gravatar)

;; Hack to store Org links upon sending Gnus messages

(defun bzg-message-send-and-org-gnus-store-link (&optional arg)
  "Send message with `message-send-and-exit' and store org link to message copy.
If multiple groups appear in the Gcc header, the link refers to
the copy in the last group."
  (interactive "P")
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (let ((gcc (car (last
                         (message-unquote-tokens
                          (message-tokenize-header
                           (mail-fetch-field "gcc" nil t) " ,")))))
              (buf (current-buffer))
              (message-kill-buffer-on-exit nil)
              id to from subject desc link newsgroup xarchive)
        (message-send-and-exit arg)
        (or
         ;; gcc group found ...
         (and gcc
              (save-current-buffer
                (progn (set-buffer buf)
                       (setq id (org-remove-angle-brackets
                                 (mail-fetch-field "Message-ID")))
                       (setq to (mail-fetch-field "To"))
                       (setq from (mail-fetch-field "From"))
                       (setq subject (mail-fetch-field "Subject"))))
              (org-store-link-props :type "gnus" :from from :subject subject
                                    :message-id id :group gcc :to to)
              (setq desc (org-email-link-description))
              (setq link (org-gnus-article-link
                          gcc newsgroup id xarchive))
              (setq org-stored-links
                    (cons (list link desc) org-stored-links)))
         ;; no gcc group found ...
         (message "Can not create Org link: No Gcc header found."))))))

(define-key message-mode-map [(control c) (control meta c)]
  'bzg-message-send-and-org-gnus-store-link)

;; (defun gnus-thread-sort-by-length (h1 h2)
;;   "Sort threads by the sum of all articles in the thread."
;;   (> (gnus-thread-length h1)
;;      (gnus-thread-length h2)))

;; (defun gnus-thread-length (thread)
;;   "Find the total number of articles in THREAD."
;;   (cond
;;    ((null thread) 0)
;;    ((listp thread) (length thread))))

(setq message-fill-column 70)
(setq message-use-mail-followup-to nil)



(setq w3m-accept-languages '("fr;" "q=1.0" "en;"))
(setq w3m-antenna-sites '(("http://eucd.info" "EUCD.INFO" time)))
(setq w3m-broken-proxy-cache t)
(setq w3m-confirm-leaving-secure-page nil)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-cookie-accept-domains t)
(setq w3m-cookie-file "c:/Home/.emacs.d/savefile/cookie")
(setq w3m-fill-column 70)
(setq w3m-form-textarea-edit-mode 'org-mode)
(setq w3m-icon-directory nil)
(setq w3m-key-binding 'info)
(setq w3m-use-cookies t)
(setq w3m-use-tab t)
(setq w3m-use-toolbar nil)


;;_ + Fancy splitting
(defun fp-build-split-regexp (x &optional y)
  (flet ((f (l)
           (let (result)
             (if (null l)
                 ".*"
               (setq result (concat "\\(" ;(regexp-quote
                                    (pop l)
                                        ;)
                                    ))
               (mapc (lambda (x) (setq result
                                       (concat result "\\|" ;(regexp-quote
                                               x
                                        ;)
                                               ))) l)
               (concat result "\\)")))))
    (concat (f x) ".*@.*" (f y))
    )
  )

(setq
 fp-split-people-friends-personal
 (fp-build-split-regexp
  '(
    "Alain.Rodermann"
    "Patricia.Cayetanot"
    "Hervé.Cayetanot"
    "Battini.Fabien"
    "Marc.Rakotomalala"
    )
  )
 fp-split-people-jadfs
 (fp-build-split-regexp
  '(
    "Bruno.Paul"
    "Bruno.VAILHEN"
    "CVRcrombez"
    "Christophe.Klein"
    "GAUBERT.Laurence"
    "Gilles.Gauchoux"
    "JBoutonnat"
    "Jean.Lacoume"
    "Kyriakos.Vavalidis"
    "Laurent Lipere"
    "Marc.Neyret"
    "Marie-Laur.Pedamon"
    "Meyer"
    "Stephane.Santori"
    "Tina.Ferembach"
    "agnes.schneider"
    "anne-marie.lacombe"
    "bruno.vailhen"
    "cjuillet"
    "cneyret"
    "francoise.berge"
    "gc.gauchoux@freesurf.ch"
    "gilles.cannet"
    "gilles.gauchoux"
    "jcbarre"
    "jean-claude.filly"
    "jean-louis.constans"
    "jean-louis.joyeux"
    "jlwippler"
    "klein.pierre"
    "l.fillion"
    "lacoume.berge"
    "laurence.gaubert"
    "laurent.nivert"
    "lipere"
    "magali.filly"
    "mchklein"
    "mpedamon"
    "nathalie.neyret"
    "nicolas.filly"
    "patrice.clerc"
    "pclerc"
    "pfabre"
    "renaud.meyer"
    "reny.safi"
    "robert.brugger"
    "rom1f1"
    "serre"
    "tina.moxham"
    "vincent.crombez"
    "yaev.neyret"
    "yannick.neyret"
    )
  )
 fp-split-supelec-metz-regexp
 (fp-build-split-regexp
  '("herve.frezza-buet" "frezza"
    "abolfazl.fatholazadeh" "afzadeh"
    "michel.ianotto" "ianotto"
    "stephane.vialle" "vialle"
    "patrick.mercier" "mercier"
    "claude.lhermitte" "lhermitte"
    "jean-louis.gutzwiller" "gutzwill"
    "annie.levey" "levey"
    "yves.houzelle" "houzelle"
    "jean-marc.vanzo" "vanzo"
    "beatrice.chevaillier" "chevaill"
    "delphine.wolfsberger" "wolfsber"
    "patrick.turelle" "turelle"
    "joel.soumagne" "soumagne"
    "georges.seignier" "seignier"
    "philippe.morosini" "morosini"
    "jean.maufoy" "maufoy"
    "catherine.mailhan" "mailhan"
    "nicolas.fressengeas" "fresseng"
    "jean-luc.collette" "collette"
    "michel.barret" "barret"
    "frederic.pennerath" "pennerath")
  '("ese-metz.fr" "metz.supelec.fr" "supelec.fr")
  )
 fp-split-supelec-metz-tech-regexp
 (fp-build-split-regexp
  '("claudine.mercier" "cmercier"
    "gilles.berlin" "berlin"
    "sebastien.vanluchene" "vanluche"
    "eric.lepezel" "lepezel"
    )
  '("ese-metz.fr" "metz.supelec.fr" "supelec.fr")
  )
 fp-split-supelec-metz-staff-regexp
 (fp-build-split-regexp
  '("veronique.baudot" "baudot"
    "dorothee.chenh" "chenh"
    "dorothee.bertolo" "bertolo"
    "serge.durin" "durin"
    "laurence.fernandes" "lfernand"
    "therese.fressengeas" "tfressen"
    "fabienne.munier" "fmunier"
    "fabienne.lemius" "lemius"
    "pascale.morosini" "pmorosini"
    "veronique.pretre" "pretre"
    "dominique.turelle" "dturelle"
    )
  '("ese-metz.fr" "metz.supelec.fr" "supelec.fr")
  )
 fp-split-supelec-supelec-staff-regexp
 (fp-build-split-regexp
  '("anne.battalie"
    "lilliane.lopes"
    )
  '("supelec.fr")
  )
 fp-split-supelec-supelec-regexp
 ".*\\(\\..*@supelec\\|@supelec-rennes\\)\\.fr"
 fp-split-people-tex-personal
 (fp-build-split-regexp
  '(
    "Rainer.Schöpf"
    "Jerzy.Ludwichowski"
    "Jim.Hefferon"
    "berend.de.boer"
    "bernard.gaulle"
    "christophe.pythoud"
    "daniel.flipo"
    "eitan.gurari"
    "eddie.kohler"
    "han.the.thanh"
    "hans.hagen"
    "jacques.andre"
    "kaja.christiansen"
    "karel.skoupy"
    "karl.berry"
    "kaveh.bazargan"
    "maurice.laugier"
    "michel.bovani"
    "olaf.weber"
    "pierre.legrand"
    "radhakrishnan.cv"
    "sarah.grimaud"
    "s.rahtz"
    "sebastian.rahtz"
    "staszek.waw"
    "thomas.esser"
    "tom.kackvinsky"
    "tom.kacvinsky"
    "tomas.rockiki"
    "volker.rw.schaa"
    "wendy.mckay"
    "werner.lemberg"
    "yannis.haralambous"
    "john.plaice"
    "eli.zaretskii"
    "paul.vojta"
    )
  )
 fp-split-subjects-tex-personal
 "\\(kpse.*\\|kpathsea\\|tex4ht\\|tex[-]?live\\|fptex\\|windvi.*\\|web2c.*\\|dvi\\|ctan\\|\\(o\\)?xdvi.*\\|wingut\\|\\(pdf\\)?\\(la\\)?tex\\(k\\)?\\|omega\\|\\(o\\)?dvips\\(k\\)?\\|latex2html\\|l2h\\)"
 )

(setq nnimap-inbox "INBOX"
      nnimap-split-predicate "UNDELETED"

      ;; avoid duplicating mail while splitting: the first matching
      ;; rule will be used
      nnimap-split-crosspost nil
      ;; Download message body to use with spam detectors... (not anymore!)
      nnimap-split-download-body nil
      nnimap-split-fancy-match-partial-words t

      nnimap-split-fancy nnmail-split-fancy
      nnimap-split-methods 'nnmail-split-fancy ; Fancy mail splitting, please
;        nnmail-split-fancy `(| ("x-gnus-mail-source"
;                                ,(concat "directory:" nix-mailing-list-directory "\\(.*\\)")
;                                "\\1")
;                               "Mailbox")
      nnmail-cache-accepted-message-ids t
      nnmail-message-id-cache-length 20000
      nnmail-split-fancy
      `(|
;       (from "fabrice\\.popineau@.*\supelec\\.fr" "perso.misc")
        (: nnmail-split-fancy-with-parent)
        ;; other splits go here
;       ("from" mail (| ("subject" "warn.*" "mail.warning")
;       "mail.misc"))
        ("gnus-warning" "duplicat\\(e\\|ion\\) of message" "systeme.duplicate")

        ;; Misc, stuff that could be detected as being spam
        (from ".*@nytimes\\.com" "misc.ny-times")
        (from "profession politique" "misc.profession-politique")
        (any ".*@.*\\(alsdesign\\.fr\\|alapage\\.com\\|ap-informatique.com\\|aquarelle\\.com\\|boursorama\\.com\\|castorama\\.fr\\|cdiscount\\.com\\|cdw\\.com\\|componentsource\\.com\\|emilio\\.com\\|execsoft\\.co\\.uk\\|ibm\\.ihost\\.com\\|installshield\\.com\\|ldlc\\.com\\|lotonext\\.com\\|pointop\\.com\\|priceminister\\.com\\|programmers\\.com\\|proxis\\.be\\|rueducommerce\\.com\\|techonline.com\\|topachat.com\\|topfinance.com\\|clust\\.com\\|emv4.com\\|mathtools.net\\|2xmoinscher.com\\|mistergooddeal.com\\|kelkoo.fr\\|meilleurduchef.com\\|lego\\.com\\|camif\\.fr\\|marcopoly\\.com\\|dell\\.fr\\|maximiles\\.fr\\)"
             (| ("subject" "\\(commande\\|your.*order\\)" "perso.commandes")
                "misc.stuff"))
        (from "\\(ChateauOnline@bp03\\.net\\)" "misc.stuff")
        (from "\\(camif@bp01\\.net\\)" "misc.stuff")
        (from "\\(.*@.*venteprivee\\.com\\)" "perso.vente-privee")
        (any "\\(castorama.*@\\|Dell Computer.*DELL@\\|Dell.*Marketing@\\|IBM e-business.*@.*\\.com\\|gooddeal.*@.*gooddeal\\.com\\|.*@superfourmi.com\\)" "misc.stuff")
        ;; fptex mailing list at NTG
        (from "fptex-bounces" "tex.fptex-admin")
        ;; Spam rules
        (|
         ;;      ("X-Spambayes-\\(Classification\\|Trained\\)" "ham" nil)
         ("X-Spambayes-\\(Classification\\|Trained\\)" "spam" "systeme.spam")
         ;;      ("X-Spam-\\(Status\\|Flag\\)" "No" nil)
         ("X-Spam-\\(Status\\|Flag\\)" "Yes"
          ;; Spam white list
          (| (from "\\(bauchat\\|bertholo\\|pegorier\\)" nil)
             ("subject" "\\(commande\\|order\\)" nil)
             "systeme.spam"))
         ("any" "popineau@prunel.*\\.fr" "systeme.spam")
         )

        (any "\\\"profession politique\\\"" "misc.profession_politique")
        ;; Microsoft
        (from ".*msdn\\(contents\\|status\\)@.*" "microsoft.msdn")

        (any ".*@.*microsoft\\.com"
             (| ("subject" "msdn" "microsoft.msdn")
                (any "laurent.ellerbach" "supelec.microsoft")
                (any "thomas.lucchini" "supelec.microsoft")
                (any "thomlucc" "supelec.microsoft")
                (from "fsharp@.*microsoft\\.com" "lisp.fsharp")
                "microsoft.misc"))
        (| (from "marie-laure.noiville" "perso.amis")
           (any "inno@xwiki.org" "perso.amis"))

        ;; Lusis
        (any "alain.rousseau" "supelec.lusis")

        ;; Rational Software
        (any "hypernews@rational\\.com" "prog.rose-cafe")
        (| (any ".*@rational\\.com" "prog.rational")
           (from "Rational Software France" "prog.rational")
           (from "flash-fr@rational.com" "prog.rational"))

        (from "copainsdavant@.*\\.linternaute.com" "perso.copainsdavant")

        (from ".*@reply.exacttarget.com" "misc.sciam")

        (any ".*@\\(ibazar\\|ibazar-group\\|ebay\\)\\.\\(fr\\|com\\)" "misc.ebay")

        (any "mailing.supelec@wanadoo.fr" "supelec.amicale")

        (from ".*@springeronline.com" "misc.springer")
        (from ".*@folli.org" "perso.conferences")

        (from ,fp-split-people-jadfs "perso.jadfs")

        (| (any "root@.*metz\\.supelec\\.fr"
                (| ("subject" "mirror" "systeme.mirror")
                   ("subject" "virus" "systeme.virus")
                   (any "\\(delivery\\|mail\\)" "systeme.mail")
                   "systeme.misc"
                   )
                )
           (any ".*ftp\\(.*admin\\)?@.*ese-metz\\.fr" "systeme.ftp")
           (from "popineau@.*dante.de" ("subject" "mirror" "systeme.mirror"))
           )
        (| ("subject" "\\(un\\)?subscribe" "systeme.listes")
           (from "list.?serv\\(er\\)" "systeme.listes")
           (from "mailman-owner@.*" "systeme.listes")
           (from "majordomo@.*" "systeme.listes")
           (from ".*-request@.*"
                 (to "fabrice\\.popineau@supelec\\.fr" "systeme.listes"))
           ("subject" "output of your job" "systeme.listes")
           ;; Message from daemons, postmaster, and the like to another.
           (any mail "systeme.mail")
           )

        ;; Non-error messages are crossposted to all relevant
        ;; groups, but we don't crosspost between the group for the
        ;; (ding) list and the group for other (ding) related mail.

        ;; Système
        (| (from "certsvp@renater\\.fr" "systeme.cert")
           (any "rssi.*@.*\\(cru\\|education\\|gouv\\).*\\.fr" "systeme.cert")
           ("subject" "rssi" "systeme.cert")
           )

        (any "\\(.*@fwntug\\.org\\|win32@esf\\.org\\)" "systeme.fwntug")

        ;; Emacs
        (any "auc-tex.*@.*" "emacs.auc-tex")
        (any "ntemacs.*@cs\\.washington\\.edu" "emacs.ntemacs")
        (any "ding-.*@\\(ifi\\.uio\\.no\\|lists\\.math\\.uh\\.edu\\|gnus\\.org\\)" "emacs.gnus")

        (any "ilisp.*@.*\\(cons\\.org\\|naggum\\.no\\|csi\\.com\\|lehman\\.com\\|bu\\.edu\\|sourceforge\\.net\\)" "emacs.ilisp")
        (any ".*@.*xemacs\\.org"
             (|
              (any "xemacs-announce@.*" "emacs.xemacs-announce")
              (any "xemacs-\\(nt\\|winnt\\)@.*" "emacs.xemacs-nt")
              (any "xemacs-design.*@.*" "emacs.xemacs-design" )
              (any "xemacs-beta.*@.*" "emacs.xemacs-beta")
              (any "xemacs-mule@.*" "emacs.xemacs-mule")
              (any "xemacs-patches@.*" "emacs.xemacs-patches")
              (any "xemacs-review@.*" "emacs.xemacs-review")
              (any "xemacs-bug@.*" "emacs.xemacs-bug")
              (any "crashes@.*" "emacs.xemacs-bug")
              )
             "emacs.xemacs-misc"
             )
        ("subject" "xemacs" (any "popineau@.*\\.fr" "emacs.xemacs-misc"))

        ;; Lisp
        (any "www-cl@ai\\.mit\\.edu" "lisp.cl-http")
;            (|
        (to "clisp-list@.*\\(sourceforge\\.net\\|cons\\.org\\|ma2s2\\.mathematik\\.uni-karlsruhe\\.de\\)" "lisp.clisp")
        (to "clisp-announce@.*sourceforge\\.net" "lisp.clisp-announce")
        (to "clisp-\\(devel\\|cvs\\)@.*sourceforge\\.net" "lisp.clisp-devel")
        ("subject" "clisp-\\(bugs\\|patches\\)-[0-9]+ \\].*" "lisp.clisp-bugs")
;             "lisp.clisp"
;             )
        (any "lisp-hug@.*" "lisp.lisp-hug")
        (any "lispweb.*@.*red-bean\\.com" "lisp.lisp-web")
        (any "ll[1]?-discuss@.*\\.mit\\.edu" "lisp.ll1-discuss")
        (| (from ".*@.*\\(xanalys\\|harlequin\\)\\.co.*"
                 (from ".*\\(lisp-sales\\|mach\\|bovy\\|bloomer\\).*@.*" "lisp.xanalys"))
           (any "lispworks" "lisp.xanalys"))
        (| (from ".*@franz\\.com"
                 (from "\\(license.*\\|fettner\\|avila\\|foderaro\\|bloomer\\|cichon\\|ginty\\)" "lisp.franz"))
           (any "allegro" "lisp.franz")
           (from "Franz Inc" "lisp.franz"))
        (from "\\(ray\\)?.?de.?la.?caze.*@.*\\(franz\\|hotmail\\)\\.com" "lisp.misc")
        ("subject" "slime-devel" "lisp.slime-devel")

        ;; Other mailing lists...
        (any "smlnet-users@.*\\.uk" "lisp.smlnet")
        (any "THEORYNT.*@.*" "theorie.theorynt")
        (any "www-lib@w3\\.org" "prog.www-lib")
        (any "caml.*@.*inria\\.fr" "lisp.caml")
        (any "isabelle-users@.*" "theorie.isabelle")
        (any "ln@.*\\(cnusc\\.fr\\|cines.fr\\)" "theorie.lnat")
        (| (any "sffranco.*@.*cyberus\\.ca" "misc.sf-franco")
           ("subject" "SFF:" "misc.sf-franco"))
        ;; TeX
        (any "LATEX-2E.*@.*" "tex.latex2e")
        (any "TEX-ED.*@.*" "tex.tex-ed")
        (any "TEX-EURO.*@.*" "tex.tex-euro")
        (any "ctan-ann.*@.*" "tex.ctan-ann")
        (any "latex-hacks" "tex.latex-hacks")
        (any "eurotex2005@gutenberg.eu.org" "tex.eurotex2005")
        (any "eurotex2005-board@gutenberg.eu.org" "tex.eurotex2005-board")
        (any "eurotex2005-program@gutenberg.eu.org" "tex.eurotex2005-program")
        (any "TEI-L@.*" "tex.tei")
        (| (any "tug.*20\\([01][0-9]\\).*@tug\\.org" "tex.tug20\\1")
           ("subject" "tug.?20\\([01][0-9]\\)" "tex.tug20\\1"))
        (any "fptex@tug\\.org" "tex.fptex")
        (any "texk-win32.*@.*ese-metz\\.fr" "tex.fptex")
        (| (any "cagut@\\(ens\\.fr\\|gutenberg\\.eu\\.org\\)" "tex.cagut")
           ("subject" "\\(cagut\\|ca du\\)" "tex.cagut"))
        (| (any "gut2001@ens\\.fr" "tex.gut2001")
           ("subject" "gut.?\\(20\\)?01" "tex.gut2001"))
        (any "Formulaire GUT 2000.*@ens\\.fr" "tex.gut2001.enregistrement")
        (any "gut@ens\\.fr" "tex.gut")
        (| (any "latex2html@tug.org" "tex.latex2html")
           ("subject" "l2h" "tex.latex2html"))
        (| (any "omega@ens\\.fr" "tex.omega")
           ("subject" "omega" "tex.omega"))
        (| (any "support@tug\\.org" "tex.texhax")
           ("subject" "tug-support" "tex.texhax"))
        (any "\\(ctan-dists@dante\\.de\\|ctan-n-dists@npc\\.de\\)" "tex.ctan.dists")
        (any "tetex@\\(informatik\\|dbs\\)\\.uni-hannover\\.de" "tex.tetex")
        (any "tetex-pretest@\\(informatik\\|dbs\\)\\.uni-hannover\\.de" "tex.tetex.pretest")
        (any "tex-?live@tug\\.org" "tex.texlive")
        (any "texhax@tug\\.org" "tex.texhax")
        (any "tex-k@.*\\(tug\\.org\\|umb\\.edu\\)" "tex.tex-k")
        (any "tex-pretest@.*\\(tug\\.org\\|umb\\.edu\\)" "tex.tex-k.pretest")
        (any "tldoc@.*tug\\.org" "tex.tldoc")
        (any "tex-implementors@.*" "tex.implementors")
        (any "tpm@othersideofthe.earth\\.li" "tex.packaging")
        (any "texd@tug\\.org" "tex.tex-daemon")
        (any "pdftex@\\(tug\\.org\\|tug\\.cs\\.umb\\.edu\\)" "tex.pdftex")
        (any "aleph@.*\\.nl" "tex.aleph")
        (any "\\(ntg-pdftex@.*\\.nl\\|noreply@sarovar\\.org\\)" "tex.pdftex-devel")
        (any "ntg-context@.*\\.nl" "tex.context")
        (any "metafont@ens\\.fr" "tex.metafont")
        (| (any "typographie@irisa\\.fr" "tex.typographie")
           ("reply-to" "typographie@irisa\\.fr" "tex.typographie"))
        (any "wingut@gutenberg\\.eu\\.org" "tex.wingut")
        (any "twg-tds@tug\\.org" "tex.twg-tds")
        (any "wxpython-users@l.*\\.wxwidgets\\.org" "lisp.wxpython")
        (any "texlib@yahoogroups\\.com" "tex.texlib")
        (any "tex-fonts@math\\.utah\\.edu" "tex.tex-fonts")
        (|
         (any "\\(NTS-L@.*uni-heidelberg\\.de\\|NTS-IMPLEMENTATION-LANGUAGE@.*rhbnc\\.ac\\.uk\\)" "tex.nts")
         ("subject" "\\bnts\\b" "tex.nts"))
        (any "\\(mailman-.*@tug\\.org\\|owner-fptex\\|fptex-admin@tug\\.org\\)" "tex.fptex-admin")

        (from ,fp-split-people-tex-personal "tex.misc")
        ("subject" ,fp-split-subjects-tex-personal "tex.misc")

        ;; Personal
        (any ,fp-split-people-friends-personal "perso.amis")

        (any ".*@\\(banquedirecte\\|axa.?banque\\)\\.fr" "perso.banque-directe")
        (any ".*\\(@.*cablewanadoo\\.com\\|ftci@.*wanadoo\\.fr\\|service.clients.*@.*wanadoo\\.fr\\)" "perso.wanadoo")
        (any ".*@gandi\\.net" "perso.gandi")

        (to ".*@lsm\\.abul\\.org" "perso.lsm2003")
        ("Subject" "\\(\\[LSM\\|LSM.?2003\\|RMLL\\)" "perso.lsm2003")

        ;; Supelec

        (from "ensam\\.fr"
              (|
               (from ".*@metz" "supelec.ensam")
               (to "info-math@" "supelec.ensam")
               (to "popineau@supelec\\.fr" "supelec.ensam"))
              )

        (|
         (from ,fp-split-supelec-metz-staff-regexp "supelec.metz-staff")
         (from ,fp-split-supelec-metz-tech-regexp "supelec.metz-tech")
         (from ,fp-split-supelec-metz-regexp "supelec.metz")
         (from ,fp-split-supelec-supelec-regexp "supelec.supelec")
         (from ,fp-split-supelec-supelec-staff-regexp "supelec.supelec-staff")
         (to "tous@supelec\\.fr" "supelec.supelec")
         (to "\\(info-\\|auto-\\|elec-\\|inge\\|perso\\|tous\\|all\\)@.*ese-metz\\.fr" "supelec.metz")
         (to "popineau@.*\\.fr"
             ("from" ".*@.*\\(ese-metz\\|metz.supelec\\)\\.fr" "supelec.etudiants-2004"))
         (to "\\(info-\\|auto-\\|elec-\\|inge\\|perso\\|tous\\|all\\)@.*supelec\\.fr" "supelec.supelec")
         ("\\(to\\|cc\\)" "popineau@.*\\.fr"
          ("from" ".*@.*\\(metz.supelec\\|ese-metz\\).fr" "supelec.metz"))
         )

        ;; People...
        ;; (any "larsi@ifi\\.uio\\.no" "people.Lars_Magne_Ingebrigtsen")
        (from "\\(popineau@rocketmail\\.com\\|popineau@club-internet\\.fr\\|popineau@.*hutchinson.fr\\|sebastien.popineau@free.fr\\)" "perso.sebastien")
        (from "\\(joelle\\.popineau@\\(wanadoo\\|free\\)\\.fr\\|popineau@.*\\.univ-metz\\.fr\\)" "perso.joelle")
        (from "popineau@ifrance\\.com" "perso.parents")
        (from ".*\\(battyani@\\|@battyani\\).*" "perso.marc")
        (from ".*pannerec.*" "perso.roger")
        (from "nobine@freesurf\\.fr" "perso.bruno")
        (from "thierrymadec@wanadoo\\.fr" "perso.amis")

        ;; Both lists below have the same suffix, so prevent
        ;; cross-posting to mkpkg.list of messages posted only to
        ;; the bugs- list, but allow cross-posting when the
        ;; message was really cross-posted.
        ;; (any "bugs-mypackage@somewhere" "mypkg.bugs")
        ;; (any "mypackage@somewhere" - "bugs-mypackage" "mypkg.list")

;       (to "Fabrice\\.Popineau@supelec\\.fr"
;           (| ("subject" "\\(commande\\|your.*order\\)" "perso.commandes")
;              ("subject" "re:.*" "perso.misc")
;              ("references" ".*@.*" "perso.misc")
;              "systeme.spam"))

;       (to "fabrice\\.popineau@esemetz\\.ese-metz\\.fr"
;           (| ("subject" "re:.*" "perso.misc")
;              ("references" ".*@.*" "perso.misc")
;              "systeme.spam"))

;       "systeme.spam"
        (to "Fabrice\\.Popineau@supelec\\.fr"
            (| ("subject" "\\(commande\\|your.*order\\)" "perso.commandes")
               "perso.misc"))
        "perso.misc"))

;
; Test Fancy Split
;
(defun fp-test-fancy-split ()
  (let (                       ;(nnmail-split-fancy '(...your customization...))
        (nnmail-split-fancy-match-partial-words nil))
    (with-temp-buffer
      (insert "\
 From: \"Austin\" <austin@maxtronic.com.tw>\
 Subject: [wxPython-users] A problem about wxListCtrl\
 To: <wxpython-users@lists.wxwidgets.org>\
 Date: Tue, 09 Nov 2004 08:47:06 +0100\
 Mailing-List: contact wxPython-users-help@lists.wxwidgets.org; run by ezmlm\
 List-Post: <mailto:wxPython-users@lists.wxwidgets.org>\
 List-Help: <mailto:wxPython-users-help@lists.wxwidgets.org>\
 List-Unsubscribe: <mailto:wxPython-users-unsubscribe@lists.wxwidgets.org>\
 List-Subscribe: <mailto:wxPython-users-subscribe@lists.wxwidgets.org>\
 Reply-To: wxPython-users@lists.wxwidgets.org\
 Delivered-To: mailing list wxPython-users@lists.wxwidgets.org\
 X-Sent: 4 minutes, 6 seconds ago\
 X-Mailer: Microsoft Office Outlook, Build 11.0.5510\
 Thread-Index: AcTGLRG1Lxu5B3jqSwyI0olCe8Ki0g==\
 X-Spam-Hits: 0.1\
 X-Spam-Hits: 0.1\
 X-Spam-Checker-Version: SpamAssassin 2.63 (2004-01-11) on  esemetz.metz.supelec.fr\
 X-Spam-Status: No, hits=1.1 required=5.0 tests=HTML_90_100,HTML_MESSAGE  autolearn=no version=2.63\
 X-Spam-Level: *\
 X-Content-Length: 2915\
 X-Spambayes-Classification: ham; 0.07\
 ")
      (nnmail-split-fancy))))

(setq gnus-registry-max-entries 2500
      gnus-registry-track-extra '(sender subject recipient))

(gnus-registry-initialize)



(provide 'fp-gnus)
