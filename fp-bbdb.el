(setq bbdb-file "~/elisp/config/bbdb")

(require 'bbdb)
(require 'bbdb-loaddefs)
;; (require 'bbdb-hooks)
(require 'bbdb-com)
(require 'bbdb-anniv)
(require 'bbdb-gnus)
;; (require 'moy-bbdb)

(setq bbdb-pop-up-window-size 5)

(setq bbdb-update-records-p 'create)
(bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-mua-pop-up nil)

(bbdb-insinuate-message)
(bbdb-initialize 'message 'gnus)

(setq bbdb-allow-duplicates t)

;; (autoload 'bbdb/send-hook "moy-bbdb"
;;   "Function to be added to `message-send-hook' to notice records
;;   when sending messages" t)

;; (add-hook 'message-send-hook 'bbdb/send-hook)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases) Not in bbdb3?
(add-hook 'bbdb-change-hook 'bbdb-timestamp)
(add-hook 'bbdb-create-hook 'bbdb-creation-date)
(add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
(autoload 'bbdb-insinuate-gnus "bbdb-insinuate-gnus" "BBDB Gnus" t)
;; (add-hook 'list-diary-entries-hook 'bbdb-include-anniversaries)

(setq bbdb-always-add-addresses t
      bbdb-complete-name-allow-cycling t
      bbdb-completion-display-record t
      bbdb-default-area-code nil
      bbdb-dwim-net-address-allow-redundancy t
      bbdb-electric-p nil
      bbdb-new-nets-always-primary 'never
      bbdb-north-american-phone-numbers-p nil
      bbdb-offer-save 'auto
      bbdb-pop-up-target-lines 3
      bbdb-print-net 'primary
      bbdb-print-require t
      bbdb-use-pop-up nil
      bbdb-user-mail-names gnus-ignored-from-addresses
      bbdb/gnus-split-crosspost-default nil
      bbdb/gnus-split-default-group nil
      bbdb/gnus-split-myaddr-regexp gnus-ignored-from-addresses
      bbdb/gnus-split-nomatch-function nil
      bbdb/gnus-summary-known-poster-mark "+"
      bbdb/gnus-summary-mark-known-posters t)

(defalias 'bbdb-y-or-n-p '(lambda (prompt) t))

;; only set this when bbdb-user-mail-names is set
;; (setq bbdb-ignore-most-messages-alist
;;       `(("To" . ,bbdb-user-mail-names)))

;; FIXME: ignore hook est cassé
;; (setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook)
;; (setq bbdb/news-auto-create-p 'bbdb-ignore-most-messages-hook)

(setq bbdb-auto-notes-alist
      `(("Newsgroups" ("[^,]+" newsgroups 0))
        ("Subject" (".*" last-subj 0 t))
        ("User-Agent" (".*" mailer 0))
        ("X-Mailer" (".*" mailer 0))
        ("Organization" (".*" organization 0))
        ("X-Newsreader" (".*" mailer 0))
        ("X-Face" (".+" face 0 'replace))
        ("Face" (".+" face 0 'replace))))

(add-hook 'bbdb-list-hook 'my-bbdb-display-xface)

(defun my-bbdb-display-xface ()
  "Search for face properties and display the faces."
  (when (or (gnus-image-type-available-p 'xface)
            (gnus-image-type-available-p 'pbm))
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (default-enable-multibyte-characters nil)
            pbm faces)
        (while (re-search-forward "^           face: \\(.*\\)" nil t)
          (setq faces (match-string 1))
          (replace-match "" t t nil 1)
          (dolist (data (split-string faces ", "))
            (condition-case nil
                (insert-image (create-image (gnus-convert-face-to-png data) nil t))
              (error
               (insert-image (gnus-create-image (uncompface data) nil t :face 'tooltip))))
            (insert " ")))))))

;; (setq bbdb-quiet-about-name-mismatches 0)

;; (setq bbdb-message-caching-enabled nil)

(provide 'fp-bbdb)
