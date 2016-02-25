;;; fp-qiqaa.el --- Emacs Prelude: Fabrice Popineau Qiqaa configuration.
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
(require 'sqlite-dump)
(require 'json)
(require 'org-bibtex)
(require 'ox-bibtex)

(defvar *fp-qiqqa-metadata-regex* "VALUES('\\([^']*\\)','metadata','\\([^']*\\)',X'\\([^']*\\)',[^;]*);$")

(defvar *fp-qiqaa-guest-library-file*
  (expand-file-name
   (concat (getenv "LOCALAPPDATA")
           "/Quantisle/Qiqqa/Guest/Qiqqa.library"))
  )

(defvar *fp-qiqaa-org-bibliography-file*
  (expand-file-name "~/Papers/Qiqqa.org")
  )

(bibtex-set-dialect 'BibTeX)

(defun decode-hex-string (hex-string)
  (apply #'concat
         (loop for i from 0 to (- (/ (length hex-string) 2) 1)
            for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
            collect (format "%c" (string-to-number hex-byte 16)))))

(defun encode-hex-string (string)
  )

(defun fp-read-datetime (s)
  (unless (string-match  "\\([0-9]\\{4\\}\\)\\(0[1-9]\\|1[0-2]\\)\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)\\([0-1][0-9]\\|2[0-4]\\)\\([0-5][0-9]\\)\\([0-5][0-9]\\)\\([0-9]\\{3\\}\\)" s)
    (message "error"))
  (let ((seconds (string-to-number (match-string-no-properties 6 s)))
        (minutes (string-to-number (match-string-no-properties 5 s)))
        (hour    (string-to-number (match-string-no-properties 4 s)))
        (day     (string-to-number (match-string-no-properties 3 s)))
        (month   (string-to-number (match-string-no-properties 2 s)))
        (year    (string-to-number (match-string-no-properties 1 s))))
    (encode-time seconds minutes hour day month year)))

;;
;; Transform this into an update.
;; The central org file is the reference.
;; Keep a timestamp for each bibref in the org file.
;; Get the last modified time from each Qiqqa bibref
;; and compare it to the timestamp in the org file.
;; If it is more recent and the bibref alredy exists,
;; then update it.
;;
(defun fp-qiqqa2org (qiqqa-file org-file)
  (let ((org-buffer (find-file-noselect org-file t)))
    (with-current-buffer org-buffer
      (erase-buffer))
    (with-temp-buffer
        (insert-file-contents-literally qiqqa-file)
      (sqlite-dump)
      (goto-char 0)
      (while (re-search-forward *fp-qiqqa-metadata-regex* nil t)
        (let* ((id (match-string 1))
               (json-object-type 'plist)
               (metadata (json-read-from-string (decode-hex-string (match-string 3))))
               (location (plist-get metadata :DownloadLocation))
               (note (plist-get metadata :Comments))
               ; (annote (plist-get metadata :Annotations))
               (key (plist-get metadata :Fingerprint))
               (modtime (plist-get metadata :DateLastModified))
               )
          ;; Read current bibtex entry
          (with-temp-buffer
            (setq org-bibtex-entries nil)
            (insert  (plist-get metadata :BibTex))
            (goto-char 0)
            (while (re-search-forward "\n," nil t) (replace-match ",\n" nil nil))
            (goto-char 0)
            (org-bibtex-read))
          ;; Add useful fields
          (when (and org-bibtex-entries (car org-bibtex-entries))
            (when location
                                        ;(message "location: %s\n" location)
              (push (cons :url (funcall (or (and (fboundp 'fp-filter-location) #'fp-filter-location) 'identity) location)) (car org-bibtex-entries)))
            (when key
              (push (cons :key key) (car  org-bibtex-entries)))
            (when note
              (push (cons :note note) (car org-bibtex-entries)))
                                        ; (when annote
                                        ;  (org-bibtex-put "ANNOTE" annote))
                                        ;              (message (format "%s\n" org-bibtex-entries))

          (with-current-buffer org-buffer

;            (message (format "%s\n" org-bibtex-entries))
              (goto-char (point-max))
              (org-bibtex-write)
                                        ;              (org-insert-time-stamp modtime t)
              (org-entry-put nil "MODTIME" (format-time-string (cdr org-time-stamp-formats) (fp-read-datetime modtime)))
              (goto-char (point-max))
              )))
        ))))

(defun fp-qiqaa-insert ()

  )
(defun fp-filter-location (loc)
  (replace-regexp-in-string "/Projets/" "/Research/" (expand-file-name loc)))

(defun fp-update-from-qiqqa ()
  (interactive)
  (fp-qiqqa2org *fp-qiqaa-guest-library-file* *fp-qiqaa-org-bibliography-file*))

(defun org2qiqaa (org-file qiqqa-file)

  )

;;; Google Scholar and bibretrieve
;;; https://github.com/venthur/gscholar
;;; https://github.com/pzorin/bibretrieve
(require 'bibretrieve)

;;; DONE: Modify this call for start-process
;;; and get the result from the process buffer

;;; GScholar.py is detected by Google and banned.
;;; Time lost.

(defun bibretrieve-scholar-create-url (author title)

  (let ((tempfile (make-temp-file "scholar" nil ".bib")))

    (call-process-shell-command "/gscholar/gscholar.py --all" nil nil nil
                (if (> (length author) 0) (concat "\"" author "\""))
                (if (> (length title) 0)  (concat "\"" title "\""))
                (concat " > " tempfile))
    (concat "file://" tempfile)
))

(defun bibretrieve-scholar ()
  (interactive)
  (setq mm-url-use-external t)
  (setq bibretrieve-backends '(("scholar" . 5)))
  (bibretrieve)
  (setq mm-url-use-external nil)
)

(defun bibretrieve-amazon-create-url (author title)
  (concat "http://lead.to/amazon/en/?key="(mm-url-form-encode-xwfu title) "&si=ble&op=bt&bn=&so=sa&ht=us"))

(defun bibretrieve-amazon ()
  (interactive)
  (setq mm-url-use-external t)
  (setq mm-url-program "w3m")
  (setq mm-url-arguments (list "-dump"))
  (setq bibretrieve-backends '(("amazon" . 5)))
  (bibretrieve)
  (setq mm-url-use-external nil)
)
