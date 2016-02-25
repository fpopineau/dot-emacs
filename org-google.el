;; Copyright (c) 2015 Fabrice Popineau
;;
;; Author: Fabrice Popineau
;; Created: April 2015
;; Version: 0.1
;; Keywords: calendar, tasks, org, google
;;
;; Google tasks for Org  is free  software; you  can redistribute  it and/or
;; modify it under the  terms of the GNU General Public  License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; Google tasks for  Org is distributed in  the hope that it  will be useful,
;; but   WITHOUT  ANY   WARRANTY;   without  even   the   implied  warranty   of
;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR PURPOSE.   See the  GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Options
;;   customization is found in the groupe 'google-tasks'
;;   M-x customize-group google-tasks

(require 'google-oauth)
(require 'json)
(require 'eieio)
(require 'url-cache)

;; (defgroup google-tasks nil "Org sync with Google Tasks")

;; My developper keys
(defconst google/oauth-client-id ""
  "Client ID for OAuth.")

(defconst google/oauth-client-secret ""
  "Google secret key. Please don't tell anyone.
I AM SERIOUS!")

(defconst google/key-url (concat "?key=" google/oauth-client-secret
                                 "&grant_type=authorization_code"
                                 ))

(defconst google/expire-time 5)

(defun google/oauth-token ()
  "Get OAuth token to access Google APIs."
  (google-oauth-auth-and-store
   google/resource-url
   google/oauth-client-id
   google/oauth-client-secret))

;; FIXME: introduce the verb into the parameters
(defun google/url-execute (command url &optional token data)
  "Send HTTP request at URL using COMMAND with DATA.
Return the server answer buffer"
  (let* ((url-request-method command)
         (url-request-data (when data (encode-coding-string data 'utf-8)))
         (url-request-extra-headers (when data (list '("Content-Type" . "application/json"))))
         ;; (url-debug t)
         (buf (if data
                  (oauth2-url-retrieve-synchronously (or token
                                                         (google/oauth-token))
                                                     url
                                                     url-request-method
                                                     url-request-data
                                                     url-request-extra-headers)
                (oauth2-url-retrieve-synchronously (or token
                                                       (google/oauth-token))
                                                   url
                                                   url-request-method

                                                   ))))
    (message "%s\n%s\n%s\n%s" url url-request-method url-request-data url-request-extra-headers)
    (message "%s" (with-current-buffer buf (buffer-string)))
    buf))

;; (defun google/url-put (url &optional data token)
;;   "Send HTTP request at URL using PUT with DATA.
;; Return the server answer buffer"
;;   (let* ((url-request-method "PUT")
;;          (url-request-data (encode-coding-string data 'utf-8))
;;          (url-request-extra-headers '(("Content-Type" . "application/json")))
;;          ; (url-debug t)
;;          (buf (oauth2-url-retrieve-synchronously (or token
;;                                                      (google/oauth-token))
;;                                                  url
;;                                                  url-request-method
;;                                                  url-request-data
;;                                                  url-request-extra-headers)))
;;     buf))

;; (defun google/url-patch (url &optional data token)
;;   "Send HTTP request at URL using PATCH with DATA.
;; Return the server answer buffer"
;;   (let* ((url-request-method "PATCH")
;;          (url-request-data data)
;;          (url-request-extra-headers '(("Content-Type" . "application/json")))
;;          ; (url-debug t)
;;          (buf (oauth2-url-retrieve-synchronously (or token
;;                                                      (google/oauth-token))
;;                                                  url
;;                                                  url-request-method
;;                                                  url-request-data
;;                                                  url-request-extra-headers)))
;;     buf))

;; (defun google/url-retrieve (url &optional token)
;;   "Retrieve URL using cache if possible."
;;   (let ((url-cache-expire-time google/expire-time))
;;     (if (url-cache-expired url)
;;         (let ((buf (oauth2-url-retrieve-synchronously (or token
;;                                                           (google/oauth-token))
;;                                                       url)))
;;           ;; This is `url-store-in-cache' modified so it uses
;;           ;; `google-contacts-resource-url' to store the cache file as the
;;           ;; current URL, rathen than the URL with the access token.
;;           (with-current-buffer buf
;;             (let ((fname (url-cache-create-filename url)))
;;               (if (url-cache-prepare fname)
;;                   (let ((coding-system-for-write 'binary))
;;                     (write-region (point-min) (point-max) fname nil 5)))))
;;           buf)
;;       (url-fetch-from-cache url))))

(defun google/http-data (buffer)
  "Return HTTP data from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((headers (buffer-substring (point-min) (point))))
      (if (string-match-p "^Content-Type:.* charset=UTF-8" headers)
          (set-buffer-multibyte t)
        (set-buffer-multibyte nil))
      (if (string-match-p "^HTTP/1.1 200 OK" headers)
          (let ((data (buffer-substring (point) (point-max))))
            (kill-buffer)
            data)
        (not (kill-buffer))))))

(defun google/http-plist (buffer)
  "Parse the JSON data from buffer to a plist"
  (let ((json-object-type 'plist)
        (data (google/http-data buffer)))
    (when data (json-read-from-string data))))

(defun google/url-data (url &optional token)
  "Return HTTP data from URL, using cache if possible"
  (google/http-data (google/url-execute "GET" url token)))

(defun google/url-plist (url &optional token)
  "Return plist from URL, using cache if possible"
  (google/http-plist (google/url-execute "GET" url token)))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun google/get-resource-url (api)
  (getf (symbol-value (intern (concat "google-" (symbol-name api) "/apis")))
        :resource-url))

(defun google/get-api-command (api group command)
  (let* ((api-desc (symbol-value (intern (concat "google-" (symbol-name api) "/apis")))))
    (substring (symbol-name (second (assoc command (getf api-desc group)))) 1)))

(defun google/get-api-url (api group command &optional substitutions)
  (let* ((api-desc (symbol-value (intern (concat "google-" (symbol-name api) "/apis"))) )
         (url (concat (getf api-desc :baseuri)
                      (third (assoc command (getf api-desc group)))
                      google/key-url
                      )))
    (when substitutions
      (loop for (from to) in substitutions
            do (setq url (apply #'replace-in-string from to (list url)))))
    url)
  )

;; * Calendar

(defgroup google-calendar nil "Org sync with Google Calendar")

(defcustom google-calendar/expire-time 3600
  "Time in seconds to keep entries in cache"
  :group 'google-calendar
  :type 'integer)

(defcustom  google-calendar/calendars-files "~/Org/google-calendars.org"
  "Org formated file created to reflect google calendars properties"
  :group 'google-calendar
  :type 'string)

(defcustom google-calendar/events-order-by "startTime"
  "Order events by start time or date of update"
  :group 'google-calendar
  :type '(choice (const :tag "Start" "startTime")
                 (const :tag "Updated" "update")))

(defcustom google-calendar/up-days 30
  "Number of days to get events before today"
  :group 'google-calendar
  :type 'integer)

(defcustom google-calendar/down-days 90
  "Number of days to get events after today"
  :group 'google-calendar
  :type 'integer)

(defcustom google-calendar/diary-file nil
  "Diary file to store Google Calendar events."
  :group 'google-calendar
  :type '(file :tag "Diary Org file")
  )

(defcustom google-calendar/tag-associations nil
  "list of association '(calendar-id tag) to synchronize at once

for calendar id, look in your calendars-file after a call to get-calendar"
  :group 'google-calendar
  :type '(repeat (list :tag "Calendar and file" (string :tag "Calendar Id") (file :tag "Org tag"))))

;; (defun google-calendar/normalize-tz (date)
;;   (if
;;    (string-match "\\([^T]+T[^+-]+[+-]\\)\\([0-9]+\\):\\([0-9]+\\)" date)
;;    (let* ((prefix (match-string 1 date))
;;           (hours (match-string 2 date))
;;           (minutes (match-string 3 date)))
;;      (concat prefix hours minutes))
;;    date))

;; (defun google-calendar/decode (date)
;;   (when (and (stringp date) (not (string= date "")))
;;     (format-time-string (org-time-stamp-format t) (date-to-time (google-calendar/normalize-tz date)))))

;; (defun google-calendar/decode-short (date)
;;   (when (and (stringp date) (not (string= date ""))
;;              (string-match "\\([1-9][0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" date))
;;     (let ((year (string-to-number (match-string 1 date)))
;;           (month (string-to-number (match-string 2 date)))
;;           (day (string-to-number (match-string 3 date))))
;;       (format-time-string (org-time-stamp-format nil) (encode-time 0 0 0 day month year)))))

(defun google-calendar/relative-time (delta)
  (let* ((now (current-time))
         (delta-time (days-to-time delta))
         (computed-date (time-add now delta-time)))
    (format-time-string "%Y-%m-%dT00:00:00Z" computed-date)))


;; (defun google-calendar/get-start-end (org-date-range)
;;   (let ((start nil)(end nil))
;;   (string-match "\\(<[^>]+?>\\)--\\(<[^>]+>\\)" org-date-range)
;;   (setq start (match-string 1 org-date-range))
;;   (setq end (match-string 2 org-date-range))

;;   (list (org-parse-time-string start) (org-parse-time-string end))))

(defun google-calendar/safe-substring (string from &optional to)
  "Calls the `substring' function safely.
\nNo errors will be returned for out of range values of FROM and
TO.  Instead an empty string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        ""
      (substring string from to))))

(defun google-calendar/alldayp (s e)
  (let ((slst (google-calendar/parse-date s))
        (elst (google-calendar/parse-date e)))
    (and
     (= (length s) 10)
     (= (length e) 10)
     (= (- (time-to-seconds
            (encode-time 0 0 0
                         (plist-get elst :day)
                         (plist-get elst :mon)
                         (plist-get elst :year)))
           (time-to-seconds
            (encode-time 0 0 0
                         (plist-get slst :day)
                         (plist-get slst :mon)
                         (plist-get slst :year)))) 86400))))

(defun google-calendar/parse-date (str)
  (list :year (string-to-number  (google-calendar/safe-substring str 0 4))
        :mon  (string-to-number (google-calendar/safe-substring str 5 7))
        :day  (string-to-number (google-calendar/safe-substring str 8 10))
        :hour (string-to-number (google-calendar/safe-substring str 11 13))
        :min  (string-to-number (google-calendar/safe-substring str 14 16))
        :sec  (string-to-number (google-calendar/safe-substring str 17 19))))

(defun google-calendar/adjust-date (fn day)
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (funcall fn (current-time) (days-to-time day))))

(defun google-calendar/add-time ()
  (google-calendar/adjust-date 'time-add google-calendar/down-days))

(defun google-calendar/subsract-time ()
  (google-calendar/adjust-date 'time-subtract google-calendar/up-days))

(defun google-calendar/format-iso2org (str &optional tz inactive)
  (let ((plst (google-calendar/parse-date str))
        (open (if inactive "[" "<"))
        (close (if inactive "]" ">")))
    (concat
     open
     (format-time-string
      (if (< 11 (length str)) "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
      (seconds-to-time
       (+ (if tz (car (current-time-zone)) 0)
          (google-calendar/time-to-seconds plst))))
     ;;(if (and repeat (not (string= repeat ""))) (concat " " repeat) "")
     close)))

(defun google-calendar/format-org2iso (year mon day &optional hour min tz)
  (concat
   (format-time-string
    (if (or hour min) "%Y-%m-%dT%H:%M" "%Y-%m-%d")
    (seconds-to-time
     (-
      (time-to-seconds
       (encode-time 0
                    (if min min 0)
                    (if hour hour 0)
                    day mon year))
      (if tz
          (car (current-time-zone)) 0))))
   (when (or hour min) ":00z")))

(defun google-calendar/iso-next-day (str &optional previous-p)
  (let ((format (if (< 11 (length str))
                    "%Y-%m-%dT%H:%M"
                  "%Y-%m-%d"))
        (plst (google-calendar/parse-date str))
        (prev (if previous-p -1 +1)))
    (format-time-string format
                        (seconds-to-time
                         (+ (google-calendar/time-to-seconds plst)
                            (* 60 60 24 prev))))))

(defun google-calendar/iso-previous-day (str)
  (google-calendar/iso-next-day str t))

(defun google-calendar/format-date (str format &optional tz)
  (let ((plst (google-calendar/parse-date str)))
    (concat
     (format-time-string format
                         (seconds-to-time
                          (+ (if tz (car (current-time-zone)) 0)
                             (google-calendar/time-to-seconds plst)))))))

(defun google-calendar/param-date (str)
  (if (< 11 (length str)) "dateTime" "date"))

(defun google-calendar/time-to-seconds (plst)
  (time-to-seconds
   (encode-time
    (plist-get plst :sec)
    (plist-get plst :min)
    (plist-get plst :hour)
    (plist-get plst :day)
    (plist-get plst :mon)
    (plist-get plst :year))))

;;; Some documentation
;;; https://developers.google.com/google-apps/calendar/v3/reference/events/update
;;; https://developers.google.com/google-apps/calendar/v3/reference/events/list
;;; FIXME: implement
;;; https://developers.google.com/google-apps/calendar/v3/sync

(defconst google-calendar/apis
      '(:baseuri "https://www.googleapis.com/calendar/v3"
        :resource-url "https://www.googleapis.com/auth/calendar"
        Acl (
             (delete :DELETE "/calendars/(calendarId)/acl/(ruleId)" "Deletes an access control rule.")
             (get :GET "/calendars/(calendarId)/acl/(ruleId)" "Returns an access control rule.")
             (insert :POST "/calendars/(calendarId)/acl" "Creates an access control rule.")
             (list :GET "/calendars/(calendarId)/acl" "Returns the rules in the access control list for the calendar.")
             (patch :PATCH "/calendars/(calendarId)/acl/(ruleId)" "Updates an access control rule. This method supports patch semantics.")
             (update :PUT "/calendars/(calendarId)/acl/(ruleId)" "Updates an access control rule.")
             (watch :POST "/calendars/(calendarId)/acl/watch" "Watch for changes to ACL resources."))

        CalendarList ((delete :DELETE "/users/me/calendarList/(calendarId)" "Deletes an entry on the user 's calendar list.")
                      (get :GET "/users/me/calendarList/(calendarId)" "Returns an entry on the user 's calendar list.")
                      (insert :POST "/users/me/calendarList" "Adds an entry to the user 's calendar list.")
                      (list :GET "/users/me/calendarList" "Returns entries on the user 's calendar list.")
                      (patch :PATCH "/users/me/calendarList/(calendarId)" "Updates an entry on the user 's calendar list. This method supports patch semantics.")
                      (update :PUT "/users/me/calendarList/(calendarId)" "Updates an entry on the user 's calendar list.")
                      (watch :POST "/users/me/calendarList/watch" "Watch for changes to CalendarList resources."))

        Calendars ((clear :POST "/calendars/(calendarId)/clear" "Clears a primary calendar. This operation deletes all events associated with the primary calendar of an account.")
                   (delete :DELETE "/calendars/(calendarId)" "Deletes a secondary calendar. Use calendars.clear for clearing all events on primary calendars.")
                   (get :GET "/calendars/(calendarId)" "Returns metadata for a calendar.")
                   (insert :POST "/calendars" "Creates a secondary calendar.")
                   (patch :PATCH "/calendars/(calendarId)" "Updates metadata for a calendar. This method supports patch semantics.")
                   (update :PUT "/calendars/(calendarId)" "Updates metadata for a calendar."))

        Channels ((stop :POST "/channels/stop" "Stop watching resources through this channel."))
        Colors ((get :GET "/colors" "Returns the color definitions for calendars and events."))
        Events ((delete :DELETE "/calendars/(calendarId)/events/(eventId)" "Deletes an event.")
                (get :GET "/calendars/(calendarId)/events/(eventId)" "Returns an event.")
                (import :POST "/calendars/(calendarId)/events/import" "Imports an event. This operation is used to add a private copy of an existing event to a calendar.")
                (insert :POST "/calendars/(calendarId)/events" "Creates an event.")
                (instances :GET "/calendars/(calendarId)/events/(eventId)/instances" "Returns instances of the specified recurring event.")
                (list :GET "/calendars/(calendarId)/events" "Returns events on the specified calendar.")
                (move :POST "/calendars/(calendarId)/events/(eventId)/move" "Moves an event to another calendar, i.e. changes an event 's organizer."
                  (destination :required))
                (patch :PATCH "/calendars/(calendarId)/events/(eventId)"       "Updates an event. This method supports patch semantics.")
                (quickAdd :POST "/calendars/(calendarId)/events/quickAdd" "Creates an event based on a simple text string."
                  (text :required))
                (update :PUT "/calendars/(calendarId)/events/(eventId)"        "Updates an event.")
                (watch :POST "/calendars/(calendarId)/events/watch" "Watch for changes to Events resources."))
        Freebusy ((query :POST "/freeBusy" "Returns free/busy information for a set of calendars."))
        Settings ((get :GET "/users/me/settings/setting" "Returns a single user setting.")
                  (list :GET "/users/me/settings" "Returns all user settings for the authenticated user.")
                  (watch :POST "/users/me/settings/watch" "Watch for changes to Settings resources.)"))))

(defun google-calendar/calendars-url ()
  (google/get-api-url 'calendar 'CalendarList 'list))

(defun google-calendar/single-calendar-url (calendar-id)
  (google/get-api-url 'calendar 'Calendars 'get
                          `(("(calendarId)" ,calendar-id))))

(defun google-calendar/fetch-calendar (calendar-id)
  (let ((google/resource-url (google/get-resource-url 'calendar)))
    (google/url-plist
     (google-calendar/single-calendar-url calendar-id))))

(defun google-calendar/get-calendar ()
  "Insert in calendars file usefull informations on user calendars"
  (interactive)
  (let* ((google/resource-url (google/get-resource-url 'calendar))
         (calendars (google/url-plist (google-calendar/calendars-url)))
         (calendars-items (plist-get calendars ':items))
         )
    (find-file-other-window google-calendar/calendars-files)
    (erase-buffer)
    (setq calendars-items (append calendars-items nil))
    (setq calendars-items (sort calendars-items 'google-calendar/calendar<))
    (mapcar 'google-calendar/get-calendar-item calendars-items))
    (save-buffer)
    (message (concat "Pull calendar list to " google-calendar/calendars-files)))

(defun google-calendar/get-calendar-item (item)
  "Put usefull information on one calendar into current buffer"
  (let ((id (plist-get item ':id))
        (summary (plist-get item ':summary))
        (description (plist-get item ':description))
        (location (plist-get item ':location))
        (timeZone (plist-get item ':timeZone))
        (hidden (plist-get item ':hidden))
        (selected (plist-get item ':selected))
        (out ""))
    (unless hidden
      (if selected
          (setq out (format google-calendar/calendar-item-format
                            summary id description location timeZone))
        (setq out (format google-calendar/calendar-item-format-unselected
                          summary id description location timeZone))))
    (insert out)
    )
)

(defun google-calendar/calendar< (a b)
  "Sort two calendars, on primary, role and id"
  (let ((a-primary (plist-get a ':primary))
        (b-primary (plist-get b ':primary))
        (a-role (plist-get a ':accessRole))
        (b-role (plist-get b ':accessRole))
        (a-id (plist-get a ':id))
        (b-id (plist-get b ':id)))
    (cond
     ((and a-primary (not b-primary)))
     ((and b-primary (not a-primary)) nil)
     ((and (string= a-role "owner") (not (string= b-role "owner"))))
     ((and (string= b-role "owner") (not (string= a-role "owner"))) nil)
     ((and (string= a-role "writer") (not (string= b-role "writer"))))
     ((and (string= b-role "writer") (not (string= a-role "writer"))) nil)
     ((string< a-id b-id)))))

(defun google-calendar/events-url (calendar-id)
  (google/get-api-url 'calendar 'Events 'list
                          `(("(calendarId)" ,calendar-id))))

(defun google-calendar/fetch-one-page (calendar-id token &optional last-update)
  (let ((google/resource-url (google/get-resource-url 'calendar)))
    (if token
        (google/url-plist (concat (google-calendar/events-url calendar-id) "&pagetoken="token))
      (let* ((full-url
              (concat (google-calendar/events-url calendar-id)
                      "&orderBy="google-calendar/events-order-by
                      "&singleEvents=True"
                      "&timeMin="(google-calendar/relative-time (- google-calendar/up-days))
                      "&timeMax="(google-calendar/relative-time google-calendar/down-days)
                      "&maxResults=2000"
                      (when last-update (concat "&updatedMin=" last-update))))
             (events (google/url-plist full-url)))
        events))))

(defun google-calendar/fetch-events (calendar-id &optional last-update)
  "Fetch all events pages"
  (let* ((events (google-calendar/fetch-one-page calendar-id nil last-update))
         (next (plist-get events ':nextPageToken))
         (items (plist-get events ':items)))
    (setq items (append items nil))
    (while next
      (let* ((n_events (google-calendar/fetch-one-page calendar-id next))
             (n_next (plist-get n_events ':nextPageToken))
             (n_items (plist-get n_events ':items)))
        (setq n_items (append n_items nil))
        (setq items (append items n_items))
        (setq next n_next)))
    items))

(defun google-calendar/get-events (calendar-id tag &optional check-id)
  "Insert in file informations on events from calendar"
  (let* ((events-items (google-calendar/fetch-events calendar-id)))
    (setq events-items (append events-items nil))
    ;; (setq events-items (reverse (sort events-items 'google-calendar/event<)))

    (find-file-other-window google-calendar/diary-file)
    ;; (erase-buffer)

    (mapcar #'(lambda (item) (google-calendar/get-events-item item tag check-id)) events-items)
    )
  (save-buffer)
  (message (concat "pull " calendar-id " into org file " google-calendar/diary-file)))

(defun google-calendar/event< (a b)
  "Sort two events on start time, end time and id"
  (let* ((a-start (plist-get a ':start))
         (a-startTime (plist-get a-start ':dateTime))
         (a-end (plist-get a ':end))
         (a-endTime (plist-get a-end ':dateTime))
         (a-id (plist-get a ':id))
         (b-start (plist-get b ':start))
         (b-startTime (plist-get b-start ':dateTime))
         (b-end (plist-get b ':end))
         (b-endTime (plist-get b-end ':dateTime))
         (b-id (plist-get b ':id)))
    (cond ((string< a-startTime b-startTime))
          ((string< b-startTime a-startTime) nil)
          ((string< a-endTime b-endTime))
          ((string< b-endTime a-endTime) nil)
          ((string< a-id b-id)))))

(defun google-calendar/get-item-property-updated ()
  "Assume you are at heading."
  (when (re-search-forward ":UPDATED:[[:space:]]+\\(\\[[^]]*\\]\\)"
                           (save-excursion (outline-next-heading) (point)) t)
    (goto-char (match-beginning 1))
    (let ((timestamp-obj (org-element-timestamp-parser)))
      (when timestamp-obj
        (google-calendar/format-org2iso
         (plist-get (cadr timestamp-obj) :year-start)
         (plist-get (cadr timestamp-obj) :month-start)
         (plist-get (cadr timestamp-obj) :day-start)
         (plist-get (cadr timestamp-obj) :hour-start)
         (plist-get (cadr timestamp-obj) :minute-start)
         (when (plist-get (cadr timestamp-obj) :hour-start)
           t))))))

(defun google-calendar/read-item-last-update (position)
  "Read the :UPDATED: timestamp item's property.
If you change the item, you can update the timestamp to the current
time to reflect your changes. The item will be updated into your
Google calendar at the next synchronization."
  (let* ((file (car position))
         (pos (cdr position)))
    (with-current-buffer (get-file-buffer file)
      (save-excursion
        (goto-char pos)
        (org-back-to-heading)
        (google-calendar/get-item-property-updated)
  ))))

;; FIXME: parse tag -> calendar
(defun google-calendar/org2item (position)
  (let* ((file (car position))
         (pos (cdr position)))
    (with-current-buffer (get-file-buffer file)
      (save-excursion
        (goto-char pos)
        (org-back-to-heading)
        (let* ((elem (org-element-headline-parser (point-max) t))
               (updated-obj (progn
                              (re-search-forward ":UPDATED:" (save-excursion (outline-next-heading) (point)))
                              (re-search-forward "\\[[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*\\]"
                                                 (save-excursion (outline-next-heading) (point)))
                              (goto-char (match-beginning 0))
                              (org-element-timestamp-parser)))
               (timestamp-obj (progn
                                (re-search-forward ":END:" (save-excursion (outline-next-heading) (point)))
                                (re-search-forward "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                                                        (save-excursion (outline-next-heading) (point)))
                                (goto-char (match-beginning 0))
                                (org-element-timestamp-parser)))
               (summary (org-element-property :title elem))
               (tag (cl-loop for x in (org-element-property :tags elem)
                             when (rassoc (list (substring-no-properties x)) google-calendar/tag-associations)
                             return (substring-no-properties x)))
               (link-obj (org-element-property ':LINK elem))
               (link (when (and link-obj
                                (string-match "\\[\\[\\(.*\\)\\]\\[.*\\]\\]" link-obj))
                       (match-string 1 link-obj)))
               (location  (org-element-property ':LOCATION elem))
               (id  (org-element-property ':ID elem))
               (creator-obj (org-element-property :CREATOR elem))
               (creator-email)
               (creator-name (when (and creator-obj (string-match "\\[\\[email:\\([^]]*\\)\\]\\[\\([^]]*\\)\\]\\]" creator-obj))
                               (setq creator-email (match-string 1 creator-obj))
                               (match-string 2 creator-obj)))
               (allday-p (and (= (plist-get (cadr timestamp-obj) :year-start)
                                 (plist-get (cadr timestamp-obj) :year-end))
                              (= (plist-get (cadr timestamp-obj) :month-start)
                                 (plist-get (cadr timestamp-obj) :month-end))
                              (= (plist-get (cadr timestamp-obj) :day-start)
                                 (plist-get (cadr timestamp-obj) :day-end))
                              (null (plist-get (cadr timestamp-obj) :hour-start))
                              (null (plist-get (cadr timestamp-obj) :minute-start))
                              (null (plist-get (cadr timestamp-obj) :hour-end))
                              (null (plist-get (cadr timestamp-obj) :minute-end))
                                 ))
               (description  (if (plist-get (cadr elem) :contents-begin)
                          (replace-regexp-in-string
                           "<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].*?>\n" ""
                           (replace-regexp-in-string
                            " *:PROPERTIES:\n *\\(.*\\(?:\n.*\\)*?\\) *:END:\n\n" ""
                            (buffer-substring-no-properties
                             (plist-get (cadr elem) :contents-begin)
                             (plist-get (cadr elem) :contents-end)))) ""))
               )
          (message "elem = %s\ntimestamp=%s\nallday-p=%s" elem (cadr timestamp-obj) allday-p)
          `(:id ,id
            :updated ,(google-calendar/format-org2iso
                       (plist-get (cadr updated-obj) :year-start)
                       (plist-get (cadr updated-obj) :month-start)
                       (plist-get (cadr updated-obj) :day-start)
                       (plist-get (cadr updated-obj) :hour-start)
                       (plist-get (cadr updated-obj) :minute-start)
                       (when (plist-get (cadr updated-obj) :hour-start) t))
            :htmlLink ,link
            ; ,@(when cancelled (list :status "cancelled"))
            ,@(when location (list :location location))
            :summary ,summary
            :tag ,tag
            :start ,(if allday-p
                       (list :date (google-calendar/format-org2iso
                                    (plist-get (cadr timestamp-obj) :year-start)
                                    (plist-get (cadr timestamp-obj) :month-start)
                                    (plist-get (cadr timestamp-obj) :day-start)))
                     (list :dateTime (google-calendar/format-org2iso
                                      (plist-get (cadr timestamp-obj) :year-start)
                                      (plist-get (cadr timestamp-obj) :month-start)
                                      (plist-get (cadr timestamp-obj) :day-start)
                                      (plist-get (cadr timestamp-obj) :hour-start)
                                      (plist-get (cadr timestamp-obj) :minute-start)
                                      (when (plist-get (cadr timestamp-obj) :hour-start) t))))
            :end ,(if allday-p
                     (list :date (google-calendar/format-org2iso
                                  (plist-get (cadr timestamp-obj) :year-end)
                                  (plist-get (cadr timestamp-obj) :month-end)
                                  (plist-get (cadr timestamp-obj) :day-end)))
                   (list :dateTime (google-calendar/format-org2iso
                                    (plist-get (cadr timestamp-obj) :year-end)
                                    (plist-get (cadr timestamp-obj) :month-end)
                                    (plist-get (cadr timestamp-obj) :day-end)
                                    (plist-get (cadr timestamp-obj) :hour-end)
                                    (plist-get (cadr timestamp-obj) :minute-end)
                                    (when (plist-get (cadr timestamp-obj) :hour-end) t))))
            ,@(when (or creator-name creator-email)
                `(:creator
                  (,@(when creator-name (list :displayName creator-name))
                      ,@(when creator-email (list :email creator-email)))))
            :description ,description))))))

(defun google-calendar/item-date (item)
  (let* ((stime (plist-get (plist-get item :start) :dateTime))
         (sday (plist-get (plist-get item :start) :date))
         (start (if stime stime sday)))
    (list (plist-get (google-calendar/parse-date start) :mon)
          (plist-get (google-calendar/parse-date start) :day)
          (plist-get (google-calendar/parse-date start) :year)
          )))


;; This helper function has several variables in its context
(defun google-calendar/item2org (item level tag)
  (let* ((id (plist-get item :id))
         ;; (updated-dateTime-decoded
         ;;  (or (and (boundp 'updated-dateTime-decoded) updated-dateTime-decoded)
         ;;      (google-calendar/parse-date (plist-get item :updated))))
         (updated-dateTime-decoded (plist-get item :updated))
         (cancelled (string= (plist-get item ':status) "cancelled"))
         (link (plist-get item ':htmlLink))
         (summary (plist-get item ':summary))
         (description (plist-get item ':description))
         (location (or (plist-get item ':location) "")) ;; FIXME: Unsatisfactory, location shouldn't appear as a property if not available
         (creator (plist-get item ':creator))
         (creator-name (plist-get creator ':displayName))
         (creator-email (plist-get creator ':email))
         (stime (plist-get (plist-get item :start) :dateTime))
         (etime (plist-get (plist-get item :end) :dateTime))
         (sday (plist-get (plist-get item :start) :date))
         (eday (plist-get (plist-get item :end) :date))
         (start (if stime stime sday))
         (end (if etime etime eday))
         (attendees (plist-get item ':attendees))
         (out ""))
    (concat
     (make-string level ?\*)
     " "
     (when cancelled "CANCELLED")
     " "
     summary
     (when tag (format " :%s:" tag))
     "\n"
     ":PROPERTIES:\n"
     ":ID: " id "\n"
     ":UPDATED: " (google-calendar/format-iso2org updated-dateTime-decoded t t) "\n"
     ":LINK: " (format "[[%s][Goto Google Calendar Web page]]" link) "\n"
     (when location ":LOCATION: ") location (when location "\n")
     (when creator (format ":CREATOR: [[email:%s][%s]]\n" creator-email creator-name))
     ":END:\n\n"
     (if (or (string= start end) (google-calendar/alldayp start end))
         (concat "\n  "(google-calendar/format-iso2org start))
       (if (and
            (= (plist-get (google-calendar/parse-date start) :year)
               (plist-get (google-calendar/parse-date end)   :year))
            (= (plist-get (google-calendar/parse-date start) :mon)
               (plist-get (google-calendar/parse-date end)   :mon))
            (= (plist-get (google-calendar/parse-date start) :day)
               (plist-get (google-calendar/parse-date end)   :day)))
           (concat "\n  <"
                   (google-calendar/format-date start "%Y-%m-%d %a %H:%M")
                   "-"
                   (google-calendar/format-date end "%H:%M")
                   ">\n")
         (concat "\n  " (google-calendar/format-iso2org start)
                 "--"
                 (google-calendar/format-iso2org
                  (if (< 11 (length end))
                      end
                    (google-calendar/iso-previous-day end))))))
     "\n"
     description ;; (when desc "\n")
     "\n")))

;; FIXME: can 2 items have the same id if they are copies laying in 2 different calendars? I don't think so.
;; Then it will be hard to merge similar items into a single one with 2 tags.
(defun google-calendar/get-events-item (item tag &optional check-id)
  "Put usefull informations on one event into current buffer.
If `check-id' is t, update items already present in agenda files."
  (let* ((id (plist-get item :id))
         (org-id (org-id-find id))
         (updated-dateTime (plist-get item ':updated))
         (updated-dateTime-decoded (google-calendar/parse-date updated-dateTime))
         )
    (if org-id
        (when check-id
          (let* ((buf (car org-id))
                 (pos (cdr org-id))
                 (local-item-updated-dateTime (google-calendar/read-item-last-update org-id)))
            ;; FIXME: check that update is working
            (cond ((string< local-item-updated-dateTime updated-dateTime)
                   ;; Need to update the fields on Org
                   (with-current-buffer buf
                     (save-excursion
                       (let (level)
                         (goto-char pos)
                         (org-back-to-heading)
                         (setq level (org-currrent-level))
                         (org-mark-subtree)
                         (delete-region (point) (mark))
                         (insert (google-calendar/item2org item level tag))
                         (org-back-to-heading)
                         (org-set-tags nil t)
                         ))))
                  ((string< updated-dateTime local-item-updated-dateTime)
                   ;; Need to update the event on Google Calendar
                   (with-current-buffer buf
                     (save-excursion
                       (google-calendar/post-event-at-point)))))))
      ;; No id, new event
      (with-current-buffer (find-file-noselect google-calendar/diary-file)
        (google-calendar/insert-into-diary item 4 tag)

        ;; (org-reveal t)
        ;; (goto-char (point-max))
        ;; (insert (google-calendar/item2org item 1 tag))
        ;; (org-back-to-heading)
        ;; (org-set-tags nil t)
        ))))

;; FIXME: org-datetree-find-date-create always creates the date entry!
(defun google-calendar/insert-into-diary (item level tag)
  (org-datetree-find-date-create (google-calendar/item-date item))
  (outline-next-heading)
  (org-back-over-empty-lines)
  (unless (looking-at "[ \t]*$") (save-excursion (insert "\n")))
  (let ((col (current-column)))
    (insert (google-calendar/item2org item level tag))
    (org-end-of-meta-data)
    ;; Ensure point is left on a blank line, at proper indentation.
    (unless (bolp) (insert "\n"))
    (unless (org-looking-at-p "^[ \t]*$") (save-excursion (insert "\n")))
    (when org-adapt-indentation (org-indent-to-column col)))
  (org-show-set-visibility 'lineage)
  (org-back-to-heading)
  (org-set-tags nil t)
  )

(defun google-calendar/fetch-calendars (&optional check-id)
  "Fetch calendars into org files as defined into google-calendar/tag-associations"
  (interactive)
  (mapcar (lambda (x) (google-calendar/get-events (nth 0 x) (nth 1 x) check-id))
          google-calendar/tag-associations))

;; FIXME:
(defun google-calendar/post-event-at-point ()
  (interactive)
  (org-back-to-heading)
  (let* ((google/resource-url (google/get-resource-url 'calendar))
         (item (google-calendar/org2item (cons (buffer-file-name) (point))))
         (id (plist-get item :id))
         (calendar-id (car (rassoc (list (plist-get item :tag)) google-calendar/tag-associations)))
         (now (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" nil t))
         (item (plist-put item :updated now)))
    (cl-remf item :tag)
    (cl-remf item :htmlLink)
    (cl-remf item :id)
    ;; (cl-remf item :updated)
    (let ((buf

           (funcall
            #'url-execute
            (if id
                (google/get-api-command 'calendar 'Events 'update)
              (google/get-api-command 'calendar 'Events 'insert))
            (if id
                (google/get-api-url 'calendar 'Events 'update
                                        `(("(calendarId)" ,calendar-id)
                                          ("(eventId)" ,id)))
              (google/get-api-url 'calendar 'Events 'insert
                                      `(("(calendarId)" ,calendar-id)))
              )
            (json-encode-plist item)
            )))
      (with-current-buffer buf
        ;; FIXME: parse the response and update the fields accordingly
        (message (buffer-string))))
    )

  ;; if succeed, update time

  )

;; * Tasks
;; Deprecated in my current workflow!

(defcustom google/tasks-files "~/Org/google-tasks.org"
  "Org formated file created to reflect Google tasks properties"
  :group 'google
  :type 'string)

;; Google Tasks Lists
;; {
;; "kind": "tasks#taskList",
;; "id": string,
;; "etag": string,
;; "title": string,
;; "updated": datetime,
;; "selfLink": string
;; }

;; Google Task
;; {
;; "kind": "tasks#task",
;; "id": string,
;; "etag": etag,
;; "title": string,
;; "updated": datetime,
;; "selfLink": string,
;; "parent": string,
;; "position": string,
;; "notes": string,
;; "status": string,
;; "due": datetime,
;; "completed": datetime,
;; "deleted": boolean,
;; "hidden": boolean,
;; "links": [
;;           {
;;           "type": string,
;;           "description": string,
;;           "link": string
;;           }
;;           ]
;; }

(defconst google-tasks/apis
  '(:baseuri "https://www.googleapis.com/tasks/v1"
    :resource-url "https://www.googleapis.com/auth/tasks"
    taskslists (
                (list   :get    "/users/@me/lists"            "Returns all the authenticated user's task lists.")
                (get    :GET    "/users/@me/lists/(tasklistId)" "Returns the authenticated user's specified task list.")
                (insert :POST   "/users/@me/lists"            "Creates a new task list and adds it to the authenticated user 's task lists.")
                (update :PUT    "/users/@me/lists/(tasklistId)" "Updates the authenticated user's specified task list.")
                (delete :DELETE "/users/@me/lists/(tasklistId)" "Deletes the authenticated user's specified task list.")
                (patch  :PATCH  "/users/@me/lists/(tasklistId)" "Updates the authenticated user's specified task list. This method supports patch semantics."))
    tasks (
           (list    :GET    "/lists/(tasklistId)/tasks"             "Returns all tasks in the specified task list.")
           (get     :GET    "/lists/(tasklistId)/tasks/(taskId)"      "Returns the specified task.")
           (insert  :POST   "/lists/(tasklistId)/tasks"             "Creates a new task on the specified task list.")
           (update  :PUT    "/lists/(tasklistId)/tasks/(taskId)"      "Updates the specified task.")
           (delete  :DELETE "/lists/(tasklistId)/tasks/(taskId)"      "Deletes the specified task from the task list.")
           (clear   :POST   "/lists/(tasklistId)/clear"             "Clears all completed tasks from the specified task list. The affected tasks will be marked as 'hidden' and no longer be returned by default when retrieving all tasks for a task list.")
           (move    :POST   "/lists/(tasklistId)/tasks/(taskId)/move" "Moves the specified task to another position in the task list. This can include putting it as a child task under a new parent and/or move it to a different position among its sibling tasks.")
           (patch   :PATCH  "/lists/(tasklistId)/tasks/(taskId)"       "Updates the specified task. This method supports patch semantics."))))


;; (google/get-api-url 'tasks 'taskslists 'list)

(defun google/get-taskslists ()
  (let ((google/resource-url (google/get-resource-url 'tasks)))
    (google/url-plist (google/get-api-url 'tasks 'taskslists 'list)))
  )

(defun google/get-taskslist-tasks (tasklist-id)
  (google/url-plist (google/get-api-url 'tasks 'tasks 'list `(("(tasklistId)" ,tasklist-id)))))

(defun google-tasks-to-org (task)
  "Converts a JSON task from Google in to an Org element.")

(defun google-tasks-from-org (task)
  "Converts an Org element task into a JSON Google task.")

(defun google-tasks-initialize()
  "Initialize Google tasks interface by downloading
the set of tasks from your Google account locally."
  (let ((tasks (google/get-all-tasks)))
    (dolist (task tasks)
      (google-tasks-to-org task)
      )
    )
  )

(defun google-tasks-org-extract-task (task)
  (let ((pd (org-element-map task '(property-drawer)
              #'identity nil t))
        result)
    (org-element-map pd 'node-property
      #'(lambda (x) (push (list  (org-element-property :key x)
                                 (org-element-property :value x))
                          result)))
    (list (org-element-property :raw-value task)
          (cons
           (org-element-property :todo-type task)
           (org-element-property :todo-keyword task))
          (result)
          )
    ))

;; Process inlinetasjk differently.
;; See #'org-element-inlinetask-parser

(defun google-tasks-org-grab ()
  "Grab all your tasks from your agenda files"
  (let ((files (org-agenda-files nil 'ifmode))
        tasks)
    (org-agenda-prepare-buffers files)
    (dolist (file files)
      (with-current-buffer (get-file-buffer file)
        (org-element-map (org-element-parse-buffer)
            '(headline inline-task)
          (lambda (task)
            (when (org-element-property :todo-keyword task)
              (push (google-tasks-org-extract-task task) tasks))))))
    (with-temp-buffer
      (mapc '(lambda (x) (insert (format "%s" x))) tasks)
      (buffer-string)
      )))


(defun google-tasks-format-tasks (tasks)
  (mapconcat #'(lambda (tasklist)
                 (let ((google-tasks-tasklist-context tasklist))
                   (concatenate
                    'string
                    (google-tasks-format-single-task (car tasklist))
                    (mapconcat 'google-tasks-format-single-task (plist-get (cadr tasklist) :items) "\n"))))
             tasks "\n"))

(defvar google-tasks-tasks-list-format
  "* %s
   :PROPERTIES:
   :ID: %s
   :UPDATED: %s
   :SELFLINK: %s
   :END
")

(defvar google-tasks-task-format
  "%s %s %s
   :PROPERTIES:
   :ID: %s
   :UPDATED: %s
   :SELFLINK: %s
   :PARENT: %s
   :POSITION: %s
   :NOTES: %s
   :DUE:
   :COMPLETED: %s
   :DELETED: %s
   :HIDDEN: %s
   :END

")

(defun google-tasks-task-depth (task)
  "Find the depth of the task in the current context.
The context is set by looking at `google-tasks-tasklist-context'.
If none, the depth is set to 1."
  (if (not (boundp 'google-tasks-tasklist-context))
      1)
  (let ((depth 2))
    (loop
     while (plist-get task :parent)
     do (incf depth)
     do (setf task
              (find (cadr (plist-get task :parent))
                    google-tasks-tasklist-context
                    :key #'(lambda (x) (plist-get x :id))))
     finally (return depth))))

(defun google-tasks-format-single-task (task)
  (let ((kind (plist-get task :kind)))
    (cond ((string= kind "tasks#taskList")
           (apply #'format google-tasks-tasks-list-format
                  (mapcar #'(lambda (x) (plist-get task x))
                          '(:title :id :updated :selfLink))
                  ))
          ((string= kind "tasks#task")
           (concatenate 'string
                        (apply #'format google-tasks-task-format
                               (make-string (google-tasks-task-depth task) ?*)
                               (let ((status (plist-get task :status)))
                                 (cond ((string= status "needsAction") "TODO")
                                       ((string= status "completed") "DONE")
                                       (t (warn "Status not handled: %s" status) "")))
                               (mapcar #'(lambda (x) (plist-get task x))
                                       '(:title :id :updated :selfLink :parent :position :notes :due :completed :deleted :hidden)))
                        (mapconcat #'(lambda (x)
                                       (format " %s: [[%s][%s]]" (plist-get x :type) (plist-get x :link) (plist-get x :description)))
                                   (plist-get task :links)
                                   "\n"
                                   )
                        "\n"))
          (t (warn "Unhandled task kind: %s" kind)))))

(defun google/get-all-tasks ()
  (let* ((google/resource-url (google/get-resource-url 'tasks))
         (taskslists (plist-get (google/get-taskslists) :items)))
    (loop for i from 0 to (1- (length taskslists))
          collect (list
                   (aref taskslists i)
                   (google/get-taskslist-tasks (plist-get (aref taskslists i) :id))))
    )
  )

(defvar google/tasks-file (expand-file-name "~/Org/google-tasks.org"))

(defun google-tasks-get-tasks ()
  "Insert Google tasks into Org bufferin"
  (interactive)
  (let* (tasks)
    (find-file-other-window google/tasks-file)
    (erase-buffer)
    (insert (google-tasks-format-tasks (google/get-all-tasks)))
    )
  (save-buffer)
  (message (concat "Pull tasks list to " google/tasks-file)))

;;; This is needed in order no to type again and again the passphrase to decrypt oauth2.plstore
;;; and until gpg-agent works on mingw64
(setq epa-file-select-keys nil)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; (google-tasks-get-tasks)


;; TODO:
;; Idea : tag task in Org and use the Tag as the tasksList.

;; Solve the problem of moving tasks around.

;; Next step : mark task as done -> update status on Google.

;; (setq foo  (google/get-all-tasks))

;; (google-tasks-format-tasks foo)

;; Check that calendar tasks are correctly updated both sides.

;; Cleanup the code

(provide 'org-google)
