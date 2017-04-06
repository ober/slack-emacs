;;; -*- lexical-binding: t -*-
;;; slack.el --- Slack.com chat interface

;; Copyright (C) 2014 Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: Slack.com chat
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Some of this is cribbed from:
;; time, and many revisions before I would expect it to be useful to anyone.
;;

;; Define slack-token to be the token provided by slack.com
;; Define slack-username as the user to post messages as.

;;; Code:

(require 'web)
(require 'json)


(defvar slack/hashes '( :users :channels-id :channels-name :api-endpoints))
(defvar slack-users (make-hash-table :test 'equal))
(defvar slack-channels-id (make-hash-table :test 'equal))
(defvar slack/channels-name (make-hash-table :test 'equal))
(defvar cruft-to-hyphens '( "=" " " "\\."))
(defvar slack/master-url "https://slack.com/api/")

(mapcar (lambda (h)
	  (set (intern (format "slack/%s" h))
	    (make-hash-table :test 'equal))) slack/hashes)

(defun slack/channel-history (name)
  (interactive "sChannel:")
  (let ((id (slack/get-channel-id-from-name name)))
    (slack/get-channel-history name id)))

(defun slack/group-history (name)
  (interactive "sChannel:")
  (let ((id (slack/get-id-from-name name)))
    (slack/get-group-history name id)))

(defun hyphenize-string (original replacements)
  (if (eq nil replacements)
      original
    (progn
      (hyphenize-string
       (replace-regexp-in-string (car replacements) "-" original)
       (cdr replacements)))))

(defun slack/make-buff-name (endpoint)
  (format "*slack-%s-%s*"
	  (hyphenize-string endpoint cruft-to-hyphens)))

(defun slack/make-uri (endpoint addendum)
  (format "%s%s" slack/master-uri addendum))


(defun slack/api-request (endpoint name handler args)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer (slack/make-buff-name name))
		(uri (format "%s%s" slack/master-url endpoint)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'handler (cdr (assoc 'ims (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/get-user-chat-list ()
  (interactive)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer "*slack-im-list*")
		(uri (format "https://slack.com/api/im.list?token=%s" slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-users-chat-list (cdr (assoc 'ims (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/get-groups-list ()
  (interactive)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer "*slack-group-list*")
		(uri (format "https://slack.com/api/groups.list?token=%s" slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-groups (cdr (assoc 'groups (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/print-groups (element)
  (let* ((purpose (cdr (assoc 'purpose element)))
	 (topic (cdr (assoc 'topic element)))
	 (members (cdr (assoc 'members element)))
	 (is_archived (cdr (assoc 'is_archived element)))
	 (creator (cdr (assoc 'creator element)))
	 (created (cdr (assoc 'created element)))
	 (name (cdr (assoc 'name element)))
	 (id (cdr (assoc 'id element))))
    (slack/create-group-link-in-buffer "blue" name id)
    (insert (propertize (format " Creator:%s" (slack/get-username-from-id creator)) 'face '(:foreground "red")))
    (insert (propertize (format " Topic:%s" topic) 'face '(:foreground "red")))
    (insert (propertize (format " Created:%s" (print-time created)) 'face '(:foreground "purple")))
    (insert (propertize (format " name:%s" name) 'face '(:foreground "Darkblue2")))
    (insert (propertize (format " id:%s" id) 'face '(:foreground "blue")))
    (insert (propertize (format " is_archived:%s" is_archived) 'face '(:foreground "darkgreen")))
    (princ "\n")))

(defun slack/print-users-chat-list (element)
  (message "XXX:%s" element)
  (let* ((is_user_deleted (cdr (assoc 'is_user_deleted element)))
	 (created (cdr (assoc 'created element)))
	 (user (cdr (assoc 'user element)))
	 (id (cdr (assoc 'id element)))
	 (username (slack/get-username-from-id user)))
    (if username (slack/create-im-link-in-buffer "blue" username id))
    (insert (propertize (format " User:%s" (slack/get-username-from-id user)) 'face '(:foreground "red")))
    (insert (propertize (format " Created:%s" (print-time created)) 'face '(:foreground "purple")))
    (insert (propertize (format " id:%s" id) 'face '(:foreground "darkblue")))
    (insert (propertize (format " Deleted?:%s" is_user_deleted) 'face '(:foreground "darkgreen")))
    (princ "\n")))

(defun slack/get-channel-history (name id)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer (format "*slack-%s-history*" name))
		(uri (format "https://slack.com/api/channels.history?token=%s&channel=%s&count=%s" slack-token id 2000))
		(id (slack/get-channel-id-from-name name))
		)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-channel-history (cdr (assoc 'messages (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/get-group-history (name id)
  (message "name:%s id:%s" name id)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer (format "*slack-%s-history*" name))
		(uri (format "https://slack.com/api/groups.history?token=%s&channel=%s&count=%s" slack-token id 2000))
		(id (slack/get-channel-id-from-name name))
		)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-channel-history (cdr (assoc 'messages (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/get-im-history (name id)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer (format "*slack-%s-history*" name))
		(uri (format "https://slack.com/api/im.history?token=%s&channel=%s&count=%s" slack-token id 200))
		;;(id (slack/get-channel-id-from-name name))
		)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-channel-history (cdr (assoc 'messages (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/list-emojis ()
  (interactive)
  (lexical-let ((data `(("token" . ,slack-token)))
		(channel-buffer "*slack-emoji-history*")
		(uri (format "https://slack.com/api/emoji.list?token=%s" slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-emojis (cdr (assoc 'emoji (json-read-from-string my-data))))))
     ;;(mapcar #'slack/print-channel-history (cdr (assoc 'emoji (json-read-from-string my-data))))))
     :url uri
     :extra-headers data)))

(defun slack/print-emojis (element)
  (message "EEE: %s" element))

(defun slack/search-file (query)
  (interactive "sSlack Query: ")
  (slack/go-search query "file"))

(defun slack/search-messages (query)
  (interactive "sSlack Query: ")
  (slack/go-search query "messages"))

(defun slack/go-search (query type)
  (lexical-let* ((data `(("token" . ,slack-token)))
		 (channel-buffer (format "*slack-search-%s-*" (replace-regexp-in-string " " "-" query)))
		 (uri (format "https://slack.com/api/search.%s?token=%s&count=%s&query=%s" type slack-token 10 query)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-search-results (cdr (assoc 'matches (cdr (assoc 'messages (json-read-from-string my-data))))))))
     :url uri
     :extra-headers data)))

(defun slack/post-im (username  message)
  (interactive "sSlack Channel:\nsMessage: ")
  (lexical-let* ((data `(("token" . ,slack-token)))
		 (channel-buffer "*slack-post*")
		 (uri (format "https://slack.com/api/chat.postMessage?token=%s&channel=%%23%s&text=%s&username=%s" slack-token channel (replace-regexp-in-string " " "%20" message) username)))
    (message "XXX: uri:%s" uri)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-post-results (json-read-from-string my-data))))
     :url uri
     :extra-headers data)))

(defun slack/post-message (username channel message)
  (interactive "sSlack Channel:\nsMessage: ")
  (lexical-let* ((data `(("token" . ,slack-token)))
		 (channel-buffer "*slack-post*")
		 (uri (format "https://slack.com/api/chat.postMessage?token=%s&channel=%%23%s&text=%s&username=%s" slack-token channel (replace-regexp-in-string " " "%20" message) username)))
    (message "XXX: uri:%s" uri)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-post-results (json-read-from-string my-data))))
     :url uri
     :extra-headers data)))

(defun slack/print-post-results (element)
  (message "AAA:%s" element)
  (let ((ts (cdr (assoc 'ts element)))
	(channel (cdr (assoc 'channel element)))
	(ok (cdr (assoc 'ok element))))
    (insert (propertize (format " Time:%s Channel:%s OK?:%s" (print-time ts) (slack/get-channel-from-id channel) ok) 'face '(:foreground "white")))))

(defun slack/print-search-results (element)
  (let ((text (cdr (assoc 'text element)))
	(ts (cdr (assoc 'ts element)))
	(type (cdr (assoc 'type element)))
	(user (cdr (assoc 'user element)))
	(username (cdr (assoc 'username element))))
    (slack/create-user-link-in-buffer "darkgreen" username user)
    (insert (propertize (format " %s" username) 'face '(:foreground "red")))
    (insert (propertize (format " %s" user) 'face '(:foreground "white")))
    (insert (propertize (format " %s" (print-time ts)) 'face '(:foreground "purple")))
    (insert (propertize (format " %s" type) 'face '(:foreground "darkblue")))
    (insert (propertize (format " %s" text) 'face '(:foreground "darkgreen")))
    (princ "\n")))

(defun slack/print-channel-history (element)
  (let* ((ts (cdr (assoc 'ts element)))
	 (text (cdr (assoc 'text element)))
	 (user (cdr (assoc 'user element)))
	 (type (cdr (assoc 'type element))))
    (insert (propertize (format " %s" (slack/get-username-from-id user)) 'face '(:foreground "darkgreen")))
    (insert (propertize (format " %s" type) 'face '(:foreground "red")))
    (insert (propertize (format " %s" (print-time ts)) 'face '(:foreground "blue")))
    (insert (propertize (format " %s" text) 'face '(:foreground "darkblue")))
    (princ "\n")))

(defun slack/list-channels ()
  (interactive)
  (lexical-let ((uri (format "https://slack.com/api/channels.list?token=%s" slack-token)))
    ;;(lexical-let ((uri (format "https://slack.com/api/channels.list?token=%s" slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*slack-channels*"
	 (switch-to-buffer-other-window "*slack-channels*")
	 (mapcar #'slack/print-list-channels (cdr (assoc 'channels (json-read-from-string my-data))))))
     :url uri)))

(defun slack/list-users ()
  (interactive)
  (lexical-let ((uri (format "https://slack.com/api/users.list?token=%s" slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*slack-users*"
	 (switch-to-buffer-other-window "*slack-users*")
	 (mapcar #'slack/print-list-users
		 (cdr (assoc 'members (json-read-from-string my-data))))))
     :url uri)))

(defun slack/function-on-url (function, url, attribute, title)
  (lexical-let ((uri (format "%s?token=%s" url slack-token)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*slack-channels*"
	 (switch-to-buffer-other-window "*slack-channels*")
	 (mapcar #'slack/print-list-channels (cdr (assoc 'channels (json-read-from-string my-data))))))
     :url uri)))

(defun slack/print-list-users (element)
  (let-alist element
    (list .profile .name .has_files .real_name .phone .is_ultra_restricted .is_restricted .is_admin .phone .skype)
    (color-insert
     (list
      (cons .name "orange") (cons .has_files "blue") (cons .real_name "purple") (cons .phone "pink") (cons .is_ultra_restricted "black") (cons .is_restricted "brown") (cons .is_admin "silver") (cons .phone "darkblue") (cons .skype "darkblue")))))

(defun slack/print-list-channels (element)
  (let-alist element
    (list .num_members .purpose .topic .is_member .members .is_general .is_archived .creator .created .name .id)
    (color-insert
     (list
      (cons .num_members "orange")
      (cons .purpose "blue")
      (cons .topic "purple")
      (cons .is_member "pink")
      (cons .members "black")
      (cons .is_general "brown")
      (cons .is_archived "silver")
      (cons .creator "darkblue")
      (cons .created "darkblue")
      (cons .name "blue")
      (cons .id "orange")))))

(defun maybe-insert-propertized (sym alist color)
  "Pull the cdr of an assoc of SYM from ALIST and print it via color COLOR"
  (let ((value (cdr (assoc sym alist))))
    (insert (propertize (format " %s:%s" sym value) 'face '(:forground color)))))

(defun color-insert (pairs)
  "Color me: Insert a propertized tag for text and color coding"
  (dolist (x pairs)
    (let ((val (car x))
	  (color (cdr x)))
      (insert (propertize (format "%s " val) 'face `(:foreground ,color)))))
  (princ "\n"))

(defun slack/print-list-users (element)
  (let-alist element
    (list .profile .name .has_files .real_name .phone .is_ultra_restricted .is_restricted .is_admin .phone .skype)
    (color-insert
     (list
      (cons .name "orange") (cons .has_files "blue") (cons .real_name "purple") (cons .phone "pink") (cons .is_ultra_restricted "black") (cons .is_restricted "brown") (cons .is_admin "silver") (cons .phone "darkblue") (cons .skype "darkblue")))))


(defun slack/print-message (element)
  (message "SSS: %s" element))

(defun slack/create-channel-link-in-buffer (color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (slack/get-channel-history title id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (slack/get-channel-history title id)))
    (insert
     (propertize
      title
      'face '(:foreground color)
      'keymap map
      'mouse-face 'highlight))))

(defun slack/create-group-link-in-buffer (color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (slack/get-group-history title id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (slack/get-group-history title id)))
    (insert
     (propertize
      title
      'face '(:foreground color)
      'keymap map
      'mouse-face 'highlight))))

(defun slack/create-im-link-in-buffer (color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (slack/get-im-history title id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (slack/get-im-history title id)))
    (insert
     (propertize
      title
      'face '(:foreground color)
      'keymap map
      'mouse-face 'highlight))))

(defun slack/create-user-link-in-buffer (color username user)
  "Insert clickable string inside a buffer"
  (lexical-let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (slack/go-search user)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (slack/search user)))
    (insert
     (propertize
      username
      'face '(:foreground color)
      'keymap map
      'mouse-face 'highlight))))

(defun print-time (ts)
  (format-time-string "%F" (seconds-to-time (if (numberp ts) ts (string-to-number ts)))))

(defun slack/get-username-from-id (id)
  (if id
      (gethash id slack-users)
    "Nobody"))

(defun slack/get-channel-from-id (id)
  (if id
      (gethash id slack/channels-id)
    "NoChan"))

(defun slack/get-channel-id-from-name (id)
  (if id
      (gethash id slack/channels-name)
    "NoChan"))

(defun slack/populate-users-hash ()
  (interactive)
  (if (eq (hash-table-count slack-users) 0)
      (slack/list-users)))

(defun slack/populate-channel-hash ()
  (interactive)
  (or (eq (hash-table-count slack/channels-name) 0)
      (eq (hash-table-count slack/channels-id) 0))
  (slack/list-channels))

(defun slack/im-open (channel message)
  (interactive "sSlack Channel:\nsMessage: ")
  (lexical-let* ((data `(("token" . ,slack-token)))
		 (channel-buffer "*slack-post*")
		 (uri
		  (format "https://slack.com/api/chat.postMessage?token=%s&channel=%%23%s&text=%s&username=%s"
			  slack-token
			  channel
			  (replace-regexp-in-string " " "%20" message) username)))
    (message "XXX: uri:%s" uri)
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer channel-buffer
	 (switch-to-buffer-other-window channel-buffer)
	 (mapcar #'slack/print-post-results (json-read-from-string my-data))))
     :url uri
     :extra-headers data)))
