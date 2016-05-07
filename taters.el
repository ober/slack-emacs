;;; -*- lexical-binding: t -*-

;; This file is for all the shit I cribbed from lokedhs well written potato-emacs
;; https://github.com/lokedhs/potato-emacs


(require 'url)
(require 'subr-x)
(require 'shr)
(require 'notifications)

(defgroup slack nil
  "Slack client implementation for Emacs"
  :prefix 'slack
  :group 'applications)

(defface slack-default
  ()
  "Default face for slack buffers."
  :group 'slack)

(defface slack-message-from
  '((((class color))
     :foreground "#00b000"
     :inherit slack-default)
    (t
     :inherit slack-default))
  "Face used to display the 'from' part of a message."
  :group 'slack)

(defface slack-message-user-name
  '((((class color))
     :background "#e0e0e0"
     :inherit slack-default)
    (t
     :inherit slack-default))
  "Face used to display user names."
  :group 'slack)

(defface slack-message-input-user-name
  '((((class color))
     :background "#e0e0e0"
     :inherit slack-default)
    (t
     :inherit slack-default))
  "Face used to display user names."
  :group 'slack)

(defface slack-message-code
  '((((class color))
     :background "#f0f0f0"
     :inherit slack-default)
    (t
     :inherit slack-default))
  "Face used to display code snippets."
  :group 'slack)

(defface slack-notification
  '((((class color))
     :foreground "#ff0000"
     :inherit slack-default)
    (t
     :inherit slack-default))
  "Face used to display the unread notifications number in the modeline."
  :group 'slack)

(defcustom slack-api-token ""
  "API token for the user"
  :type 'string
  :group 'slack)

(defcustom slack-url "https://slack.dhsdevelopments.com/"
  "The main url for the Slack instance"
  :type 'string
  :group 'slack)

(defvar slack-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<S-return>") 'slack-insert-nl)
    (define-key map (kbd "RET") 'slack-send-input-line)
    (define-key map (kbd "@") 'slack-insert-user)
    map))

(defvar slack--active-buffers nil)
(defvar slack--connection nil
  "The current HTTP connection that is waiting for data, or nil if not currently connected to the server.")
(defvar slack--event-id nil
  "The event id for the current connection, or nil if client not started")
(defvar slack-display-notifications-string "")
(defvar slack--notifications nil)
(defvar slack--unread-channels nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--string-match-start (string key)
  (and (>= (length string) (length key))
       (string= (subseq string 0 (length key)) key)))

(defun slack--assoc-with-check (tag alist &optional allow-missing)
  (let ((value (assoc tag alist)))
    (cond (value
           (cdr value))
          (allow-missing
           nil)
          (t
           (error "No value for tag: %s" tag)))))

(defun slack--make-random-string (n)
  (with-output-to-string
    (loop repeat n
          do (princ (format "%c" (+ ?a (random (1+ (- ?z ?a)))))))))

(defun slack--make-temp-directory (prefix)
  (cl-labels ((try-make-dir (name)
                 (condition-case condition
                     (progn
                       (make-directory name)
                       t)
                   (error nil))))
    (loop repeat 25
          for dirname = (format "%s%s-%s" temporary-file-directory prefix (slack--make-random-string 40))
          when (try-make-dir dirname)
          return dirname
          finally (error "Unable to create temporary directory"))))

(defun slack--format-date (date)
  (format-time-string "%a %d %b %Y, %H:%M:%S" (date-to-time date)))

(cl-defmacro slack--with-channel (cid &body body)
  (declare (indent 1))
  (let ((buffer-sym (gensym "buffer")))
    `(let ((,buffer-sym (slack--find-channel-buffer ,cid)))
       (when ,buffer-sym
         (with-current-buffer ,buffer-sym
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--json-parse-result-buffer ()
  (let* ((content (buffer-substring (point) (point-max)))
         (decoded-content (decode-coding-string content 'utf-8)))
    (json-read-from-string decoded-content)))

(defun slack--url-handler (status buffer callback as-json-p)
  (let ((error-status (getf status :error)))
    (if error-status
        (progn
          (message "Got error: %S" status)
          (signal (car error-status) (cdr error-status)))
      ;; ELSE: No error
      (progn
        (goto-char (point-min))
        (search-forward "\n\n")
        (let ((data (if as-json-p
                        (slack--json-parse-result-buffer)
                      (buffer-substring (point) (point-max)))))
          (with-current-buffer buffer
            (funcall callback data))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer (current-buffer))))))))

(cl-defmacro slack--with-url-params ((url-sym url method) &body body)
  (declare (indent 1))
  (let ((base-sym (gensym "base-")))
    `(let* ((,base-sym slack-url))
       (let ((url-request-method ,method)
             (url-request-extra-headers (list (cons "API-Token" slack-api-token)))
             (,url-sym (format "%s%sapi/1.0%s"
                               ,base-sym
                               (if (eql (aref ,base-sym (1- (length ,base-sym))) ?/) "" "/")
                               ,url)))
         ,@body))))

(cl-defun slack--url-retrieve (url method callback &key (as-json-p t) check-if-shutdown)
  (let ((buffer (current-buffer)))
    (slack--with-url-params (result-url url method)
      (url-retrieve result-url (lambda (status)
                                 (unless (and check-if-shutdown slack--shutdown-in-progress)
                                   (slack--url-handler status buffer callback as-json-p)))
                    nil t))))

(cl-defun slack--url-retrieve-synchronous (url method)
  (slack--with-url-params (result-url url method)
    (let ((buffer (url-retrieve-synchronously result-url)))
      (let ((data (with-current-buffer buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (slack--json-parse-result-buffer))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))
        data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--read-input-line (start end)
  (let ((uid-refs (loop for overlay in (overlays-in start end)
                        for uid = (overlay-get overlay 'slack-user-ref)
                        when uid
                        collect (list (overlay-start overlay) (overlay-end overlay) uid overlay))))
    (with-output-to-string
      (loop with p = start
            for uid-ref in (sort uid-refs (lambda (a b) (< (first a) (first b))))
            if (< p (first uid-ref))
            do (princ (buffer-substring p (first uid-ref)))
            do (progn
                 (princ (format "\U000f0001user:%s:%s\U000f0001"
                                (third uid-ref) (buffer-substring (first uid-ref) (second uid-ref))))
                 (setq p (second uid-ref))
                 (delete-overlay (fourth uid-ref)))
            finally (when (< p end)
                      (princ (buffer-substring p end)))))))

(defun slack--input (str)
  (let ((url-request-data (encode-coding-string (json-encode `((text . ,str))) 'utf-8)))
    (slack--url-retrieve (format "/channel/%s/create" slack--channel-id) "POST"
                          (lambda (data) nil))))

(defun slack-send-input-line ()
  "Send the currently typed line to the server."
  (interactive)
  (let ((text (string-trim (slack--read-input-line slack--input-marker (point-max)))))
    (when (not (equal text ""))
      (delete-region slack--input-marker (point-max))
      (slack--input text))))

(defun slack-insert-nl ()
  "Insert a newline into the message."
  (interactive)
  (insert "\n"))

(defun slack-switch-to-next-unread ()
  "Switch to the first channel with unread messages."
  (interactive)
  (let ((buffer (loop for channel in (slack--get-all-unread-channels)
                      for buffer = (slack--find-channel-buffer (first channel) :create-if-missing t)
                      when (not (eq buffer (current-buffer)))
                      return buffer)))
    (if buffer
        (switch-to-buffer buffer)
      (message "No channels with unread messages"))))

(defun slack--insert-message (message-id timestamp updated-date from text image extra-html)
  (save-excursion
    (goto-char slack--output-marker)
    (let ((new-pos (loop with prev-pos = (point)
                         for pos = (previous-single-char-property-change prev-pos 'slack-timestamp)
                         until (let ((prop (get-char-property pos 'slack-timestamp)))
                                 (and prop (string< prop timestamp)))
                         do (setq prev-pos pos)
                         until (= pos (point-min))
                         finally
                         return prev-pos)))
      (goto-char new-pos)
      (let ((inhibit-read-only t))
        (let ((start (point)))
          (insert (concat (propertize (concat (format "[%s] %s" from (slack--format-date timestamp))
                                              (if updated-date
                                                  (format " (updated %s)" (slack--format-date updated-date))
                                                "")
                                              "\n")
                                      'face 'slack-message-from)
                          text
                          "\n\n"))
          (when image
            (slack--insert-image (slack--assoc-with-check 'file image))
            (insert "\n"))
          (when extra-html
            (let ((extra-html-start (point)))
              (insert extra-html)
              (shr-render-region extra-html-start (point))))
          (add-text-properties start (point)
                               (list 'read-only t
                                     'slack-message-id message-id
                                     'slack-timestamp timestamp
                                     'front-sticky '(read-only))))))))

(define-derived-mode slack-channel-mode nil "Slack"
  "Mode for Slack channel content"
  (use-local-map slack-channel-mode-map)
  (setq-local slack--output-marker (make-marker))
  (setq-local slack--input-marker (make-marker))
  (set-marker slack--output-marker (point-max))
  (insert "channel> ")
  (add-text-properties (point-at-bol) (point)
                       (list 'read-only t
                             'rear-nonsticky t
                             'front-sticky '(read-only)
                             'inhibit-line-move-field-capture t
                             'field 'output))
  (set-marker-insertion-type slack--output-marker t)
  (set-marker slack--input-marker (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--insert-image-handler (overlay data)
  (let ((image (create-image data nil t))
        (start (overlay-start overlay))
        (end (overlay-end overlay)))
    (delete-overlay overlay)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char start)
        (delete-region start end)
        (let ((start (point)))
          (insert-image image "[image]")
          (slack--extend-message-text-properties start (point)))))))

(cl-defun slack--insert-image (file)
  (let ((buffer (current-buffer))
        (start (point)))
    (insert "[loading-image]")
    (let ((overlay (make-overlay start (point))))
      (slack--url-retrieve file "GET"
                            (lambda (data)
                              (slack--insert-image-handler overlay data))
                            :as-json-p nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maths rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To render an expression:
;;;   latex -halt-on-error -output-directory=z foo.tex
;;;   dvipng -o foo.png -bg transparent -q -T tight -z 9 z/foo.dvi

(defun slack--render-maths-to-image (latex-expression)
  (let* ((dirname (slack--make-temp-directory "slack-render"))
         (file-prefix (format "%s/math-formula" dirname)))
    (with-temp-buffer
      (insert "\\documentclass[12pt]{article}")
      (insert "\\pagestyle{empty}")
      (insert "\\begin{document}")
      (insert "\\(")
      (insert latex-expression)
      (insert "\\)")
      (insert "\\end{document}")
      (write-file (format "%s.tex" file-prefix)))
    (if (not (zerop (shell-command (format "latex -halt-on-error -output-directory=%s %s.tex" dirname file-prefix))))
        (message "Illegal formula, can't render with LaTeX")
      (if (not (zerop (shell-command "dvipng -o %s.png -bg transparent -q -T tight -z 9 %s.dvi" file-prefix file-prefix)))
          (message "Unable to convert formula to png")
        (format "%s.png" file-prefix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--user-ref-updated (overlay &rest rest)
  (delete-overlay overlay))

(defun slack-insert-user ()
  "Select a username to be inserted into the new message."
  (interactive)
  (let ((result (let ((completion-ignore-case t))
                  (completing-read "User: " (mapcar #'second slack--users) nil t nil nil nil t))))
    (if (equal result "")
        ;; Blank string, simply insert an @-sign
        (insert "@")
      ;; ELSE: Insert a username tag
      (let ((element (find result slack--users :key #'second :test #'equal)))
        (unless element
          (error "Selected element not found in user list: %S" element))
        (let ((start (point)))
          (insert (second element))
          (let ((overlay (make-overlay start (point) nil nil nil)))
            (overlay-put overlay 'face 'slack-message-input-user-name)
            (overlay-put overlay 'slack-user-ref (first element))
            (overlay-put overlay 'modification-hooks '(slack--user-ref-updated))))))))

(defun slack--parse-json-decode-span (text face)
  (let ((s (slack--parse-json-decode-element text)))
    (propertize s 'font-lock-face face)))

(defvar slack-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'slack-open-selected-link)
    (define-key map (kbd "RET") 'slack-open-selected-link)
    map))

(defun slack-open-selected-link ()
  "Open the link at the current position."
  (interactive)
  (when-let ((url (get-char-property (point) 'slack-link-destination)))
    (browse-url url)))

(defun slack--parse-json-decode-url (element)
  (let ((addr (slack--assoc-with-check 'addr element))
        (description (slack--assoc-with-check 'description element)))
    (propertize description
                'font-lock-face 'link
                'mouse-face 'highlight
                'help-echo (format "mouse-2: browse url: %s" addr)
                'slack-link-destination addr
                'keymap slack-url-keymap)))

(defun slack--parse-json-decode-element (element)
  (etypecase element
    (string element)
    (array (apply #'concat (loop for e across element collect (slack--parse-json-decode-element e))))
    (cons (let ((type (slack--assoc-with-check 'type element)))
            (cond ((string= type "p")
                   (slack--parse-json-decode-element (slack--assoc-with-check 'e element)))
                  ((string= type "b")
                   (slack--parse-json-decode-span (slack--assoc-with-check 'e element) 'bold))
                  ((string= type "i")
                   (slack--parse-json-decode-span (slack--assoc-with-check 'e element) 'italic))
                  ((string= type "code")
                   (slack--parse-json-decode-span (slack--assoc-with-check 'e element) 'slack-message-code))
                  ((string= type "code-block")
                   (format "\n%s\n" (slack--parse-json-decode-span (slack--assoc-with-check 'code element) 'slack-message-code)))
                  ((string= type "url")
                   (slack--parse-json-decode-url element))
                  ((string= type "user")
                   (propertize (slack--assoc-with-check 'user_description element) 'font-lock-face 'slack-message-user-name))
                  ((string= type "newline")
                   "\n")
                  ((string= type "math")
                   (format "\n  %s\n" (slack--parse-json-decode-element (slack--assoc-with-check 'e element))))
                  ((string= type "inline-math")
                   (format "[%s]" (slack--parse-json-decode-element (slack--assoc-with-check 'e element))))
                  (t
                   (format "[unknown-element %s]" type)))))))

(defun slack--parse-json-message (content)
  (slack--parse-json-decode-element content))

(defun slack--extend-message-text-properties (start end)
  (let* ((pos (1- start))
         (message-id (get-text-property pos 'slack-message-id))
         (timestamp (get-text-property pos 'slack-timestamp)))
    (unless (and message-id timestamp)
      (error "No message text properties at position %d" pos))
    (add-text-properties start end (list 'read-only t
                                         'slack-message-id message-id
                                         'slack-timestamp timestamp
                                         'front-sticky t))))

(defun slack--find-message-in-log (message-id)
  (loop with curr = (point-min)
        for pos = (next-single-property-change curr 'slack-message-id)
        while pos
        for value = (get-char-property pos 'slack-message-id)
        when (equal value message-id)
        return (list pos (next-single-property-change pos 'slack-message-id))
        do (setq curr pos)
        finally (return nil)))

(defun slack--process-channel-message (message loading-history)
  (with-current-buffer (slack--find-channel-buffer (slack--assoc-with-check 'channel message))
    (let ((message-id (slack--assoc-with-check 'id message))
          (updated (slack--assoc-with-check 'updated message t)))
      (when (or loading-history
                (not updated)
                (let ((old-message-pos (slack--find-message-in-log message-id)))
                  (if old-message-pos
                      (destructuring-bind (start end) old-message-pos
                        (let ((inhibit-read-only t))
                          (delete-region start end))
                        t)
                    nil)))
        (when (not (slack--assoc-with-check 'deleted message t))
          (let* ((timestamp (slack--assoc-with-check 'created_date message))
                 (text (slack--assoc-with-check 'text message))
                 (from (slack--assoc-with-check 'from message))
                 (parsed (slack--parse-json-message text))
                 (user (cl-assoc from slack--users :test #'equal))
                 (image (let ((image-entry (assoc 'image message)))
                          (if image-entry (cdr image-entry) nil)))
                 (extra-html (slack--assoc-with-check 'extra_html message t))
                 (updated-date (slack--assoc-with-check 'updated_date message t)))
            (slack--insert-message message-id timestamp updated-date
                                    (if user (second user) "Unknown")
                                    (string-trim parsed)
                                    image
                                    extra-html)
            (when (and (not loading-history)
                       (not (get-buffer-window (current-buffer) 'visible)))
              (let ((old slack--unread-in-channel))
                (incf slack--unread-in-channel)
                (when (zerop old)
                  (slack--recompute-modeline))))))))))

(defun slack--process-channel-type-notification (message)
  (let* ((user (slack--assoc-with-check 'user message))
         (cid (slack--assoc-with-check 'channel message))
         (add-type (slack--assoc-with-check 'add-type message))
         (buffer (slack--find-channel-buffer cid :create-if-missing nil)))
    (when buffer
      (with-current-buffer buffer
        (cond ((equal add-type "begin")
               (cl-pushnew user slack--current-typing :test #'equal))
              ((equal add-type "end")
               (setq slack--current-typing (cl-remove user slack--current-typing :test #'equal)))
              (t
               (error "Unexpected typing mode: ~S" add-type)))
        (slack--recompute-channel-modeline)))))

(defun slack--update-active-state-for-user-from-id (uid new-state)
  (if slack--users
      (let* ((element (find uid slack--users :key #'car :test #'equal)))
        (if element
            (setf (cdr (nthcdr 2 element)) (list new-state))
          ;; ELSE: User was not found in user list, add a stub entry and request an update
          (push (list uid "(loading)" nil new-state) slack--users)))
    ;; ELSE: User list has not been updated yet, add the request to pending
    (cl-pushnew (list uid new-state) slack--pending-user-state :key #'car :test #'equal)))

(defun slack--update-active-state-for-user-from-message (message new-state)
  (slack--update-active-state-for-user-from-id (slack--assoc-with-check 'user message) new-state))

(defun slack--update-active-state-from-sync (message)
  (let* ((uids (mapcar (lambda (v)
                         (slack--assoc-with-check 'id v))
                       (slack--assoc-with-check 'users message)))
         (unregistered-users (cl-remove-if (lambda (v)
                                             (cl-member v slack--users :key #'car :test #'equal))
                                           uids)))
    (loop for row in slack--users
          do (setf (fourth row) (if (cl-member (first row) uids :test #'equal) t nil)))
    (dolist (uid unregistered-users)
      (slack--update-active-state-for-user-from-id uid t))))

(defun slack--process-channel-update-user (message)
  (slack--with-channel (slack--assoc-with-check 'channel message t)
    (let ((type (slack--assoc-with-check 'add-type message)))
      (cond ((equal type "add")
             (slack--update-active-state-for-user-from-message message t))
            ((equal type "remove")
             (slack--update-active-state-for-user-from-message message nil))
            ((equal type "sync")
             (slack--update-active-state-from-sync message))
            (t
             (message "Unexpected user update message: %S" message))))))

(defun slack--process-notification (message)
  (let* ((cid (slack--assoc-with-check 'channel message))
         (e (cl-find cid slack--notifications :key #'car :test #'equal)))
    (if e
        (incf (cdr e))
      (push (cons cid 1) slack--notifications)))
  (slack--recompute-modeline))

(defun slack--request-channel-info (cid callback)
  (slack--url-retrieve (format "/channel/%s" cid) "GET"
                        (lambda (data)
                          (funcall callback data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--find-unread-channel-data (cid)
  (cl-find cid slack--unread-channels :key #'car :test #'equal))

(defun slack--process-unread (message)
  (let* ((cid (slack--assoc-with-check 'channel message))         
         (e (slack--find-unread-channel-data cid))
         (n (slack--assoc-with-check 'count message)))
    (if e
        (progn
          (setf (third e) n)
          (slack--recompute-modeline))
      (progn
        (push (list cid "[unknown]" n) slack--unread-channels)
        (slack--request-channel-info cid
                                      (lambda (data)
                                        (let ((info (slack--find-unread-channel-data cid)))
                                          (when info
                                            (let ((channel-name (slack--assoc-with-check 'name data)))
                                              (setf (second info) channel-name)
                                              (slack--recompute-modeline))))))))))

(defun slack--process-channel-change (message)
  (let ((cid (slack--assoc-with-check 'channel message))
        (name (slack--assoc-with-check 'name message))
        (topic (slack--assoc-with-check 'topic message)))
    (slack--with-channel cid
      (unless (equal name slack--name)
        (slack--update-channel-name-in-buffer name))
      (setq slack--topic topic))))

(defun slack--get-all-unread-channels ()
  (let ((opened-channels (mapcan (lambda (v)
                                   (destructuring-bind (cid . buffer) v
                                     (with-current-buffer buffer
                                       (when (and slack--name (plusp slack--unread-in-channel))
                                         (list (list cid slack--name))))))
                                 slack--active-buffers)))
    (let ((remote (loop for (cid name unread-count) in slack--unread-channels
                        when (and (plusp unread-count)
                                  (not (cl-member cid opened-channels :key #'first :test #'equal)))
                        collect (list cid name))))
      (append opened-channels remote))))

(defun slack--make-unread-notification-string ()
  (if-let ((result (slack--get-all-unread-channels)))
      (with-output-to-string
        (princ "Unread: ")
        (loop for channel in result
              for first = t then nil
              unless first
              do (princ "/")
              do (princ (second channel))))))

(defun slack--add-remove-binding (cid add-p)
  (unless slack--event-id
    (error "Client has not been started"))
  (let ((url (with-output-to-string
               (princ "/channel-updates/update?event-id=")
               (princ slack--event-id)
               (princ "&cmd=")
               (princ (if add-p "add" "remove"))
               (princ "&channel=")
               (princ cid)
               (princ "&services=content,state,notifications,unread"))))
    (slack--url-retrieve url "POST" (lambda (data)
                                       (let ((result (assoc 'result data)))
                                         (unless (and result (equal (cdr result) "ok"))
                                           (message "Unable to connect to channel")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main event processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--process-new-message (message)
  (let ((type (slack--assoc-with-check 'type message)))
    (cond ((equal type "m")
           (slack--process-channel-message (slack--assoc-with-check 'c message) nil))
          ((equal type "type")
           (slack--process-channel-type-notification message))
          ((equal type "cu")
           (slack--process-channel-update-user message))
          ((equal type "usernot")
           (slack--process-notification message))
          ((equal type "unread")
           (slack--process-unread message))
          ((equal type "channel-change")
           (slack--process-channel-change message))
          (t
           (message "Unprocessed message: %S" message)))))

(defun slack--notify-error ()
  "Add an error message to the end of all active buffers indictating to the user that the connection failed."
  (loop for (cid . buffer) in slack--active-buffers
        do (with-current-buffer buffer
             (save-excursion
               (goto-char slack--output-marker)
               (let ((inhibit-read-only t))
                 (insert "Server disconnected\n"))))))

(defun slack--fetch-message (queue)
  (when slack--connection
    (error "Attempt to fetch a new message while a current request is already active"))
  (let* ((url (with-output-to-string
                (princ "/channel-updates?channels=")
                (loop for (cid . buffer) in slack--active-buffers
                      for first = t then nil
                      unless first
                      do (princ ",")
                      do (princ cid))
                (princ "&format=json&services=content,state,channel,notifications,unread")
                (when queue
                  (princ "&event-id=")
                  (princ queue)))))
    (let ((connection (slack--url-retrieve url
                                            "GET"
                                            (lambda (data)
                                              (setq slack--connection nil)
                                              (let ((error-result (assoc 'result data)))
                                                (if (and error-result (equal error-result "error"))
                                                    ;; The event id has expired
                                                    (slack--notify-error)
                                                  ;; ELSE: No error
                                                  (progn
                                                    (loop for message across (cdr (assoc 'data data))
                                                          do (slack--process-new-message message))
                                                    (let ((queue (cdr (assoc 'event data))))
                                                      (setq slack--event-id queue)
                                                      (unless queue
                                                        (message "Unexpected result from update: %S" data)
                                                        (error "No queue in channel update"))
                                                      (slack--fetch-message queue))))))
                                            :check-if-shutdown t)))
      (with-current-buffer connection
        (setq-local slack--shutdown-in-progress nil))
      (setq slack--connection connection))))

(cl-defun slack--load-history (&key (num-messages 50))
  (slack--url-retrieve (format "/channel/%s/history?format=json&num=%d" slack--channel-id num-messages)
                        "GET"
                        (lambda (data)
                          (loop for message across (slack--assoc-with-check 'messages data)
                                do (slack--process-channel-message message t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--enable-buffer (buffer)
  (with-current-buffer buffer
    (let ((active-buffers slack--active-buffers))
      (push (cons slack--channel-id buffer) slack--active-buffers)
      (cond ((null active-buffers)
             (slack--fetch-message nil))
            (t
             (slack--add-remove-binding slack--channel-id t))))))

(defun slack--buffer-closed ()
  (setq slack--active-buffers (cl-remove (current-buffer) slack--active-buffers :key #'cdr :test #'eq))
  (when (and (null slack--active-buffers)
             (member 'slack-display-notifications-string global-mode-string))
    (setq global-mode-string (remove 'slack-display-notifications-string global-mode-string)))
  (let ((connection slack--connection))
    (when connection
      (if slack--active-buffers
          ;; We still have other opened buffers, simply unregister the binding
          (when nil
            ;; TODO: Remove isn't supported by the server yet, so let's just keep the binding active
            (slack--add-remove-binding slack--channel-id nil))
        ;; ELSE: This was the last buffer, close the connection
        (let ((proc (get-buffer-process connection)))
          (when proc
            (message "All channel windows closed. Disconnecting from server.")
            (with-current-buffer connection
              (setq-local slack--shutdown-in-progress t))
            (setq slack--event-id nil)
            (setq slack--unread-channels nil)
            (condition-case condition
                (delete-process proc)
              (error (message "Error when closing buffer: %S" condition)))))))))

(defun slack--update-channel-name-in-buffer (name)
  "Updates the local buffer configration as well as its name to reflect the actual name of the channel."
  (setq slack--name name)
  (rename-buffer (format "Slack - %s" name) t))

(defun slack--window-config-updated ()
  "Hook function that is locally installed for window-configuration-change-hook in all channel buffers."
  (let ((recompute nil))
    (when (get-buffer-window)
      ;; Clear unread count
      (when (plusp slack--unread-in-channel)
        (setq slack--unread-in-channel 0)
        (setq recompute t))
      ;; Clear notifications for channel
      (let ((e (cl-find slack--channel-id slack--notifications :key #'car :test #'equal)))
        (when (and e (plusp (cdr e)))
          (slack--url-retrieve (format "/channel/%s/clear-notifications" slack--channel-id)
                                "POST"
                                (lambda (data)
                                  nil))
          (setf (cdr e) 0)
          (setq recompute t))))
    (when recompute
      (slack--recompute-modeline))))

(defun slack--create-buffer (cid)
  (let ((buffer (generate-new-buffer (format "*slack-%s*" cid))))
    (with-current-buffer buffer
      (slack-channel-mode)
      (setq-local slack--channel-id cid)
      (setq-local slack--users nil)
      (setq-local slack--pending-user-state nil)
      (setq-local slack--last-typing-notifcation nil)
      (setq-local slack--current-typing nil)
      (setq-local slack--channel-mode-line "")
      (setq-local slack--unread-in-channel 0)
      (setq-local slack--name nil)
      (setq-local slack--topic nil)
      (setq mode-line-format (append mode-line-format (list 'slack--channel-mode-line)))
      (slack--request-channel-info cid
                                    (lambda (data)
                                      (slack--update-channel-name-in-buffer (slack--assoc-with-check 'name data))
                                      (slack--request-user-list (lambda (users)
                                                                   (slack--update-userlist users)
                                                                   (slack--load-history)))))
      (slack--enable-buffer buffer)
      (add-hook 'kill-buffer-hook 'slack--buffer-closed nil t)
      (add-hook 'post-self-insert-hook 'slack--send-typing-notification t t)
      (add-hook 'window-configuration-change-hook 'slack--window-config-updated nil t))
    ;; Update the modeline indicator if needed
    (unless (member 'slack-display-notifications-string global-mode-string)
      (if global-mode-string
          (setq global-mode-string (append global-mode-string '(slack-display-notifications-string)))
        (setq global-mode-string '("" slack-display-notifications-string)))
      (slack--recompute-modeline))
    buffer))

(cl-defun slack--find-channel-buffer (cid &key create-if-missing)
  (let ((e (find cid slack--active-buffers :key #'car :test #'equal)))
    (cond (e
           (cdr e))
          (create-if-missing
           (slack--create-buffer cid))
          (t
           (error "No buffer for channel %s" cid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slack--request-user-list (callback)
  (slack--url-retrieve (format "/channel/%s/users" slack--channel-id)
                        "GET"
                        (lambda (data)
                          (funcall callback
                                   (loop for ch across (slack--assoc-with-check 'members data)
                                         collect (cons (slack--assoc-with-check 'id ch)
                                                       (list (slack--assoc-with-check 'description ch)
                                                             (slack--assoc-with-check 'nickname ch)
                                                             (slack--assoc-with-check 'image_name ch))))))))

(defun slack--update-userlist (users)
  (setq slack--users users)
  (dolist (v slack--pending-user-state)
    (destructuring-bind (uid new-state) v
      (slack--update-active-state-for-user-from-id uid new-state)))
  (setq slack--pending-user-state nil))

(defun slack--name-for-uid (uid)
  (let ((v (slack--assoc-with-check uid slack--users t)))
    (if v
        (car v)
      uid)))

(defun slack--send-typing-notification ()
  (let ((now (float-time)))
    (when (or (null slack--last-typing-notifcation)
              (> now (+ slack--last-typing-notifcation 2)))
      (let ((url-request-data (encode-coding-string (json-encode '((state . t))) 'utf-8)))
        (slack--url-retrieve (format "/channel/%s/type" slack--channel-id) "POST"
                              (lambda (data)
                                (unless (equal (slack--assoc-with-check 'result data) "ok")
                                  (message "Error when sending typing notification")))))
      (setq slack--last-typing-notifcation now))))

(defun slack--request-channel-list ()
  (let ((result (slack--url-retrieve-synchronous "/channels" "GET")))
    (let ((channels (loop
                     for domain across result
                     for domain-id = (slack--assoc-with-check 'id domain)
                     for domain-name = (slack--assoc-with-check 'name domain)
                     append (loop
                             for group across (slack--assoc-with-check 'groups domain)
                             for group-id = (slack--assoc-with-check 'id group)
                             for group-name = (slack--assoc-with-check 'name group)
                             append (loop
                                     for channel across (slack--assoc-with-check 'channels group)
                                     for channel-id = (slack--assoc-with-check 'id channel)
                                     for channel-name = (slack--assoc-with-check 'name channel)
                                     collect (list channel-id domain-name group-name channel-name))))))
      channels)))

(defun slack--choose-channel-id ()
  (let* ((channels (coerce (slack--request-channel-list) 'vector))
         (names-list (loop for i from 0 below (length channels)
                           for chan = (aref channels i)
                           for channel-name = (fourth chan)
                           unless (or (string-match "^Private channel for users" channel-name))
                           collect (cons (if (loop for i2 from 0 below (length channels)
                                                   when (and (/= i i2) (equal (fourth (aref channels i2)) channel-name))
                                                   return t
                                                   finally (return nil))
                                             (format "%s (%s/%s)" channel-name (second chan) (third chan))
                                           channel-name)
                                         (first chan)))))
    (let ((result (completing-read "Channel: " names-list nil t nil nil nil nil)))
      (let ((e (find result names-list :key #'car :test #'equal)))
        (unless e
          (error "Did not find selected channel"))
        (cdr e)))))

(defun slack--make-separated-string (&rest args)
  (with-output-to-string
    (loop with first = t
          for v in args
          when v
          do (progn
               (unless first
                 (princ " "))
               (princ v)
               (setq first nil)))))

(defun slack--recompute-modeline ()
  (setq slack-display-notifications-string
        (slack--make-separated-string
         (if slack--notifications
             (let ((n (reduce #'+ (mapcar #'cdr slack--notifications))))
               (if (plusp n)
                   (propertize (format "Slack:%d" n)
                               'face 'slack-notification))))
         (slack--make-unread-notification-string)))
  (force-mode-line-update t))

(defun slack--recompute-channel-modeline ()
  (setq slack--channel-mode-line
        (slack--make-separated-string
         (if slack--current-typing
             (with-output-to-string
               (if (null (cdr slack--current-typing))
                   (princ (format "%s is typing" (slack--name-for-uid (car slack--current-typing))))
                 (loop for (uid . rest) on slack--current-typing
                       if rest
                       do (princ (format "%s, " (slack--name-for-uid uid)))
                       else
                       do (princ (format "%s are typing") (slack--name-for-uid uid))))))))
  (force-mode-line-update))

(defun slack-client (channel-id)
  "Open a Slack channel for the given CHANNEL-ID."
  (interactive (list (slack--choose-channel-id)))
  (unless (and slack-api-token (plusp (length slack-api-token)))
    (user-error "Set the variable ‘slack-api-token’ before starting the Slack client."))
  (let ((buffer (slack--find-channel-buffer channel-id :create-if-missing t)))
    (switch-to-buffer buffer)))

(defun slack-close-all-sessions ()
  "Close all active Slack client buffers"
  (interactive)
  (loop for (cid . buffer) in slack--active-buffers
        do (kill-buffer buffer)))

(provide 'slack)
