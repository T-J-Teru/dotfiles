(defface notmuch-crypto-inline-pgp-marker
  '((t (:background "seagreen1" :foreground "black")))
  "Face used for marking start/end of inline pgp blocks"
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defface notmuch-crypto-inline-pgp-content
  '((t (:background "lavender")))
  "Face used for displaying inline pgp content"
  :group 'notmuch-crypto
  :group 'notmuch-faces)

(defun andrew/notmuch--find-next-inline-block (config)
  (let ((start nil)
        (end nil)
        (start-marker (plist-get config :start))
        (end-marker (plist-get config :end)))
    (when (re-search-forward (concat "^" start-marker) nil t)
      (beginning-of-line)
      (setq start (point)))
    (when (re-search-forward (concat "^" end-marker) nil t)
      (end-of-line)
      (setq end (point)))
    (if (and start end)
        (cons start end)
      nil)))

(defun andrew/notmuch--insert-banner (text face)
  (let ((start (point)))
    (insert text)
    (add-text-properties start (point) (list 'face face))
    (insert "\n")))

(defun andrew/notmuch--do-epg-func-on-region (context start end func)
  (let ((plain nil))
    (condition-case ex
        (setq plain (decode-coding-string
                     (funcall func context
                              (buffer-substring start end))
                     'utf-8))
      (error nil))
    plain))

(defun andrew/notmuch--decrypt-region (config start end)
  (let* ((context (epg-make-context 'OpenPGP))
         (plain (andrew/notmuch--do-epg-func-on-region
                 context start end 'epg-decrypt-string)))
    (goto-char start)
    (if plain
        (delete-region start end))
    (let ((encstatus (list :status (if plain "good" "bad"))))
      (notmuch-crypto-insert-encstatus-button encstatus))
    (if (epg-context-result-for context 'verify)
        (let* ((sig (epg-context-result-for context 'verify))
               (msg (plist-get config :message))
               (from (notmuch-show-get-header :From msg))
               (sigstatus (list
                           :fingerprint (epg-signature-fingerprint
                                         (car sig))
                           :status (epg-signature-status (car sig))
                           :userid from
                           :keyid (epg-signature-key-id (car sig)))))
          (notmuch-crypto-insert-sigstatus-button sigstatus from)))
    (when plain
      (andrew/notmuch--insert-banner
       "-----BEGIN PGP MESSAGE-----"
       'notmuch-crypto-inline-pgp-marker)
      (let ((plain-start (point)))
        (insert plain)
        (if (not (bolp))
            (insert "\n"))
        (add-text-properties
         plain-start (point)
         '(face notmuch-crypto-inline-pgp-content)))
      (andrew/notmuch--insert-banner
       "-----END PGP MESSAGE-----"
       'notmuch-crypto-inline-pgp-marker))))

(defun andrew/notmuch--verify-region (config start end)
  (let* ((context (epg-make-context 'OpenPGP))
         (plain (andrew/notmuch--do-epg-func-on-region
                 context start end 'epg-verify-string)))
    (if (epg-context-result-for context 'verify)
        (let* ((sig (epg-context-result-for context 'verify))
               (msg (plist-get config :message))
               (from (notmuch-show-get-header :From msg))
               (sigstatus (list
                           :fingerprint (epg-signature-fingerprint
                                         (car sig))
                           :status (epg-signature-status (car sig))
                           :userid from
                           :keyid (epg-signature-key-id (car sig)))))
          (goto-char start)
          (delete-region start end)
          (notmuch-crypto-insert-sigstatus-button sigstatus from)
          (andrew/notmuch--insert-banner
           "-----BEGIN PGP SIGNED MESSAGE-----"
           'notmuch-crypto-inline-pgp-marker)
          (let ((plain-start (point)))
            (insert plain)
            (add-text-properties
             plain-start (point)
             '(face notmuch-crypto-inline-pgp-content)))
          (andrew/notmuch--insert-banner
           "-----END PGP SIGNED MESSAGE-----"
           'notmuch-crypto-inline-pgp-marker)
          (goto-char (point-max))))))

(defun andrew/notmuch--process-next-inline-pgp (config)
  (let ((inline-pgp (andrew/notmuch--find-next-inline-block config))
        (handler (plist-get config :handler)))
    (when inline-pgp
      (let ((end-marker (make-marker))
            (start (car inline-pgp))
            (end (cdr inline-pgp)))
        (set-marker-insertion-type end-marker t)
        (move-marker end-marker end)
        (funcall handler config start end)
        (goto-char (marker-position end-marker)))
      (andrew/notmuch--process-next-inline-pgp config))))

(defun andrew/notmuch--process-buffer (&rest config)
  (goto-char (point-min))
  (andrew/notmuch--process-next-inline-pgp config))

(defun andrew/notmuch-decrypt-inline-pgp (msg depth)
  (andrew/notmuch--process-buffer
   :message msg
   :start "-----BEGIN PGP MESSAGE-----"
   :end "-----END PGP MESSAGE-----"
   :handler 'andrew/notmuch--decrypt-region))

(defun andrew/notmuch-verify-inline-pgp (msg depth)
  (andrew/notmuch--process-buffer
   :message msg
   :start "-----BEGIN PGP SIGNED MESSAGE-----"
   :end "-----END PGP SIGNATURE-----"
   :handler 'andrew/notmuch--verify-region))

(defun andrew/notmuch-load-additional-searches (&optional filename)
  "Load contents of FILENAME and add them to `notmuch-saved-searches'.

The contents of FILENAME are loaded, and each line is treated as
a list that defines a search as would appear in
`notmuch-saved-searches'.  Each search is loaded and added to the
`notmuch-saved-searches' variable.

If FILENAME is not passed then the file ~/.notmuch-searchs is loaded."
  ;; Ensure that we have a filename.
  (unless filename
    (setq filename "~/.notmuch-searches"))
  ;; Load the contents of the file, and add them to the searches list.
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (while
            (not (equal (point) (point-max)))
          (let ((start (line-beginning-position))
                (end (line-end-position)))
            (ignore-errors
              (let ((lst
                     (read-from-string
                      (buffer-substring start end))))
                (add-to-list 'notmuch-saved-searches (car lst) t))))
          (forward-line)))))

(defvar andrew/notmuch-hello-sections
  '(notmuch-hello-insert-header
    notmuch-hello-insert-saved-searches
    notmuch-hello-insert-search
    notmuch-hello-insert-recent-searches
    (notmuch-hello-insert-tags-section
     "All tags (all mail)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter notmuch-hello-tag-list-make-query)
    (notmuch-hello-insert-tags-section
     "All tags (all unread)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter "tag:unread")
    (notmuch-hello-insert-tags-section
     "All tags (last hour/unread)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter "date:1h.. AND tag:unread")
    (notmuch-hello-insert-tags-section
     "All tags (last 6hrs/unread)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter "date:6h.. AND tag:unread")
    (notmuch-hello-insert-tags-section
     "All tags (last 12hrs/unread)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter "date:12h.. AND tag:unread")
    (notmuch-hello-insert-tags-section
     "All tags (last 24hrs/unread)"
     :search-type tree
     :initially-hidden (not notmuch-show-all-tags-list)
     :hide-tags notmuch-hello-hide-tags
     :filter "date:24h.. AND tag:unread")
    notmuch-hello-insert-footer)
  "The value in this variable replaces `notmuch-hello-sections'
during startup of `notmuch'.")

(provide 'andrew-notmuch)
