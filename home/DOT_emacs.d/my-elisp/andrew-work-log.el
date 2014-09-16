(defvar andrew-work-log/root-dir "~/Documents/work-log")

(defun andrew-work-log/log-file-path (&optional date)
  "Return path to work log file for DATE.
Returns the path to the log file being used for DATE. If DATE is
not given, or is nil, then return the log file for the CURRENT-TIME."
  (unless date
    (setq date (current-time)))
  (let* ((time-list (decode-time date))
         (month (nth 4 time-list))
         (year (nth 5 time-list)))
    (format "%s/%d/%02d-%s.org"
            andrew-work-log/root-dir
            year
            month
            (format-time-string "%B" date))))

(defun andrew-work-log/log-entry-olp (&optional date)
  "Return the outline path for log entry on DATE.
Return the heading text used for the log entry of DATE.  If DATE
is not given, or is nil, then use the CURRENT-TIME."
  (unless date
    (setq date (current-time)))
  (list (format-time-string "Week %W")
        (format-time-string "%A %-e %B, %Y" date)))

(defun andrew-work-log/find-log-entry (&optional date)
  (interactive)
  (unless date
    (setq date (current-time)))
  (let ((log-file-path (andrew-work-log/log-file-path date)))
    ;; If the LOG-FILE-PATH does not exist, then create it, perform
    ;; initialisation, and save the file.
    (when (and (not (get-file-buffer log-file-path))
               (not (file-exists-p log-file-path)))
      (make-directory (file-name-directory log-file-path) t)
      (find-file log-file-path)
      (andrew-work-log/init)
      (save-buffer))
    ;; Now just jump to the correct entry.
    (let ((m (org-find-olp
              (cons (andrew-work-log/log-file-path date)
                    (andrew-work-log/log-entry-olp date)))))
      (set-buffer (marker-buffer m))
      (widen)
      (goto-char m))))

(defun andrew-work-log/init ()
  "Generate template work log for current month into current buffer.
Will only perform initialisation if the current buffer is empty."
  (interactive)
  (unless (eq (point-min) (point-max))
    (error "Can only perform initialisation in an empty buffer."))
  (let* ((curr-time (current-time))
         (time-detail (decode-time curr-time))
         (month (nth 4 time-detail))
         (year (nth 5 time-detail))
         (day 1)
         (week 1)
         (time (encode-time 1 1 0 day month year))
         (curr-week-header nil))
    ;; While we're still in the same month.
    (while (= (nth 4 (decode-time time)) month)
      ;; Build the week header.
      (let ((week-header (format-time-string "* Week %W\n" time)))
        ;; Possibly insert the week header.
        (when (or (not curr-week-header)
                (not (string= curr-week-header week-header)))
          (insert week-header)
          (setq curr-week-header week-header)))
      ;; If it's not a weekend day, then insert day entry.
      (unless (or (string= (format-time-string "%w" time) "0")
                  (string= (format-time-string "%w" time) "6"))
        (insert (format-time-string "** %A %-e %B, %Y\n" time)))
      (setq day (1+ day))
      (setq time (encode-time 1 1 0 day month year)))))

(defun andrew-work-log/visit-log-entry (&optional date)
  "Open up work log for DATE.
If DATE is nil, or not specified, then open the work-log for the
current date."
  (interactive)
  (unless date
    (setq date (current-time)))
  (andrew-work-log/find-log-entry date)
  (switch-to-buffer (current-buffer))
  (hide-sublevels 1)
  (org-show-entry)
  (org-show-siblings))

(defun andrew-work-log/forward-week (&optional count)
  "When viewing a work log, move forward, or backward COUNT weeks."
  (interactive)
  (let ((file-name (expand-file-name (buffer-file-name)))
        (log-dir (expand-file-name andrew-work-log/root-dir)))
    (unless file-name
      (error "Not a work log file: No file name"))
    (unless (string= (substring-no-properties file-name
                                              nil
                                              (length log-dir))
                     log-dir)
      (error "Not a work log file: Incorrect directory"))
    ;; Find heading at the correct level.
    (outline-back-to-heading)
    (outline-up-heading (- (outline-level) 2) t)
    (unless (= (outline-level) 2)
      (error "Not a work log file: Couldn't find level 2 heading"))
    ;; Extract the heading, and reconstruct a date.
    ;; Modify the date by COUNT weeks.
    ;; Visit the correct work log file.
    (message "File Name: %s" file-name))
  (error "Not implemented yet"))

(provide 'andrew-work-log)
