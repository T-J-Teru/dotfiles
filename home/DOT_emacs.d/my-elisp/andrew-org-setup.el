;;       Org Mode Setup

(setq org-directory "~/.org")
(defun andrew-org-file (name)
  "Return NAME within ORG-DIRECTORY"
  (expand-file-name (concat org-directory "/" name)))

(add-hook 'org-mode-hook 'andrew-org-mode-hook)
(defun andrew-org-mode-hook ()
  (message "In andrew-org-mode-hook...")
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (setq org-hide-leading-stars t)
  (set-face-attribute 'org-hide nil
                      :foreground "grey30")
  (setq org-hide-emphasis-markers t)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa.jar")
  (set-face-attribute 'org-code nil
                      :inherit 'unspecified
                      :foreground "deep sky blue"))

(setq andrew-default-org-file (andrew-org-file "todo.org"))

;; Set this to nil so the default does not get used by accident
;; anywhere, I should be placing all my data into known locations.
(setq org-default-notes-file nil)

(setq org-log-into-drawer 't)

;;; Keywords
;; (t) TODO - Default initial state of things to be done.
;; (p) PROGRESSING - Being worked on.v
;; (b) BLOCKED - Waiting on something, or someone.
;; (e) DELEGATED - Have assigned this to someone else to complete.
;; (f) DEFERED - This task is on hold to a specific date/time.
;;
;; (s) SOMEDAY - Closed, will revisit this someday. Probably never.
;; (d) DONE - Task is complete.
;; (a) ABANDONDED - Task will never be completed.
;;v
;; (n) NOTE - Random snippet of information.
;; (m) MEETING - Notes on a meeting.

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESSING(p!/!)" "BLOCKED(b@/!)"
                  "DELEGATED(e@/!)" "DEFERED(f@/!)"
                  "|"
                  "DONE(d!/!)" "ABANDONED(a@/!)" "SOMEDAY(s@/!)")
        (type     "|" "NOTE(n)" "MEETING(m)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "medium blue" :weight bold)
        ("PROGRESSING" :foreground "dark green" :weight bold)
        ("BLOCKED" :foreground "red" :weight bold)
        ("DELEGATED" :foreground "dark orange" :weight bold)
        ("DEFERRED" :foreground "dark orange" :weight bold)
        ;; Finished
        ("DONE" :foreground "dark blue" :weight bold)
        ("ABANDONED" :foreground "dark blue" :weight bold)
        ("SOMEDAY" :foreground "dark blue" :weight bold)
        ;; States
        ("NOTE" :foreground "brown" :weight bold)
        ("MEETING" :foreground "brown" :weight bold)))

;; All files in ORG-DIRECTORY are agenda files.
(setq org-agenda-files (list org-directory))

;; Where to look for places to refile.
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))

(setq org-agenda-custom-commands
      '(("z" "Agenda (next 7 days)" agenda ""
         ((org-agenda-span 7)
          (org-agenda-start-on-weekday nil)
          (org-agenda-overriding-header "Agenda (next 7 days)")))
        ("p" . "Personal Views")
        ("pu" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
         ((org-agenda-overriding-header "Uncategorized items")))
        ("pb" "Blocked/delegated tasks" tags "TODO=\"BLOCKED\"|TODO=\"DELEGATED\""
         ((org-agenda-overriding-header "Blocked/delegated tasks:")
          (org-agenda-sorting-strategy
           '(todo-state-up priority-down category-up))))
        ("pd" "Deferred tasks" tags "TODO=\"DEFERRED\""
         ((org-agenda-overriding-header "Deferred tasks:")))
        ("pa" "Abandoned tasks" tags "TODO=\"ABANDONED\""
         ((org-agenda-overriding-header "Abandoned tasks:")))
        ("pm" "Meetings" agenda ""
         ((org-agenda-overriding-header
           "Completed Meetings:")
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'nottodo
                                      '("MEETING")))
          (org-agenda-skip-scheduled-if-done nil)))
        ("pn" "Notes" tags "TODO=\"NOTE\""
         ((org-agenda-overriding-header "Notes")
          (org-agenda-sorting-strategy '(time-down))))
        ("ps" "Someday tasks" tags "TODO=\"SOMEDAY\""
         ((org-agenda-overriding-header "Someday tasks:")))
        ))

(require 'andrew-work-log)

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline andrew-default-org-file "Inbox")
         "** TODO %?
   SCHEDULED: %t
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("m" "Meeting / Phone-Call" entry
         (file+headline andrew-default-org-file "Inbox")
         "** MEETING %?                                                            :meeting:
   CLOSED: %U SCHEDULED: %t
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("n" "Note" entry
         (file+headline (andrew-org-file "notes.org") "Notes")
         "** NOTE %?
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("f" "Film Related Task" entry
         (file+headline (andrew-org-file "media.org") "Media")
         "** TODO %?
   SCHEDULED: %t
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("l" "Work Log Entry" item
         (function andrew-work-log/find-log-entry)
;;         (file+olp (andrew-org-file "work-log.org") "Work Log" "Monday, September 22, 2014")
         nil :kill-buffer t)
        ))

;; Can't mark a parent as DONE until all its children are DONE.
(setq org-enforce-todo-dependencies t)

;; When clocking in and out, if, on clocking out the time spent is
;; zero, then delete the clock line.
(setq org-clock-out-remove-zero-time-clocks t)

;; Where to place clock entries.
(setq org-clock-into-drawer "LOGBOOK")

;; Log when an issue is closed.
(setq org-log-done 'time)

;; Reduce clutter in the agenda view.
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; Just skip unreachable file.
(setq org-agenda-skip-unavailable-files t)

(provide 'andrew-org-setup)
