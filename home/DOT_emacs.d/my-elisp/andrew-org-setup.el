(require 'org)

(setq andrew-org/org-directory "~/.org")

(defun andrew-org/project-directory (project)
  "Return path to directory for project PROJECT"
  (expand-file-name (concat andrew-org/org-directory "/" project)))

(defun andrew-org/filename (project service)
  "Return path to file for SERVICE within PROJECT"
  (expand-file-name (concat (andrew-org/project-directory project)
                            "/" service ".org")))

(defun andrew-org/is-legal-project-name (project)
  "Return TRUE if PROJECT is a legal project name.

Valid project names start with a letter (lower or upper case),
then are optionally followed by zero or more letters (upper or
lower case), numbers, dashes, or underscores.  A valid project
name however, must not end in a dash or underscore."
  (string-match "^[a-zA-Z]\\([_-0-9a-zA-Z]*[0-9a-zA-Z]\\)?$" project))

(defvar andrew-org/services nil)
(setq andrew-org/services nil)

(defun andrew-org/register-service (service handlers)
  "blah..."
  (if (assoc service andrew-org/services)
      (error "Service `%s' is already registered" service))
  ;; Check each action in the handlers is valid.

  ;; Now we can register this service.
  (setq andrew-org/services (acons service handlers
                             andrew-org/services))
  )

(defun andrew-org/insert-type-todo-setting (type-todo)
  (insert (format "#+TYP_TODO: | %s\n" type-todo)))

(defun andrew-org/init-service-generic (project heading)
  (insert (format "\
* %s/%s
  :PROPERTIES:
  :CATEGORY: %s
  :END:
" project heading project)))

(defun andrew-org/init-service-notes (project)
  (andrew-org/insert-type-todo-setting "NOTE(n)")
  (andrew-org/init-service-generic project "notes"))

(defun andrew-org/init-service-meetings (project)
  (andrew-org/insert-type-todo-setting "MEETING(m)")
  (andrew-org/init-service-generic project "meetings"))

(defun andrew-org/init-service-expenses (project)
  (andrew-org/insert-type-todo-setting "EXPENSE(e)")
  (andrew-org/init-service-generic project "expenses"))

(defun andrew-org/init-service-tasks (project)
  (andrew-org/init-service-generic project "tasks"))

(andrew-org/register-service 'notes
                             '((init . andrew-org/init-service-notes)
                               (open . nil)))

(andrew-org/register-service 'meetings
                             '((init . andrew-org/init-service-meetings)
                               (open . nil)))

(andrew-org/register-service 'expenses
                             '((init . andrew-org/init-service-expenses)
                               (open . nil)))

(andrew-org/register-service 'tasks
                             '((init . andrew-org/init-service-tasks)
                               (open . nil)))

(defun andrew-org/create-new-project (project)
  "Setup files and directories for new project PROJECT"
  (interactive "sproject: ")

  ;; Some basic sanity checks to get things started.
  (unless (file-exists-p andrew-org/org-directory)
    (error "Org directory `%s' does not exist"
           andrew-org/org-directory))
  (if (file-exists-p (andrew-org/project-directory project))
      (error "Org project `%s' already exists in `%s'"
             project andrew-org/org-directory))
  (unless (andrew-org/is-legal-project-name project)
    (error "Org project name `%s' is not valid" project))

  ;; We now have a valid project name, for a project that does not
  ;; otherwise exist.  Create the directory for this project and
  ;; populate it with the initial files for each service.
  (make-directory (andrew-org/project-directory project))

  ;; For every service create the filename within the project, open
  ;; the buffer, and call the init callback.
  (mapcar
   (lambda (service-and-handlers)
     (let ((service (car service-and-handlers))
           (handlers (cdr service-and-handlers)))
       (with-temp-file (andrew-org/filename project (symbol-name service))
         (let ((init-handler (cdr (assq 'init handlers))))
           (if init-handler
               (funcall init-handler project))))))
   andrew-org/services)

  ;; Rebuild the org-agenda-files list to pick up the new project.
  (andrew-org/rebuild-org-agenda-files)

  ;; All done.
  (message "Org project `%s' created..." project))

;; Set this to nil so the default does not get used by accident
;; anywhere, I should be placing all my data into known locations.
(setq org-default-notes-file nil)

(defun andrew-org/find-org-agenda-files (filename)
  "Find org files named `filename' from all projects.

Looks in `andrew-org/org-directory' for all org files called
`filename' and return a list of all the paths."
  (if (eq filename "")
      (error "No filename"))
  (let ((files '()))
    (dolist (entry (reverse
                    (directory-files andrew-org/org-directory t)))
      (if (file-directory-p entry)
          (let ((tasks-file-path (concat entry "/" filename)))
            (if (file-readable-p tasks-file-path)
                (setq files (cons tasks-file-path files))))))
    files))

(defun andrew-org/rebuild-org-agenda-files ()
  "Rebuild the value for `org-agenda-files'.

Looks in `andrew-org/org-directory' for all todo list related files
and adds them to the `org-agenda-files' list"
  (interactive)
  (setq org-agenda-files
        (andrew-org/find-org-agenda-files "tasks.org")))

(defun andrew-org/current-service-name ()
  "Return service name of current buffer.

TODO: This should do more to confirm that the current buffer is
within a project, and possibly even that the file looks like a
service file."
  (file-name-base (buffer-file-name)))

(defun andrew-org/org-refile-targets ()
  "Find list of refile targets.

Finds all files in all projects that have the same filename as the
current buffer."
  (let ((service-name (andrew-org/current-service-name))
        (target-list '()))
    (dolist (entry (reverse
                    (directory-files andrew-org/org-directory t)))
      (if (file-directory-p entry)
          (let ((service-file-path (concat entry "/" service-name ".org")))
            (if (file-readable-p service-file-path)
                (setq target-list
                     (cons service-file-path target-list))))))
    target-list))

(setq org-refile-targets '((andrew-org/org-refile-targets . (:maxlevel . 1))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESSING(p!/!)" "BLOCKED(b@/!)"
                  "DELEGATED(e@/!)" "DEFERED(f@/!)"
                  "|"
                  "DONE(d!/!)" "ABANDONED(a@/!)" "SOMEDAY(s@/!)")))

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

(setq org-capture-templates
      '(("t" "TASK that needs completing" entry
         (file+headline (concat andrew-org/org-directory "/inbox/tasks.org") "inbox/tasks")
         "** TODO %?
   SCHEDULED: %t
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("m" "Record of a MEETING or phone-call" entry
         (file+headline (concat andrew-org/org-directory "/inbox/meetings.org") "inbox/meetings")
         "** MEETING %?                                                            :meeting:
   CLOSED: %U SCHEDULED: %t
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("n" "NOTE some helpful information" entry
         (file+headline (concat andrew-org/org-directory "/inbox/notes.org") "inbox/notes")
         "** NOTE %?
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)
        ("e" "An EXPENSES record" entry
         (file+headline (concat andrew-org/org-directory "/inbox/expenses.org") "inbox/expenses")
         "** NOTE %?
   :PROPERTIES:
   :ID:       %(shell-command-to-string \"uuidgen\")   :CREATED:  %U
   :END:
   " :prepend t :kill-buffer t)))

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
          (org-agenda-skip-scheduled-if-done nil)
          (org-agenda-files
           (andrew-org/find-org-agenda-files "meetings.org"))))
        ("pn" "Notes" tags "TODO=\"NOTE\""
         ((org-agenda-overriding-header "Notes")
          (org-agenda-sorting-strategy '(time-down))
          (org-agenda-files
           (andrew-org/find-org-agenda-files "notes.org"))))
        ("ps" "Someday tasks" tags "TODO=\"SOMEDAY\""
         ((org-agenda-overriding-header "Someday tasks:")))
        ))

(defun andrew-org/org-mode-hook ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-ditaa-jar-path "/usr/share/java/ditaa.jar")
  ;; Rebuild the list of agenda files.
  (andrew-org/rebuild-org-agenda-files)
  ;; Place notes and timestamps into a draw.  Keeps the clutter down.
  (setq org-log-into-drawer 't)
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
  (set-face-attribute 'org-hide nil
                      :foreground "grey30")
  (set-face-attribute 'org-code nil
                      :inherit 'unspecified
                      :foreground "deep sky blue"))

(defun andrew-org/agenda-mode-hook ()
  (setq header-line-format
        (substitute-command-keys
         "Filter by category: '\\[org-agenda-filter-by-category]', \
Remove filter: '\\[org-agenda-filter-remove-all]'"))

  (insert
   (substitute-command-keys "\
Filter by:
  category	\\[org-agenda-filter-by-category]
  regexp	\\[org-agenda-filter-by-regexp]
  tag		\\[org-agenda-filter-by-tag]

  remove all	\\[org-agenda-filter-remove-all]

"))
  (hl-line-mode 1))

;; Register hooks
(add-hook 'org-agenda-mode-hook 'andrew-org/agenda-mode-hook)
(add-hook 'org-mode-hook 'andrew-org/org-mode-hook)

(provide 'andrew-org-setup)
