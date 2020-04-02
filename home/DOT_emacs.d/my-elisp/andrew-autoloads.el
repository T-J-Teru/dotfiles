;;; andrew-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "andrew-after-save" "andrew-after-save.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-after-save.el

(autoload 'andrew/make-executable-after-save "andrew-after-save" "\
Called when a file is saved.  Makes the file executable if it
looks like a script.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "andrew-cc-mode" "andrew-cc-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from andrew-cc-mode.el

(autoload 'andrew-cc-mode "andrew-cc-mode" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-cc-mode" '("andrew" "font-lock-format-specifier-face")))

;;;***

;;;### (autoloads nil "andrew-cperl-mode" "andrew-cperl-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-cperl-mode.el

(autoload 'andrew-cperl-mode "andrew-cperl-mode" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-cperl-mode" '("andrew-cperl")))

;;;***

;;;### (autoloads nil "andrew-diff-mode" "andrew-diff-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from andrew-diff-mode.el

(autoload 'andrew-diff-mode "andrew-diff-mode" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "andrew-email-mode" "andrew-email-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-email-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-email-mode" '("andrew-email-mode")))

;;;***

;;;### (autoloads nil "andrew-format-comment" "andrew-format-comment.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-format-comment.el

(autoload 'andrew/reformat-comment "andrew-format-comment" "\
Reformt a comment line /****  TEXT ****/

\(fn)" t nil)

;;;***

;;;### (autoloads nil "andrew-forth-mode" "andrew-forth-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-forth-mode.el

(autoload 'andrew-forth-mode "andrew-forth-mode" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "andrew-javascript-mode" "andrew-javascript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-javascript-mode.el

(autoload 'andrew-javascript-mode "andrew-javascript-mode" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "andrew-latex-mode" "andrew-latex-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-latex-mode.el

(autoload 'andrew-latex-mode "andrew-latex-mode" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "andrew-notmuch" "andrew-notmuch.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from andrew-notmuch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-notmuch" '("andrew/notmuch-")))

;;;***

;;;### (autoloads nil "andrew-org" "andrew-org.el" (0 0 0 0))
;;; Generated autoloads from andrew-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-org" '("org-" "andrew-org-space")))

;;;***

;;;### (autoloads nil "andrew-org-capture" "andrew-org-capture.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from andrew-org-capture.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-org-capture" '("org-capture-place-item")))

;;;***

;;;### (autoloads nil "andrew-org-setup" "andrew-org-setup.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from andrew-org-setup.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-org-setup" '("andrew-org/")))

;;;***

;;;### (autoloads nil "andrew-r-mode" "andrew-r-mode.el" (0 0 0 0))
;;; Generated autoloads from andrew-r-mode.el

(autoload 'andrew-r-mode "andrew-r-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "andrew-work-log" "andrew-work-log.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from andrew-work-log.el

(autoload 'andrew-work-log/visit-log-entry "andrew-work-log" "\
Open up work log for DATE.
If DATE is nil, or not specified, then open the work-log for the
current date.

\(fn &optional DATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "andrew-work-log" '("andrew-work-log/")))

;;;***

;;;### (autoloads nil "menu-toggle" "menu-toggle.el" (0 0 0 0))
;;; Generated autoloads from menu-toggle.el

(autoload 'toggle-menubar-and-toolbar "menu-toggle" "\
Switches off both menu and tool bar.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mparen" "mparen.el" (0 0 0 0))
;;; Generated autoloads from mparen.el

(autoload 'jump-to-matching-paren "mparen" "\
Go to the matching parenthesis if on parenthesis otherwise insert a hash.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("andrew-ispell.el") (0 0 0 0))

;;;***

(provide 'andrew-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; andrew-autoloads.el ends here
