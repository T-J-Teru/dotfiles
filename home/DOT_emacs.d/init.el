;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Frame Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable the startup splash screen.
(setq inhibit-splash-screen t)

;; Default settings to use when creating a new frame.
(setq default-frame-alist
      `(;; menu-bar-lines and tool-bar-lines are how old
        ;; versions of emacs (before 24) initialised the
        ;; menu & tool bars.  Now we just check the
        ;; values of menu-bar-mode and tool-bar-mode.
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        ;; Select a suitable font.
        ;; (font . "Liberation Mono 10")
        (font . "Ubuntu Mono 10")
        ;; And force the startup frame width
        (width . 80)))

;; Disable both horizontal and vertical scroll bars.  Older versions
;; of emacs don't support horizontal scroll bars, hence the extra care
;; in this case.
(set-scroll-bar-mode nil)
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

;; Setup the window title format.
(setq frame-title-format
      (list "emacs: "
            '(buffer-file-name
             "%f"
             (dired-directory dired-directory "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     Control Use Of System Startup Configuration Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't load the 'default.el' system startup file.
(setq inhibit-default-init t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Package System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup the emacs package system.
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use "a2ps" for printing, setup suitable command line switches.
;;
;; TODO: Should probably make this smarter, only selecting a2ps if the
;; program is actually available.
(setq lpr-command "a2ps")
(setq lpr-switches '("--sides=duplex" "-r" "--columns=2"))

;; Set to `nil' as I'm not using "lpr" as for `lpr-command'.
(setq lpr-add-switches nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Load Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure the emacs load-path, adding directories and loading new
;; features as required.
(defconst elisp-directory (expand-file-name "~/.emacs.d"))
(defmacro add-to-load-path (directory &rest body)
  "If DIRECTORY exists then it is added to the `load-path', and
then BODY is evaluated.  If directory does not exist then the
`load-path' is not changed, and BODY is not evaluated."
  `(when (file-exists-p
          (concat elisp-directory "/" ,directory))
     (add-to-list 'load-path (concat elisp-directory "/" ,directory))
     ,@body))

(add-to-load-path "cedet" (require 'cedet-devel-load))
(add-to-load-path "packages")
(add-to-load-path "my-elisp")
(add-to-load-path "org-mode/lisp")
(add-to-load-path "cc-mode")
(add-to-load-path "notmuch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        My Autoloads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'andrew-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Mode Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setting up the modeline
(setq line-number-mode t)
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Mini-Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make the mini-buffer prompt readonly, and place the cursor outside
;; of the readonly text.
(setq minibuffer-prompt-properties
      '(read-only t
        point-entered minibuffer-avoid-prompt
        face minibuffer-prompt))

;; Use `icomplete-mode' and the additional `icomplete+' package for
;; minibuffer completion.
(require 'icomplete+ nil t)
(icomplete-mode)

;; Arrange to make scripts executable when they are first saved.
(autoload 'andrew/make-executable-after-save "andrew-after-save")
(add-hook 'after-save-hook 'andrew/make-executable-after-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Disabled Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following commands are disabled by default, but I do use them,
;; so remove the disabled marker.  This allows me to use them without
;; getting the annoying "this command is disabled" prompt.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Line Truncation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on line truncation by default.  This means long lines will be
;; cut short at the right hand edge of the screen.
(set-default `truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Cursor Overwrite Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Colors to use when the cursor is in non-overwrite (default) and
;; overwrite mode.
;;
;; TODO: Should try to also change the `mc/cursor-face' to something
;; similar, so that the multiple cursors packages appears to be themed
;; in.
(defvar cursor-default-colour "LimeGreen")
(defvar cursor-overwrite-colour "red")

(defun cursor-overwrite-mode ()
  "Set cursor colour according to insert mode"
  (set-cursor-color
   (if overwrite-mode
       cursor-overwrite-colour
       cursor-default-colour)))
(add-hook 'overwrite-mode-hook 'cursor-overwrite-mode)
(set-cursor-color cursor-default-colour)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               Dos To Unix Text Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper fucnion for converting from DOS to UNIX style line endings.
(defun dos2unix ()
  "Convert dos formatted buffer to unix formatted buffer by
removing\nany \\r characters."
  (interactive)
  (let ((current-point-position (point)))
    (progn
      (message "dos2unix: Converting dos characters '\r' to ''")
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))
      (goto-char current-point-position)))
      (message "dos2unix: done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;         Cursor Shows Mark Active / Inactive State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cursor-activate-mark ()
  "Function to call when the mark is activated."
  (progn
    (setq cursor-type 'bar)))
(add-hook 'activate-mark-hook 'cursor-activate-mark)

(defun cursor-deactivate-mark ()
  "Function to call when the mark is deactivated."
  (progn
    (setq cursor-type 'box)))
(add-hook 'deactivate-mark-hook 'cursor-deactivate-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  Spell Checking Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the spell checker to use, the alternative is ispell, but
;; aspell seems to give better suggestions.
(setq-default ispell-program-name "aspell")

;; Make sure that we pick up the correct dictionary name.
;; In truth only the ispell-dictionary needs to be set, but
;; it is nice to set them both just to be on the safe side.
(cond
 ((string= ispell-program-name "aspell")
  ;; aspell has 'british' dictionary.
  (progn (setq flyspell-default-dictionary "british")
         (setq ispell-dictionary "british")))
 ((string= ispell-program-name "ispell")
  ;; ispell calls it an 'english' dictionary.
  (progn (setq flyspell-default-dictionary "english")
         (setq ispell-dictionary "english")))
 (t (error "Unknown ispell program name `%s'"
           ispell-program-name)))

;; The `andrew-ispell' library adds some additional spell checking
;; functions.
(require 'andrew-ispell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    How To Show Parenthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on showing of parenthesis
(show-paren-mode t)

;; Select how we show parenthesis based on the type of display we are
;; using.  The `expression' display does not work well in terminals,
;; where we usually have fewer colours available.
(if (display-graphic-p)
    (setq show-paren-style 'expression)
  (setq show-paren-style 'parenthesis))

;; Customise the face used for showing matching parenthesis.
;;
;; TODO: Not sure if this is the best way to configure faces these
;; days.
(if (display-graphic-p)
    (set-face-attribute 'show-paren-mismatch-face
                        nil
                        :strike-through "red"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                 Tracking Position Of Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When active `winpoint-mode' tracks the position of point per
;; window, rather than per-buffer.  This means that if two windows (W1
;; and W2) are both visiting buffer A, and they have different values
;; for `point', then in one window (W1) switches to buffer B and then
;; back to buffer A, the value for `point' in W1 will be preserved.
;; The standard behaviour is that when W1 revisits buffer A it would
;; take on the value of `point' from W2.
(when (require 'winpoint nil t)
  (winpoint-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Line Numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pull in line numbering support.
;;
;; TODO: This should be lazily loaded only when line numbering is
;; activated for the first time.
(require 'linum+)
(setq linum-narrow-relative nil)

;; Configure how line numbers are displayed.
(if window-system
  (setq linum+-smart-format "%%%dd"
        linum+-dynamic-format "%%%dd"))

;; Allow the line number of the curent line to be highlighted.
;;
;; TODO: This should be lazily loaded as part of activating line
;; number mode.
(when (require 'hlinum nil t)
  (hlinum-activate))

;; KEYBINDING: Turns line number mode on/off.
(global-set-key (kbd "C-c n") 'linum-mode)
(global-set-key (kbd "C-c N") 'global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Template System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The template system presents a template file when a new file is
;; created with a particular file extension.
;;
;; TODO: Is is possible to lazily load this at all? Is there any kind
;; of new file hook that we can use?
(require 'template)
(template-initialize)
(setq template-auto-update-disable-regexp ".ido.last")
(setq template-auto-update nil)
(setq template-message-buffer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Email Related Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup the users email address by pulling it from the EMAIL
;; environment variable.  Gives a warning if the EMAIL environment
;; variable is not set.
(if (not (getenv "EMAIL"))
    (progn
      (display-warning
       :warning
       "Missing EMAIL environment variable, setting email to <invalid>.")
      (setq user-mail-address "<invalid>")))

;; Load my email related settings after loading the sendmail package.
(eval-after-load "sendmail"
  (lambda ()
    (message "Loading andrew-email-mode")
    (require 'andrew-email-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Buffer Switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The `list-buffers' function is used when emacs starts up on
;; multiple files, in this case I want to use `ibuffer' instead of the
;; standard buffer selector.
(defalias 'list-buffers 'ibuffer)

;; Turn on line highlighting in the `ibuffer' window.  Also activate
;; the auto update featue of `ibuffer'.
(add-hook 'ibuffer-mode-hook (lambda ()
                               (hl-line-mode 1)
                               (ibuffer-auto-mode)))

;; Advise the ibuffer redisplay engine to force update of the line
;; highlighting, this resolves an issue where the highlight is not
;; displayed correctly when the ibuffer buffer is first shown.
(eval-after-load "hl-line"
  '(defadvice ibuffer-redisplay-engine
       (after andrew-ibuffer-redisplay activate)
     (hl-line-highlight)))

;; KEYBINDING: Use `ibuffer' for buffer switching.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      PERL Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following needs to be done before `cperl-mode' is loaded as
;; these variables are read once at initialisation time, after which,
;; changing the value has no effect.  As such, making these changes in
;; the `cperl-mode-hook' has no effect.
(if (>= emacs-major-version 24)
    (progn
      (defvar cperl-invalid-face nil)
      (setq cperl-highlight-variables-indiscriminately 't)))

;; Now load my cperl customisations when entering cperl-mode.
(add-hook 'cperl-mode-hook 'andrew-cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Various Mode Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'diff-mode-hook 'andrew-diff-mode)
(add-hook 'javascript-mode-hook 'andrew-javascript-mode)
(add-hook 'forth-mode-hook 'andrew-forth-mode)
(add-hook 'c-mode-hook 'andrew-cc-mode)
(add-hook 'c++-mode-hook 'andrew-cc-mode)
(add-hook 'latex-mode-hook 'andrew-latex-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Line Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEYBINDING: Allow line highlighting to be toggled on/off.
(global-set-key (kbd "C-c h") 'hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Highlighting Whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure whitespace mode to highlight trailing whitespace, and
;; turn on whitespace mode.
(setq whitespace-style '(face trailing))
(global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Version Control Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This just turns off the emacs version control handling.
;;
;; TODO: Would be nice to try and setup and use magit mode.  I took a
;; quick look, and it seems pretty neat.
(setq vc-make-backup-files t)
(setq vc-handled-backends nil)
(setq version-control 'never)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Configure Grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tweak the grep configuration to ignore backup files.
(eval-after-load "grep"
  (lambda ()
    (grep-apply-setting
     'grep-command
     "grep --exclude='*~' --exclude='.#*' -IHn -e ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Configure avy Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: The face configuration part of this would be better done in a
;; theme.
(with-eval-after-load 'avy
  (setq avy-background 't)
  (set-face-attribute 'avy-lead-face nil
  :foreground "red"
  :background 'unspecified))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Other Window Backwards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function for moving through the windows backwards.
(defun other-window-backward ()
  "Like other window, but go backwards."
  (interactive)
  (other-window -1))

;; KEYBINDING: Cycle throgh windows backwards.
(global-set-key (kbd "C-x O") 'other-window-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               (concat elisp-directory "/auto-complete/dict/"))
  (ac-config-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Indention Mode? Tabs Or Spaces?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Though I turn this off, this is not really the best solution.
;; I do want this off for all of my personal projects (as I think
;; indentation with tabs sucks) but GNU projects use mixed tabs and
;; spaces mode.  Really I should create a function that figures out
;; which project I'm working on and switches this on or off
;; accordingly.
(set-default `indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Global Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEYBINDING: Grow and shrink windows
(global-set-key (kbd "C-c <up>") 'enlarge-window)
(global-set-key (kbd "C-c <down>") 'shrink-window)

;; KEYBINDING: Just to matching parenthesis
(global-set-key (kbd "C-c #") 'jump-to-matching-paren)

;; KEYBINDING: Toggle menubar and toolbar
(global-set-key (kbd "<f12>") 'toggle-menubar-and-toolbar)

;; KEYBINDING: Jump to work or sub-word based on first letter
(global-set-key (kbd "C-c /") 'avy-goto-word-or-subword-1)

;; KEYBINDING: Quickly jump to a specific character in buffer.
(global-set-key (kbd "C-c .") 'avy-goto-char)

;; KEYBINDING: Just to specific window using shortcut key.
;;
;; TODO: There is a slight bug with this package and the fci-mode
;; package, sometimes the number appears to the right of the width
;; marker.  Would be nice if I could find a fix or work-around for tis
;; bug.
(global-set-key (kbd "C-c w") 'ace-window)

;; KEYBINDING: Jump to beginning / end of buffer.
;;
;; In some terminals 'C-<home>' and 'C-<end>' do not work.  These
;; alternative bindings do seem to work in these terminals.
(global-set-key (kbd "C-x <home>") 'beginning-of-buffer)
(global-set-key (kbd "C-x <end>") 'end-of-buffer)

;; KEYBINDING: Use iedit mode to make changes within the current
;; function.
(global-set-key (kbd "C-c ;") 'iedit-mode-toggle-on-function)

;; KEYBINDING: Easier access to `goto-line'.
(global-set-key [?\M-g] 'goto-line)

;; KEYBINDING: Activate multiple cursors mode.
(global-set-key (kbd "C-c e") 'mc/edit-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Header Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: This should really move into a theme.
(set-face-attribute 'header-line nil
                    :foreground "grey20"
                    :background "grey90"
                    :box '(:line-width 1 :color "red"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Miscellaneous Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Delete should delete the current select,
(delete-selection-mode t)

;; Highlight the current selection.
(transient-mark-mode t)

;; Show possible keybindings
(which-key-mode 1)

;; Setup the auto-mode-alist
;;
;; I name my gdb scripts as *.gdb
(add-to-list 'auto-mode-alist '("\\.gdb\\'" . gdb-script-mode))
;; Ensure we use org-mode when required
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Ensure we use forth mode for all possible extensions
(add-to-list 'auto-mode-alist '("\\.of\\'"  . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'"  . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth\\'" . forth-mode))
;; Ensure we use cperl-mode not perl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; Start in the right mode when editing mutt files.
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Configure FCI mode, this places the 80 column marker down the right
;; hand endge of the screen.
(when (fboundp 'fci-mode)
  ;; KEYBINDING: Toggle `fci-mode' on / off.
  (global-set-key (kbd "C-c |") 'fci-mode)
  (add-hook 'c-mode-common-hook 'fci-mode)
  (add-hook 'cperl-mode-hook 'fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode))

;; Turn on `undo-tree' whenever we have it available.  This draws the
;; pretty undo tree diagrams to allow easier navigation of the undo
;; history.
(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1))

;; Turn on `xclip-mode', this allows copy and paste to the x-clipboard
;; when running in the terminal, using the xclip program.
(when (fboundp 'xclip-mode)
  (xclip-mode 1))

;; Activate `winner-mode' to allow for window undo and redo.
(winner-mode 1)

;; Enable `cua-mode' but without the C-x/C-z/C-v bindings being
;; enabled.  This is basically an alternative to rectangle selection
;; mode.
(cua-selection-mode 1)

;; Move the mouse pointer away from the cursor.  This is mostly
;; redundant now, as most systems I use these days hide the mouse
;; cursor when I start typing.
(if (display-graphic-p)
    (mouse-avoidance-mode 'cat-and-mouse))

;; Setting `bookmark-save-flag' will cause any command that creates a
;; bookmark to automatically save the bookmarks to disk.  This means
;; that they survive even if emacs crashes.
(setq bookmark-save-flag 1)

;; Allow blocks to be hidden / shown on demand.  This is really great
;; package, but I don't make enough use of it.
;;
;; TODO: See if I can make more use of this.  Is there a better
;; alternative?  I'd like a version where hidden blocks are
;; automatically expanded upon entry.
(require 'hideshow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                   Setup `browse-kill-ring'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEYBINDING: Browse the `kill-ring'.
(global-set-key "\C-cy" 'browse-kill-ring)

;; Settings for `browse-kill-ring-mode'.
(add-hook
 'browse-kill-ring-mode-hook
 (lambda ()
   ;; Temporarily highlight inserted item.
   (setq browse-kill-ring-highlight-inserted-item t)

   ;; Highlight current choice in the kill ring buffer.
   (setq browse-kill-ring-highlight-current-entry t)

   ;; String separating entries in the `separated' style
   (setq browse-kill-ring-separator
         "\n--separator------------------------------")

   ;; Don't allow standard navigation in kill ring buffer.
   (define-key browse-kill-ring-mode-map
     (kbd "<down>") 'browse-kill-ring-forward)
   (define-key browse-kill-ring-mode-map
     (kbd "<up>") 'browse-kill-ring-previous)
   (define-key browse-kill-ring-mode-map
     (kbd "<right>") 'browse-kill-ring-forward)
   (define-key browse-kill-ring-mode-map
     (kbd "<left>") 'browse-kill-ring-previous)

   ;; Face for the separator
   (defface browse-kill-ring-separator
     '((t . (:inherit bold)))
     "Face used for the separator in browse-kill-ring buffer")
   (setq browse-kill-ring-separator-face 'browse-kill-ring-separator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Setup `R-mode'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading the `ess-site' file, which is needed for `R-mode' is too
;; expensive to do at every startup, especially how infrequently I
;; actually edit R code.
;;
;; This hack gives me R mode support for *.R and *.Rd files at minimal
;; cost.
;;
;; TODO: Extend the list of file extensions for which I load and
;; activate `R-mode' to cover more of the auxiliary R file extensions.

(defun andrew-r-mode ()
  (interactive)
  (require 'ess-site)
  (R-mode))

(add-to-list 'auto-mode-alist '("\\.R\\'" . andrew-r-mode))
(add-to-list 'auto-mode-alist '("\\.Rd\\'" . andrew-r-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                Setup `notmuch' email client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup notmuch email system.
(with-eval-after-load "notmuch"
  (require 'andrew-notmuch)
  (require 'gnus)
  (add-to-list 'notmuch-show-insert-text/plain-hook
               'andrew/notmuch-decrypt-inline-pgp)
  (add-to-list 'notmuch-show-insert-text/plain-hook
               'andrew/notmuch-verify-inline-pgp)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-fcc-dirs "outbox")
  (setq notmuch-search-oldest-first nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Org Mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grab my customisation of core `org-mode' stuff.
(add-hook 'org-load-hook
          (progn
            (require 'andrew-org)
            (require 'andrew-org-capture)))

;; My settings to support logging work.
(require 'andrew-work-log)

;; KEYBINDING: Visit my work log.
(global-set-key (kbd "C-c l") 'andrew-work-log/visit-log-entry)

;; These settings are all about how I setup task management stuff.
;;
;; I'm in the process of rewritting all of this stuff.  When the
;; environment variable is set I pull in the "under-development"
;; version of the settings.
;;
;; TODO: This needs to be split into the parts that need to be loaded
;; at configuration time, and the parts that can be deferred, and
;; loaded as part of the `org-mode-hook', this would speed up emacs
;; startup.
(if (getenv "ANDREW_ORG_V2")
    (require 'andrew-org-v2-setup)
  (require 'andrew-org-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Customisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store custom settings into a separate file.  Load that file now to
;; pick up previously stored settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
