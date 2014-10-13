(setq inhibit-splash-screen t)

(setq default-frame-alist
      `(;; menu-bar-lines and tool-bar-lines are how old
        ;; versions of emacs (before 24) initialised the
        ;; menu & tool bars.  Now we just check the
        ;; values of menu-bar-mode and tool-bar-mode.
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

(set-scroll-bar-mode nil)
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(setq inhibit-default-init t)

(setq frame-title-format
      (list '(using-xemacs
             "xemacs: "
             "emacs: ")
            '(buffer-file-name
             "%f"
             (dired-directory dired-directory "%b"))))

(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defconst elisp-directory (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (concat elisp-directory "/packages"))
(add-to-list 'load-path (concat elisp-directory "/my-elisp"))

(add-to-list 'load-path (concat elisp-directory "/cc-mode"))

(when (file-exists-p
       (concat elisp-directory "/cedet"))
  (add-to-list 'load-path (concat elisp-directory "/cedet"))
  (require 'cedet-devel-load))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda ()
                               (hl-line-mode 1)
                               (ibuffer-auto-mode)))

(eval-after-load "hl-line"
  '(defadvice ibuffer-redisplay-engine (after
                                        andrew-ibuffer-redisplay
                                        activate)
     (hl-line-highlight)))

(defalias 'list-buffers 'ibuffer)

(setq line-number-mode t)
(setq column-number-mode t)

(setq minibuffer-prompt-properties
      '(read-only t
        point-entered minibuffer-avoid-prompt
        face minibuffer-prompt))

(when (fboundp 'xclip-mode)
  (xclip-mode 1))

(setq bookmark-save-flag 1)

(transient-mark-mode t)

(set-default `indent-tabs-mode nil)

(delete-selection-mode t)

(if (display-graphic-p)
    (mouse-avoidance-mode 'cat-and-mouse))

(add-hook 'after-save-hook
   '(lambda ()
      (let ( (temp (substring buffer-file-name -3)) )
        (if (or (equal temp ".pl")
                (equal temp ".sh"))
            (executable-make-buffer-file-executable-if-script-p)))))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(autoload 'r-mode "ess-site")
(autoload 'javascript-mode "javascript" nil t)
(autoload 'forth-mode "gforth")

(autoload 'andrew-cperl-mode "andrew-cperl-mode")
(autoload 'andrew-cc-mode "andrew-cc-mode")
(autoload 'jump-to-matching-paren "mparen")
(autoload 'toggle-menubar-and-toolbar "menu-toggle")

;; I create gdb command scripts as *.gdb
(add-to-list 'auto-mode-alist '("\\.gdb\\'" . gdb-script-mode))
;; Take care of supporting R mode.
(add-to-list 'auto-mode-alist '("\\.R\\'" . r-mode))
;; Javascript support.
(add-to-list 'auto-mode-alist (cons  "\\.js\\'" 'javascript-mode))
;; Lots of different extensions for FORTH
(add-to-list 'auto-mode-alist '("\\.of\\'"  . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'"  . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'"  . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth\\'" . forth-mode))
;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Ensure we use cperl-mode not perl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; Start in the right mode when editing mutt files.
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(setq ctl-x-a-map (make-sparse-keymap))
(define-key ctl-x-map "\C-a" ctl-x-a-map)

;; Allow blocks to be hidden / shown on demand.
(require 'hideshow)
;; For doing haskell - will fail silently if haskell mode is not available on
;; the machine that this emacs is being run on.
(require 'haskell-mode nil t)
;; Linker script mode.
(require 'ld-script)

(define-key ctl-x-a-map "j" 'ace-jump-word-mode)
(define-key ctl-x-a-map "J" 'ace-jump-char-mode)
(define-key ctl-x-a-map "o" 'ace-window)

(global-set-key (kbd "C-x /") 'ace-jump-word-mode)

(when (fboundp 'fci-mode)
  (global-set-key (kbd "C-x |") 'fci-mode))

(when (fboundp 'fci-mode)
  (add-hook 'c-mode-common-hook 'fci-mode)
  (add-hook 'cperl-mode-hook 'fci-mode)
  (add-hook 'emacs-lisp-mode-hook 'fci-mode))

(defvar iedit-toggle-key-default (kbd "C-;"))
(define-key global-map iedit-toggle-key-default 'iedit-mode)
(define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
(define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
(define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function)

(require 'icomplete+ nil t)
(icomplete-mode)

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1))

(winner-mode 1)

(cua-selection-mode 1)

(defun andrew-configure-diff-mode ()
  (font-lock-add-keywords
   nil
   '(("^index \\(.+\\).*\n"
      (0 diff-header-face) (1 diff-index-face prepend))
     ("^diff --git \\(.+\\).*\n"
      (0 diff-header-face) (1 diff-file-header-face prepend))
     ))
  )

(add-hook 'diff-mode-hook 'andrew-configure-diff-mode)

(add-hook 'forth-mode-hook
          '(lambda ()
             (setq forth-indent-level 2)
             (setq forth-minor-indent-level 1)
             (setq forth-custom-words
                   '((("QT\"")
                      compile-only
                      (font-lock-string-face . 1)
                      "[\"\n]" nil string
                      (font-lock-string-face . 1))))))

(add-hook 'javascript-mode-hook
          '(lambda () (setq js-indent-level 2)))

(add-hook 'latex-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)
             (setq LaTeX-figure-label "figure:")
             (setq LaTeX-table-label "table:")))

;; Set up the spell checker to use.
;; Thve alternative is ispell - but aspell gives better suggestions.
(setq-default ispell-program-name "aspell")
;;
;; Make sure that we pick up the correct dictionary name.
;; In truth only the ispell-dictionary needs to be set, but
;; it is nice to set them both just to be on the safe side.
;;
(if (string-equal ispell-program-name "aspell")
  ;; aspell has 'british' dictionary.
  (progn (setq flyspell-default-dictionary "british")
         (setq ispell-dictionary "british"))
  ;;
  ;; ispell calls it an english dictionary.
  (progn (setq flyspell-default-dictionary "english")
         (setq ispell-dictionary "english")))

(require 'andrew-ispell)

;; A bug in emacs 24 causes the following line to be needed.
;; Would be nice to remove this one day.
(if (>= emacs-major-version 24)
    (progn
      (defvar cperl-invalid-face nil)
      (setq cperl-highlight-variables-indiscriminately 't)))

;; Now load my cperl customisations when entering cperl-mode.
(add-hook 'cperl-mode-hook 'andrew-cperl-mode)

(add-hook 'c-mode-hook 'andrew-cc-mode)
(add-hook 'c++-mode-hook 'andrew-cc-mode)

(eval-after-load "sendmail" (lambda ()
                              (message "Loading andrew-email-mode")
                              (require 'andrew-email-mode)))

(define-key ctl-x-a-map "e" 'mc/edit-lines)

(defun using-xemacs ()
  (string-match "XEmacs\\|Lucid" emacs-version))

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

(show-paren-mode t)
(if (display-graphic-p)
    (setq show-paren-style 'expression)
  (setq show-paren-style 'parenthesis))

(set-default `truncate-lines t)

(defun toggle-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defvar cursor-default-colour "LimeGreen")
(defvar cursor-overwrite-colour "red")

(defun cursor-overwrite-mode ()
  "Set cursor colour according to insert mode"
  (set-cursor-color
   (if overwrite-mode
       cursor-overwrite-colour
       cursor-default-colour)))
(add-hook 'post-command-hook 'cursor-overwrite-mode)

(when (require 'winpoint nil t)
  (winpoint-mode 1))

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

(eval-after-load "grep"
  (lambda ()
   (grep-apply-setting 'grep-command
                       "grep --exclude='*~' --exclude='.#*' -IHn -e ")))

(when (require 'browse-kill-ring nil t)
  (global-set-key "\C-cy" 'browse-kill-ring)

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
  (setq browse-kill-ring-separator-face 'browse-kill-ring-separator))

(if (display-graphic-p)
    (set-face-attribute 'show-paren-mismatch-face
                        nil
                        :strike-through "red"))

(set-face-attribute 'header-line nil
                    :foreground "grey20"
                    :background "grey90"
                    :box '(:line-width 1 :color "red"))

(require 'linum+)

(setq linum-narrow-relative nil)

(if window-system
  (setq linum+-smart-format "%%%dd"
        linum+-dynamic-format "%%%dd"))

(global-set-key (kbd "C-c n") 'linum-mode)
(global-set-key (kbd "C-c N") 'global-linum-mode)

(when (require 'hlinum nil t)
  (hlinum-activate))

(setq whitespace-style '(face trailing empty))
(global-whitespace-mode)

(setq vc-make-backup-files t)

(setq vc-handled-backends nil)
(setq version-control 'never)

(global-set-key (kbd "C-x <home>") 'beginning-of-buffer)
(global-set-key (kbd "C-x <end>") 'end-of-buffer)
;; For some reason C-<home> and C-<end> don't work inside a terminal.
(global-unset-key (kbd "C-<home>"))
(global-unset-key (kbd "C-<end>"))

(global-set-key [?\M-g] 'goto-line)

;; Allow the current line to be highlighted.
(global-set-key (kbd "C-c h") 'hl-line-mode)

;; Allow me to grow/shrink the window (when spilt horizontally)
;; from the keyboard.
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-x <down>") 'shrink-window)

(global-set-key (kbd "C-x #") 'jump-to-matching-paren)

(global-set-key (kbd "<f12>") 'toggle-menubar-and-toolbar)

(require 'template)
(template-initialize)
(setq template-auto-update-disable-regexp ".ido.last")
(setq template-auto-update nil)

;;       Org Mode
(add-to-list 'load-path (concat elisp-directory "/org-mode/lisp"))
(add-to-list 'load-path (concat elisp-directory "/org-mode/contrib/lisp"))
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(defun my-org-mode-hook ()
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
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun org-file (name)
  (let ((org-directory (expand-file-name "~/.org/")))
    (concat org-directory name)))

(setq org-agenda-files (list (org-file "")))

(setq org-default-notes-file (org-file "refile.org"))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               (concat elisp-directory "/auto-complete/dict/"))
  (ac-config-default))

;; Function for moving through the windows backwards
(defun other-window-backward ()
  "Like other window, but go backwards."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'other-window-backward)

(if (not (getenv "EMAIL"))
    (progn
      (display-warning
       :warning
       "Missing EMAIL environment variable, setting email to <invalid>.")
      (setq user-mail-address "<invalid>")))

(message "All done.")
