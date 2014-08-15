;; This is a dumping ground for old bits of my emacs configuration
;; file that I don't think is needed anymore.

;; ===========================================================================
;; Add extra directories to the load path.
(defconst elisp-directory (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path (concat elisp-directory "/lisp"))
(add-to-list 'load-path (concat elisp-directory "/completion-ui"))
(add-to-list 'load-path (concat elisp-directory "/ess-5.7.1/lisp"))
(add-to-list 'load-path (concat elisp-directory "/magit"))
(add-to-list 'load-path (concat elisp-directory "/ace-jump-mode"))
(add-to-list 'load-path (concat elisp-directory "/ace-window"))
(add-to-list 'load-path (concat elisp-directory "/site-specific"))
(add-to-list 'load-path (concat elisp-directory "/my-elisp"))
(add-to-list 'load-path (concat elisp-directory "/auto-complete"))
(add-to-list 'load-path (concat elisp-directory "/popup"))

(if (>= emacs-major-version 24)
    (add-to-list 'load-path (concat elisp-directory "/cperl-mode")))

;; I believe there's a bug in cc-mode after about version 5.31.3 which can
;; cause it to go into an infinite loop in some rare cases.  At some point
;; I'd like to at least create a reproducible bug for this.
(if (file-exists-p (concat elisp-directory "/cc-mode-head"))
    (add-to-list 'load-path (concat elisp-directory "/cc-mode-head"))
  (add-to-list 'load-path (concat elisp-directory "/cc-mode-5.32.2")))

;; ===========================================================================
;; Special mode for editing CGEN .cpu files, basically scheme mode with
;; some extra highlighting thrown in.
(autoload 'cpu-mode "cpu-mode")
(add-to-list 'auto-mode-alist '("\\.cpu\\'" . cpu-mode))

;; ===========================================================================
;;       Color Theme
(eval-after-load "color-theme"
  '(progn (color-theme-initialize)))
(require 'color-theme)


;; ===========================================================================
;;       Faces
(make-face 'hl-line)  ;; Create face before it's autoloaded, just so
                      ;; it can be customised.
(set-face-attribute 'hl-line nil
                    :background "gray25")

(set-face-attribute 'shadow nil :foreground "grey50")

(defun andrew-configure-diff-mode-faces ()
  (set-face-attribute 'diff-file-header nil
                      :foreground "green"
                      :background "unspecified")

  (set-face-attribute 'diff-header nil
                      :foreground "cyan"
                      :weight 'bold
                      :background "unspecified")

  (set-face-attribute 'diff-function nil
                      :foreground "red2"
                      :background "unspecified")

  (set-face-attribute 'diff-refine-change nil
                      :foreground "black"
                      :background "pale turquoise")

  (set-face-attribute 'diff-indicator-added nil
                      :weight 'bold
                      :foreground "green")

  (set-face-attribute 'diff-indicator-removed nil
                      :weight 'bold
                      :foreground "red")

  (set-face-attribute 'diff-indicator-changed nil
                      :weight 'bold
                      :foreground "yellow")

  (set-face-attribute 'diff-removed nil
                      :background "black")

  (set-face-attribute 'diff-added nil
                      :background "black")))

;;============================================================================
;; Setup font lock mode.
(defun andrew-configure-font-lock-faces ()
  ;; Red comments when running in an xterm.
  (if (not (display-graphic-p))
      (set-face-attribute 'font-lock-comment-face nil
                          :foreground "red1"))

  (set-face-attribute 'font-lock-type-face nil
                      :foreground "medium purple")

  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground "orange red"))

(defun andrew-configure-font-lock-mode ()
  (setq font-lock-maximum-decoration  t
        font-lock-verbose             t
        font-lock-support-mode        'jit-lock-mode
        lazy-lock-defer-on-scrolling  nil
        lazy-lock-defer-contextually  t
        lazy-lock-stealth-verbose     t
        lazy-lock-stealth-lines       50
        lazy-lock-stealth-time        3)
  (andrew-configure-font-lock-faces))

(add-hook 'font-lock-mode-hook 'andrew-configure-font-lock-mode)
(global-font-lock-mode t)

;;===========================================================================
;; This face is used in the mini-buffer to shadow out unused parts of the
;; filename.
(set-face-attribute 'file-name-shadow nil :foreground "gray35")

;;============================================================================
;; Printing configuration...
(setq lpr-command "a2ps")
(setq lpr-switches '("--sides=duplex" "-r" "--columns=2"))
(setq printer-name "HPLaserJet4050")
(setq lpr-add-switches nil)

;;============================================================================
;; This will configure some faces for me. The faces don't exists in
;; normal mode, so calling set-face-attribute will fail. HOWEVER; the
;; call to lambda says: delay evaluation till run time, in which case
;; flyspell mode (for example) will have been loaded, and the face will
;; then exist - magic :)
;;
;; Faces for flyspell-mode
(add-hook 'flyspell-mode-hook
          (lambda()
            (set-face-attribute 'flyspell-duplicate nil
                                :foreground "DeepSkyBlue1"
                                :underline t
                                :weight 'bold)))

;; ===========================================================================
;; Rectangle marking
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark" "Drag out a rectangular region with the mouse." t)

;;============================================================================
;; My first elisp function...
(defun get-pod (buf)
  "Runs pod2text on the saved version of a buffer."
  (interactive "bBuffer Name: ")
  (let ((target-buffer (get-buffer-create "*__pod__*")))
    (shell-command (format "perldoc -t %s" (buffer-file-name (get-buffer buf)))
                   target-buffer)))
