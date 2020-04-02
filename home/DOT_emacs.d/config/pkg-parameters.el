;; Emacs configuration

;; Settings for `package.el'.

(require 'package)

;; Set repository list with HTTPS links, set repository priorities, set display
;; preferences, and check signature for packages that provide one.
(setq-default package-archives
              '(("gnu"          . "https://elpa.gnu.org/packages/")
                ;; ("marmalade"    . "http://marmalade-repo.org/packages/")
                ("melpa"        . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("org"          . "https://orgmode.org/elpa/"))
              package-archive-priorities
              '(("gnu"          . 1)
                ;; ("marmalade"    . 2)
                ("melpa"        . 2)
                ("melpa-stable" . 2)
                ("org"          . 2))
              package-menu-hide-low-priority 't
              package-menu--hide-packages 't
              ;; Check signature when a package provides one, but don't block
              ;; the installation of unsigned packages.
              package-check-signature 'allow-unsigned)

;; If `use-package' is not already installed, install it.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure all other packages with `use-package'.
(eval-when-compile
  (require 'use-package))
(setq-default use-package-compute-statistics t)

;; Automatically update packages when new versions are available (check every
;; week), but ask confirmation instead of being invasive.
(use-package auto-package-update
  :ensure t
  :config
  (setq-default auto-package-update-prompt-before-update t
                auto-package-update-interval 7
                auto-package-update-delete-old-versions t
                auto-package-update-hide-results nil)
  (auto-package-update-maybe))

;; Make sure `use-package' directives can use :bind and :diminish keywords.
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)

;; Make sure `exec-path-from-shell' is available before anything else,
;; otherwise Emacs won't find GnuPG and won't be able to verify
;; packages presenting a signature (they will raise an error, because
;; of the signature checking policy enforced by
;; `package-check-signature' above).
(use-package exec-path-from-shell
  :if (memq system-type '(darwin gnu/linux))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
