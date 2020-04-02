;; Emacs configuration

;; Settings to make Emacs correctly find GnuPG and prompt for passphrases.
;; See <https://emacs.stackexchange.com/q/32881/18177> for details:
;; this also requires the following options in `~/.gnupg/gpg-agent.conf'
;; allow-emacs-pinentry
;; allow-loopback-pinentry
;; This will work only after `exec-path-from-shell' is loaded (this is done in
;; `pkg-parameters.el').
(setenv "GPG_AGENT_INFO" "~/.gnupg/S.gpg-agent")
(setenv "SSH_AUTH_SOCK" "~/.gnupg/S.gpg-agent.ssh")
(setq-default epg-gpg-program "gpg"
              epg-gpg-home-directory "~/.gnupg/"
              epa-pinentry-mode 'loopback)

(use-package pinentry
  :ensure t)
(pinentry-start)
