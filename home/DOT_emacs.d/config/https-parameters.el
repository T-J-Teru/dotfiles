;; Emacs configuration

;; Settings to enforce verification of HTTPS connections.
;; Taken from <https://glyph.twistedmatrix.com/2015/11/editor-malware.html>.
;; This configuration only works if Emacs was built with GnuTLS
;; embedded. See the end of this file if Emacs uses an external
;; installation of GnuTLS.

;; Set global network security policy, log important messages from GnuTLS,
;; always check certificates validity and set minimal threshold for key length
;; (for details, see
;; <https://gnutls.org/manual/html_node/Selecting-cryptographic-key-sizes.html>).
(setq-default network-security-level 'medium
              gnutls-log-level 2
              gnutls-verify-error t
              gnutls-min-prime-bits 3072)

;; Path to a certificate bundle. This file should be maintained up to date.
;; To do that, run the following command monthly in ~/emacs.d:
;; curl --remote-name --time-cond cacert.pem https://curl.haxx.se/ca/cacert.pem
;; This will download an up-to-date copy of the Mozilla CA certificate bundle,
;; kindly provided in the correct format (PEM) by the cURL developpers.
;; In case we cannot get this file, abort loading the configuration. In no
;; circumstance we should allow unverified HTTPS connections.
(if (file-executable-p "~/.emacs.d/update-cacert.sh")
    (call-process "~/.emacs.d/update-cacert.sh")
  (progn
    (message "File not found or not executable: ~/.emacs.d/update-cacert.sh")
    (message "Falling back on system certificate bundle: /private/etc/ssl/cert.pem")))

(setq-default gnutls-trustfiles (list ;"~/.emacs.d/cacert.pem"
                                      "/private/etc/ssl/cert.pem"))

;; If GnuTLS is not embedded in Emacs, but accessed from a separate
;; installation, then the following lines are also required.
;; (setq-default tls-checktrust t)
;; (setq-default tls-program
;;               (format "gnutls-cli --x509cafile %s -p %%p %%h"
;;                       gnutls-trustfiles))
