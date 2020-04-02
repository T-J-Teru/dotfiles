;; Emacs configuration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              Setup File Loading And Load Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro add-to-load-path (directory &rest body)
  "If DIRECTORY exists then it is added to the `load-path', and
then BODY is evaluated.  If directory does not exist then the
`load-path' is not changed, and BODY is not evaluated."
  `(when (file-exists-p
          (concat user-emacs-directory "/" ,directory))
     (add-to-list 'load-path (concat user-emacs-directory "/" ,directory))
     ,@body))

;; This directory contains small, single file packages that are not
;; found in any of the repositories available through use-package.
(add-to-load-path "packages")

;; This directory contains setup code that I've written, the code in
;; here is all autoloaded, and shouldn't be required directly.
(add-to-load-path "my-elisp" (require 'andrew-autoloads))

;; This directory contains files that form part of this init file, but
;; split out in an attempt to make this file more managable.  These
;; are all loaded directly below.
(add-to-load-path "config")

;; This needs to come first, otherwise `package.el' will complain.
(package-initialize)

;; File to record paremeters set through the `customize' utility.
(setq-default custom-file (concat user-emacs-directory "custom.el"))

;; Load parameter files.
;;
;; Always load `https-parameters' first, `gpg-parameters' second and
;; `pkg-parameters' third. This is to make sure the rest of the
;; configuration can install packages on demand, that the package
;; manager will never connect to repositories without using TLS, and
;; that it will be able to check signatures when they are available.
;;
;; After that load `global-parameters' which currently does all the
;; rest of my emacs configuration.
(load "https-parameters")
(load "pkg-parameters")
(load "gpg-parameters")
(load "global-parameters")

;; The last thing we do is load in the `customize' settings.  Maybe
;; this should be done first so that my initialisation settings will
;; override these?
(load custom-file t)
