;; Loading the `ess-site' file, which is needed for `R-mode' is too
;; expensive to do at every startup, especially how infrequently I
;; actually edit R code.
;;
;; This hack gives me R mode support for *.R and *.Rd files at minimal
;; cost.
;;
;; TODO: Extend the list of file extensions for which I load and
;; activate `R-mode' to cover more of the auxiliary R file extensions.

;;;###autoload
(defun andrew-r-mode ()
  (interactive)
  (use-package ess
    :ensure t
    :config
    (require 'ess-site))
  (R-mode))
