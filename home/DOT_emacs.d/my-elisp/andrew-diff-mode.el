;; Configure forth-mode.
;;;###autoload
(defun andrew-diff-mode ()
  (font-lock-add-keywords
   nil
   '(("^index \\(.+\\).*\n"
      (0 diff-header-face) (1 diff-index-face prepend))
     ("^diff --git \\(.+\\).*\n"
      (0 diff-header-face) (1 diff-file-header-face prepend)))))

