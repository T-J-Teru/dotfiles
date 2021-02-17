;; Configure forth-mode.
;;;###autoload
(defun andrew-diff-mode ()
  (font-lock-add-keywords
   nil
   '(("^index \\(.+\\).*\n"
      (0 'diff-header) (1 'diff-index prepend))
     ("^diff --git \\(.+\\).*\n"
      (0 'diff-header) (1 'diff-file-header prepend)))))

