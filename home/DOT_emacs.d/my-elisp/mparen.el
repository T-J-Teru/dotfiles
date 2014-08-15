;; Allow jumping between matching parenthesis.

(defun jump-to-matching-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert a hash."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(provide 'mparen)

