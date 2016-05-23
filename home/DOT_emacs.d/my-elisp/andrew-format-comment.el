;;;###autoload
(defun andrew/reformat-comment ()
  "Reformt a comment line /****  TEXT ****/"
  (interactive)
  (let ((eol (line-end-position))
        (bol (line-beginning-position))
        (now (point))
        (target-width 65)
        bot eot len lgap rgap)
    (beginning-of-line)
    (unless (looking-at "/\\*\\*\\*\\* ")
      (error "Line does not start with '/**** '"))
    (forward-char 5)
    (skip-syntax-forward "-")
    (setq bot (point))
    (end-of-line)
    (unless (looking-back " \\*\\*\\*\\*/")
      (error "Line does not end with ' ****/'"))
    (backward-char 5)
    (skip-syntax-backward "-")
    (setq eot (point))
    (setq len (- eot bot))
    (setq lgap (/ (- target-width len) 2))
    (unless (> lgap 1)
      (error "Comment text is too long, not enough gap"))
    (setq rgap (- (- target-width len) lgap))
    (unless (> rgap 1)
      (error "Comment text is too long, not enough gap"))
    (beginning-of-line)
    (forward-char 5)
    (delete-horizontal-space)
    (insert-char ?\s lgap)
    (end-of-line)
    (backward-char 5)
    (delete-horizontal-space)
    (insert-char ?\s rgap)
    (goto-char now)))

(provide andrew-format-comment)
