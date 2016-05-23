;; Configure forth-mode.
;;;###autoload
(defun andrew-forth-mode ()
  (setq forth-indent-level 2)
  (setq forth-minor-indent-level 1)
  (setq forth-custom-words
        '((("QT\"")
           compile-only
           (font-lock-string-face . 1)
           "[\"\n]" nil string
           (font-lock-string-face . 1)))))
