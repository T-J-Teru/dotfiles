;; Called from `latex-mode-hook' to activate features I want.
;;;###autoload
(defun andrew-latex-mode ()
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (setq LaTeX-figure-label "figure:")
  (setq LaTeX-table-label "table:"))
