(require 'org-capture)

;; Fixed version of `org-capture-place-item'.
(defun org-capture-place-item ()
  "Place the template as a new plain list item."
  (let* ((txt (org-capture-get :template))
	 (target-entry-p (org-capture-get :target-entry-p))
	 (ind (1+ (org-current-level)))
	 beg end)
    (when (org-capture-get :exact-position)
      (goto-char (org-capture-get :exact-position)))
    (cond
     ((not target-entry-p)
      ;; Insert as top-level entry, either at beginning or at end of file
      (setq beg (point-min) end (point-max)))
     (t
      (setq beg (1+ (point-at-eol))
            end (save-excursion (outline-next-heading) (point)))))
    (if (org-capture-get :prepend)
        (progn
          (goto-char beg)
          (when (org-list-search-forward (org-item-beginning-re) end t)
            (goto-char (match-beginning 0))
            (setq ind (org-get-indentation))))
      (goto-char end)
      (when (org-list-search-backward (org-item-beginning-re) beg t)
        (setq ind (org-get-indentation))
        (org-end-of-item)))
    ;; Remove common indentation
    (setq txt (org-remove-indentation txt))
    ;; Make sure this is indeed an item
    (unless (string-match (concat "\\`" (org-item-re)) txt)
      (setq txt (concat "- "
                        (mapconcat 'identity (split-string txt "\n")
                                   "\n  "))))
    ;; Set the correct indentation, depending on context
    (setq ind (make-string ind ?\ ))
    (setq txt (concat ind
                      (mapconcat 'identity (split-string txt "\n")
                                 (concat "\n" ind))
                      "\n"))
    ;; Insert, with surrounding empty lines
    (org-capture-empty-lines-before)
    (setq beg (point))
    (insert txt)
    ;; APB: Not sure what this is for.
    ;; (or (bolp) (insert "\n"))
    (org-capture-empty-lines-after) ;; Change from 1 to nil.
    (org-capture-position-for-last-stored beg)
    (forward-char 1)
    (setq end (point))
    (org-capture-mark-kill-region beg (1- end))
    (org-capture-narrow beg (1- end))
    (if (or (re-search-backward "%\\?" beg t)
            (re-search-forward "%\\?" end t))
        (replace-match ""))))

(provide 'andrew-org-capture)
