(require 'org)
(require 'andrew-org-capture)

(defun org-return (&optional indent)
  "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
See the individual commands for more information."
  (interactive)
  (let (org-ts-what)
    (cond
     ((or (bobp) (org-in-src-block-p))
      (if indent (newline-and-indent) (newline nil t)))
     ((org-at-table-p)
      (org-table-justify-field-maybe)
      (call-interactively 'org-table-next-row))
     ;; when `newline-and-indent' is called within a list, make sure
     ;; text moved stays inside the item.
     ((and (org-in-item-p) indent)
      (if (and (org-at-item-p) (>= (point) (match-end 0)))
	  (progn
	    (save-match-data (newline nil t))
	    (org-indent-line-to (length (match-string 0))))
	(let ((ind (org-get-indentation)))
	  (newline nil t)
	  (if (org-looking-back org-list-end-re)
	      (org-indent-line)
	    (org-indent-line-to ind)))))
     ((and org-return-follows-link
	   (org-at-timestamp-p t)
	   (not (eq org-ts-what 'after)))
      (org-follow-timestamp-link))
     ((and org-return-follows-link
	   (let ((tprop (get-text-property (point) 'face)))
	     (or (eq tprop 'org-link)
		 (and (listp tprop) (memq 'org-link tprop)))))
      (call-interactively 'org-open-at-point))
     ((and (org-at-heading-p)
	   (looking-at
	    (org-re "\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)[ \t]*$")))
      (org-show-entry)
      (end-of-line 1)
      (newline nil t))
     (t (if indent (newline-and-indent) (newline nil t))))))

(defun andrew-org-space ()
  "Insert a space and reindent line if it's a new heading.
This is bound to space key, as ELECTRIC-INDENT-MODE will
automatically indent new lines, this makes starting a new header
easier."
  (interactive)
  (self-insert-command 1)
  (when (looking-back "^ +\\*+ +")
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space))))

(define-key org-mode-map (kbd "<SPC>") 'andrew-org-space)

(provide 'andrew-org)
