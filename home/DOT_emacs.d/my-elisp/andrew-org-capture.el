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

;; Fixed version of `org-capture-refile', this calls my new function
;; `org-save-and-close-refile-target-buffer'.
(defun org-capture-refile ()
  "Finalize the current capture and then refile the entry.
Refiling is done from the base buffer, because the indirect buffer is then
already gone.  Any prefix argument will be passed to the refile command."
  (interactive)
  (unless (eq (org-capture-get :type 'local) 'entry)
    (user-error "Refiling from a capture buffer makes only sense \
for `entry'-type templates"))
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
	 (pos (make-marker))
	 (org-capture-is-refiling t)
	 (kill-buffer (org-capture-get :kill-buffer 'local))
	 (jump-to-captured (org-capture-get :jump-to-captured 'local)))
    ;; Since `org-capture-finalize' may alter buffer contents (e.g.,
    ;; empty lines) around entry, use a marker to refer to the
    ;; headline to be refiled.  Place the marker in the base buffer,
    ;; as the current indirect one is going to be killed.
    (set-marker pos (save-excursion (org-back-to-heading t) (point)) base)
    ;; `org-capture-finalize' calls `org-capture-goto-last-stored' too
    ;; early.  We want to wait for the refiling to be over, so we
    ;; control when the latter function is called.
    (org-capture-put :kill-buffer nil :jump-to-captured nil)
    (org-capture-finalize)

    (save-window-excursion
      (with-current-buffer base
	(org-with-point-at pos
	  (call-interactively 'org-refile))))
    (when kill-buffer
      (with-current-buffer base (save-buffer))
      (kill-buffer base)
      (org-save-and-close-refile-target-buffer))
    (when jump-to-captured (org-capture-goto-last-stored))))

;; This is a new function added to support the above.
;; Called from org-capture-refile after the entry has been refiled,
;; causes us to save and close the org buffer into which the item was
;; refiled.
(defun org-save-and-close-refile-target-buffer ()
  (let ((marker org-capture-last-stored-marker))
    (if (and marker (marker-buffer marker)
	     (buffer-live-p (marker-buffer marker)))
	(let ((buf (marker-buffer marker)))
	  (with-current-buffer buf (save-buffer))
	  (kill-buffer buf)))))

(provide 'andrew-org-capture)
