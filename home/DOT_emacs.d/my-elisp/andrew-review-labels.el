(defvar andrew-review-labels-valid-labels
  '("Reviewed-By" "Approved-By" "Tested-By")
  "List of all valid review labels.")

(defun andrew-review-labels-insert (label-type)
  "Insert a review label at `point' in the current buffer.
LABEL-TYPE is one of the strings in
`andrew-review-labels-valid-labels'."
  (interactive
   (list (completing-read "What review label to insert: "
                          andrew-review-labels-valid-labels
                          nil t)))
  (if (not (member label-type andrew-review-labels-valid-labels))
      (error (format "Invalid label type `%s'" label-type)))
  (insert (format "%s: %s <%s>\n" label-type user-full-name
                  user-mail-address)))

(defun andrew-review-labels-insert-tested-by ()
  "Insert a \"Tested-By\" review label."
  (interactive)
  (andrew-review-labels-insert "Tested-By"))

(defun andrew-review-labels-insert-reviewed-by ()
  "Insert a \"Reviewed-By\" review label."
  (interactive)
  (andrew-review-labels-insert "Reviewed-By"))

(defun andrew-review-labels-insert-approved-by ()
  "Insert a \"Approved-By\" review label."
  (interactive)
  (andrew-review-labels-insert "Approved-By"))

(defun andrew-review-labels-insert-acked-by ()
  "Insert a \"Acked-By\" review label."
  (interactive)
  (andrew-review-labels-insert "Acked-By"))

(defvar andrew-review-labels-keymap (make-sparse-keymap)
  "Keymap for `andrew-review-labels' mode.")

(define-key andrew-review-labels-keymap
  "t" 'andrew-review-labels-insert-tested-by)
(define-key andrew-review-labels-keymap
  "r" 'andrew-review-labels-insert-reviewed-by)
(define-key andrew-review-labels-keymap
  "a" 'andrew-review-labels-insert-approved-by)
(define-key andrew-review-labels-keymap
  "k" 'andrew-review-labels-insert-acked-by)

(defvar andrew-review-labels-keymap-prefix "C-c +"
  "Prefix for the `andrew-review-labels' keymap.")

;;;###autoload
(define-minor-mode andrew-review-labels
  "Toggle Review Labels mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Read Labels mode is enabled additional key combinations are
added to the keymap to insert review labels.  All keys are
prefixed with `andrew-review-labels-keymap-prefix' prefix.

\\{andrew-review-labels-keymap}"
  ;; The initial value.
  :init-value nil

  ;; The indicator for the mode line.
  :lighter ""

  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c +")
              andrew-review-labels-keymap)
            map))
