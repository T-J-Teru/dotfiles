;;;###autoload
(defun andrew/make-executable-after-save ()
  "Called when a file is saved.  Makes the file executable if it
looks like a script."
  (let ((temp (substring buffer-file-name -3)))
    (if (or (equal temp ".pl")
            (equal temp ".sh"))
        (executable-make-buffer-file-executable-if-script-p))))
