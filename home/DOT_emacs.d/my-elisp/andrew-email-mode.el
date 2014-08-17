(defun andrew-email-mode ()
  ;;
  ;; Switch on spell checking.
  (flyspell-mode)
  ;;
  ;; Switch on word wrapping.
  (auto-fill-mode)
  ;;
  ;; Move to the correct place to start typing the email.
  (mail-text)
  ;;
  ;; Allow mutt aliases to be inserted.
  (autoload 'mutt-alias-insert "mutt-alias")
  (local-set-key (kbd "C-c C-a") 'mutt-alias-insert)
)

;; Add a hook to change how email mode operates.
(add-hook 'mail-mode-hook 'andrew-email-mode)

(provide 'andrew-email-mode)
