(defface ispell-correct-face '((t (:background "pale green"))) nil)

;; Uses the pulse library to quickly highlight a correct spelling.
(defadvice ispell-word (around apb-ispell-word)
  "Highlight correct spelling"
  (require 'pulse)
  (let* ((pos (point))
         (word (ispell-get-word following))
         ;; De-structure return word info list.
         (start (car (cdr word)))
         (end (car (cdr (cdr word))))
         (word (car word))
         (offset (- pos end)))
    ;; Now perform the actual ispell-work call.
    (let ((ispell-result ad-do-it))
      ;; Reposition cursor.
      (forward-char offset)
      (unless ispell-result
        ;; Hide the cursor while we flash the word, stops annoying cursor
        ;; flicker.
        (let ((vis (internal-show-cursor-p))
              (win (get-buffer-window (current-buffer))))
          (internal-show-cursor win nil)
          (pulse-momentary-highlight-region start end 'ispell-correct-face)
          (internal-show-cursor win vis))))))

(provide 'andrew-ispell)
