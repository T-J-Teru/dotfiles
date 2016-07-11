(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :search-type tree)
     (:name "unread" :query "tag:unread" :key "u" :search-type tree)
     (:name "flagged" :query "tag:flagged" :key "f" :search-type tree)
     (:name "sent" :query "tag:sent" :key "t" :search-type tree)
     (:name "drafts" :query "tag:draft" :key "d" :search-type tree)
     (:name "all mail" :query "*" :key "a" :search-type tree)
     (:name "unread-threads" :query "tag:mythread and tag:unread" :search-type tree)
     (:name "recent-unread" :query "tag:unread AND date:2h.. AND NOT tag:buildroot" :search-type tree)
     (:name "recent-inbox" :query "tag:unread AND tag:inbox AND date:24h.." :search-type tree)
     (:name "todo" :query "tag:todo" :search-type tree))))
 '(send-mail-function (quote smtpmail-send-it))
 '(template-use-package t nil (template)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
