;;; lang-org.el -- Configuration for org-mode

;;; Commentary:

;;; Code:
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package org-bullets
  :straight t)

(setq org-agenda-files '("~/Google Drive/org"))
(setq org-log-done 'note)
(setq org-closed-keep-when-no-todo t)

(setq org-agenda-include-diary t)
(setq org-default-notes-file "~/Google Drive/org/index.org")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Google Drive/org/inbox.org" "Tasks")
         "* TODO %?\n")
	("r" "Reminder" entry (file+headline "~/Google Drive/org/inbox.org" "Reminders")
	 "* Reminder %?\n")
        ("j" "Journal" entry (file+datetree "~/Google Drive/org/journal.org")
         "* %?\n\nEntered on %U\n")))

;; Keybinds
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(provide 'lang-org)

;;; lang-org.el ends here
