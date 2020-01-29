;;; lang-org.el -- Configuration for org-mode

;;; Commentary:

;;; Code:
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package org-bullets
  :straight t)

(setq org-agenda-files '("~/Dropbox/org"))
(setq org-log-done 'note)
(setq org-closed-keep-when-no-todo t)
(setq org-log-into-drawer t)

(setq org-agenda-include-diary t)
(setq org-default-notes-file "~/Dropbox/org/index.org")
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "IN PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("WAITING" . (:foreground "yellow"))
	("IN PROGRESS" . (:foreground "orange"))
	("DONE" . (:foreground "green"))
	("CANCELLED" . (:foreground "blue"))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "* TODO %?\n")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\n\nEntered on %U\n")))

;; Keybinds
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(provide 'lang-org)

;;; lang-org.el ends here
