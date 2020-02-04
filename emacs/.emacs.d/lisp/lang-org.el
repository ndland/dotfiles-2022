;;; lang-org.el -- Configuration for org-mode

;;; Commentary:

;;; Code:
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)

(use-package org-bullets
  :straight t)

(setq org-agenda-files '("~/Dropbox/org"))
(setq org-closed-keep-when-no-todo t)
(setq org-log-into-drawer t)

(setq org-default-notes-file "~/Dropbox/org/index.org")
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
			  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"  "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "firebrick3" :weight bold)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "* TODO %?\n")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\n\nEntered on %U\n")))

;; Keybinds
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "<f11>") 'org-clock-goto)

(provide 'lang-org)

;;; lang-org.el ends here
