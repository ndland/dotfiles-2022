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

(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))

(setq org-default-notes-file "~/Dropbox/org/inbox.org")
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords '((sequence "TODO(t)" "MEETING(m)" "HABIT(h)" "CALL(p)" "NEXT(n)" "|" "DONE(d)")
			  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"  "CANCELLED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("MEETING" :foreground "DarkGoldenrod1" :weight bold)
	("HABIT" :foreground "SpringGreen1" :weight bold)
	("CALL" :foreground "DodgerBlue1" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "firebrick3" :weight bold)))

(setq org-capture-templates
      '(("t"
	 "Todo"
	 entry
	 (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n" :empty-lines 1)
	("m"
	 "Meeting"
	 entry
	 (file org-default-notes-file)
	 "* MEETING with %? :meeting:\n %^{Context} %u" :empty-lines 1)
	("p"
	 "Phone"
	 entry
	 (file org-default-notes-file)
	 "* CALL %? :call:\n %^{Phone_Number}p %^{Context}p %u" :empty-lines 1)
	("h"
	 "Habit"
	 entry
	 (file org-default-notes-file)
	 "* HABIT %? :habit:\n %^{SCHEDULED}p %^{Behavior}p %^{Location}p %u" :empty-lines 1)
        ("j"
	 "Journal"
	 entry
	 (file+datetree "~/Dropbox/org/journal.org")
         "**** %<%r> %?%a \n %^{Mood}p \n" :tree-type week :empty-lines 1)))

;; Keybinds
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c f") 'org-clock-goto)

(provide 'lang-org)

;;; lang-org.el ends here
