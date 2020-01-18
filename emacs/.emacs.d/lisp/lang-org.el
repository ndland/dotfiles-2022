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

;; Keybinds
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'lang-org)

;;; lang-org.el ends here
