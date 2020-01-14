;;; lang-org.el -- Configuration for org-mode

;;; Commentary:

;;; Code:
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(use-package org-bullets
  :straight t)

(setq org-agenda-files '("~/Google Drive/org"))

(provide 'lang-org)

;;; lang-org.el ends here
