(require 'use-package)

(use-package flycheck
  :init (global-flycheck-mode))
(use-package evil
  :init
  (setq evil-want-keyinding nil))
(use-package evil-collection
  :after evil)
(use-package org)
(use-package vertico)
(use-package orderless)
(use-package consult)
(use-package company)
(use-package consult-company
  :after company)
(use-package marginalia)
(use-package embark)

(provide 'packages)
