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
(use-package git-gutter
  :init
  (global-git-gutter-mode))
;; Neccessary for OS X
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))
;; Give me some metrics about my coding habits
(use-package wakatime-mode
  :init
  (global-wakatime-mode))
(use-package tree-sitter
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs)
(use-package olivetti)


(provide 'packages)
