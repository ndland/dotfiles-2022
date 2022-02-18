;;; completions.el  --- Emacs completion configureation

;;; Commentary:

;;; Code:
(require 'vertico)
(require 'orderless)
(require 'consult)
(require 'company)
(require 'consult-company)
(require 'marginalia)
(require 'embark)

(global-company-mode)
(setq company-idle-delay 0.0
      company-minimum-prefix-length 1)

(vertico-mode)

(setq completion-styles '(orderless))

(define-key company-mode-map [remap completion-at-point] #'consult-company)

(marginalia-mode)

(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "C-h  b") 'embark-bindings)

(provide 'completions)

;;; completions.el ends here
