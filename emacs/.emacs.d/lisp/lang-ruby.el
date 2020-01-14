;;; lang-ruby.el -- Configuration for Ruby

;;; Commentary:

;;; Code:

(use-package enh-ruby-mode
  :straight t
  :mode
  (("\\.rb\\'" . ruby-mode)))
  
(provide 'lang-ruby)

;;; lang-ruby.el ends here
