;;; Ruby.el starts here

;;; Commentary:

;;; Code:

(use-package enh-ruby-mode
  :straight t
  :mode
  (("\\.rb\\'" . ruby-mode)))
  
(provide 'lang-ruby)
