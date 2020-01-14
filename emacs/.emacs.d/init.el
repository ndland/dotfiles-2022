;;; init.el -- Emacs config file

;;; Commentary:

;;; Code:

;; Load user config directory
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'base)
(require 'lang-ruby)
(require 'lang-org)

;;; init.el ends here
