;; Enable packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set the theme
(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox-dark-hard t))

;; Auto complete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Web-mode
(use-package web-mode
  :ensure t)

;; Set the font
(set-frame-font "Inconsolata Nerd Font 14" nil t)

;; UI improvements
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1")) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf")))))
