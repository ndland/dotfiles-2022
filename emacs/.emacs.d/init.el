;;; init.el --- Entry point for Emacs configuration

;;; Commentary:

;;; Code:
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font" ))
(set-face-attribute 'default t :font "JetBrainsMono Nerd Font" :height 120 )

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UI improvements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Handle temp files
(setq
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 create-lockfiles nil
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory )))

(global-display-line-numbers-mode t)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil) ;; lock files will kill `npm start'

;; This is only to satisfy flycheck
(defvar js-indent-level)
(setq js-indent-level 2)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                org-agenda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; ----------------------------------------------------------------------------
;;; This is where my newly created config files are getting loaded
;;; ----------------------------------------------------------------------------

;; Load user config directory
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Packages
(require 'packages)
(require 'org-config)
(require 'evil-config)
(require 'completions)
(require 'keymaps)
(require 'typescript)

;;; ----------------------------------------------------------------------------
;;; This is the end of my 'new' config
;;; ----------------------------------------------------------------------------

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; Niceties from doom
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init(doom-modeline-mode 1))

;; Better documentation
(use-package helpful
  :config
  (define-key global-map [remap describe-function] #'helpful-callable)
  (define-key global-map [remap describe-variable] #'helpful-variable)
  (define-key global-map [remap describe-key] #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package projectile
  :config
  (setq projectile-mode t))

(use-package magit)

;; TODO: This isn't working, why?
(use-package magit-todos
  :commands (magit-todos-mode))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(wakatime-api-key "15252bb2-a832-4b9f-8b4b-bdfd914ce63f")
 '(wakatime-cli-path "/usr/local/bin/wakatime-cli")
 '(warning-suppress-types '((use-package) (use-package) (lsp-mode) (lsp-mode))))
