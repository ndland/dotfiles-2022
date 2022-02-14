;; Initialize package sources
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

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

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

(setq js-indent-level 2)

;; Packages

;; Latest version of org-mode
(use-package org)

;; Evil; Vim bindings
(use-package evil 
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package helm-core)

(use-package helm
  :after helm-core
  :config
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)
  (helm-mode 1))

;; Niceties from doom
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-oceanic-next t)
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

(use-package company
  :config
  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1))

(use-package projectile
  :config
  (setq projectile-mode t))

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; Neccessary for OS X
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(use-package js2-mode
  :hook (js-mode . js2-minor-mode))

;; Give me some metrics about my coding habits
(use-package wakatime-mode
  :init
  (global-wakatime-mode))

(use-package magit)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))
