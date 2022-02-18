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

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                org-agenda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Load user config directory
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Packages
(require 'packages)
(require 'org-config)
(require 'evil-config)
(require 'completions)

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

(use-package projectile
  :config
  (setq projectile-mode t))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred))

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

;; TODO: This isn't working, why?
(use-package magit-todos
  :commands (magit-todos-mode))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))
