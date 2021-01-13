;; This is just a jump off point for my Emacs config
;; I will continue to modify this to my own personal
;; preference as time goes on.

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips

(menu-bar-mode -1) ; Disable the menu bar

;; Set up the visual bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "VictorMono Nerd Font" :height 100)

(load-theme 'doom-palenight t)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line) 
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :init (counsel-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)) 

(use-package doom-themes)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer nl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Rune is just an arbitrary name space can be changed
  (nl/leader-keys
    ;; This is the prefix
    "t" '(:ignore t :which-key "Toggles")
    ;; This comes after the prefix is triggered
    "tt" '(counsel-load-theme :which-key "Choose Theme")

    "f" '(:ignore t :which-key "File")
    "ff" '(counsel-find-file :which-key "Find File")
    "fs" '(save-buffer :which-key "Save File")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Git Status")

    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Org Agenda")
    "oc" '(org-capture :which-key "Org Capture")
    "oh" '(org-archive-subtree-default :which-key "Org Archive")
    "or" '(org-refile :which-key "Org Refile")
    "ot" '(org-todo :which-key "Org TODO")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(counsel-ibuffer :which-key "Switch Buffers"))

(use-package hydra)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-serach-path '("~/dev")))
  ;; When you switch projects, load dired first
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(defun nl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1))

(defun nl/org-heading-setup ()
  ;; Scale headings
  (dolist (face '((org-level-1 . 1.5)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.1)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'normal :height (cdr face))))

(use-package org
  :hook (org-mode . nl/org-mode-setup)
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
	'("~/Dropbox/org/inbox.org"
	  "~/Dropbox/org/tasks.org"))
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t)
  (nl/org-heading-setup)

  (setq org-refile-targets
	'(("tasks.org" :maxlevel . 1)
	  ("archive.org" :maxlevel . 2)))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(e)" "WAITING(w)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(a)")))

  (setq org-capture-templates
	'(("t" "Tasks")
	  ("tt" "Task" entry (file+olp "~/Dropbox/org/tasks.org" "Inbox")
	   "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
	  ("j" "Journal")
	  ("jj" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
	   "\n* %<%I:%M %p> - %^{Summary} :journal:\n\n%?\n" :empty-lines 1 :clock-in :clock-resume))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; Look into using Forge by same author as magit


;; Figure out how to manage this shit.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" default))
 '(package-selected-packages
   '(org org-bullets evil-magit magit counsel-projectile projectile hydra evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters use-package ivy doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
