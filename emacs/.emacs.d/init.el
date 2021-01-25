;;; Init.el --- This is my Emacs Config

;;; Commentary:

;;; This is just a jump off point for my Emacs config I will continue
;;; to modify this to my own personal preference as time goes on.

;;; Code:
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips

(menu-bar-mode -1) ; Disable the menu bar

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Set up the visual bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "VictorMono Nerd Font" :height 100)

(electric-pair-mode 1)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

(require 'use-package)
;; (setq use-package-always-ensure t)

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

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

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
    "gf" '(magit-gitflow-popup :which-key "Git Flow")

    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Org Agenda")
    "oc" '(org-capture :which-key "Org Capture")
    "oh" '(org-archive-subtree-default :which-key "Org Archive")
    "or" '(org-refile :which-key "Org Refile")
    "ot" '(org-todo :which-key "Org TODO")

    "s" '(:ignore t :which-key "Snippets")
    "si" '(yas-insert-snippet :which-key "Insert Snippet")
    "sn" '(yas-new-snippet :which-key "New Snippet")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(counsel-ibuffer :which-key "Switch Buffers")))

(use-package hydra)

(use-package company
  :straight t
  :init
  (global-company-mode 1))

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

(use-package magit-gitflow
  :straight t
  :hook
  (magit-mode . turn-on-magit-gitflow))

(use-package diff-hl
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(defun nl/org-mode-setup ()
  ;; (org-indent-mode)
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
  :bind
  ([remap org-set-tags-command] . #'counsel-org-tag)
  :config
  (setq org-log-into-drawer t)
  (setq org-agenda-files
	'("~/Dropbox/org/tasks.org"
	  "~/Dropbox/org/habits.org"
	  "~/Dropbox/org/notes.org"))
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t)
  (nl/org-heading-setup)

  (setq org-habit-graph-column 60)

  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)

  (setq org-refile-targets
	'(("tasks.org" :maxlevel . 1)
	  ("notes.org" :maxlevel . 1)
	  ("habits.org" :maxlevel . 1)
	  ("archive.org" :maxlevel . 2)))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b!)" "PLAN(p!)" "READY(r!)" "ACTIVE(a!)" "REVIEW(e!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "COMPLETED(c!)" "CANCELLED(l@/!)")))

  (setq org-todo-keyword-faces
	'(("TODO" . org-warning)
	  ("NEXT" . (:foreground "gold" :weight bold))
	  ("DONE" . (:foreground "lime green" :weight bold))
	  ("BACKLOG" . (:foreground "dim gray" :weight regular))
	  ("PLAN" . (:foreground "orange red" :weight regular))
	  ("READY" . (:foreground "spring green" :weight bold))
	  ("ACTIVE" . (:foreground "yellow" :weight bold))
	  ("REVIEW" . (:foreground "orange" :weight bold))
	  ("WAITING" . (:foreground "salmon" :weight bold))
	  ("HOLD" . (:foreground "tomato" :weight bold))
	  ("COMPLETED" . (:foreground "lime green" :weight bold))
	  ("CANCELLED" . (:foreground "red" :weight bold))))

  (setq org-capture-templates
	'(("t" "Tasks")
	  ("tt" "Task" entry
	   (file+olp "~/Dropbox/org/tasks.org" "Inbox")
	   "* TODO %?\nCaptured: %U\n %a\n %i"
	   :empty-lines 0)
	  ("td" "Task Today" entry
	   (file+olp "~/Dropbox/org/tasks.org" "Inbox")
	   "* TODO %?\nSCHEDULED: %t\nCaptured: %U\n %a\n %i"
	   :empty-lines 0)
	  ("j" "Journal")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/Dropbox/org/journal.org" "Journal")
	   "\n* %<%I:%M %p> - %^{Summary} :journal:\n\n%?\n"
	   :empty-lines 0 :clock-in :clock-resume)
	  ("jb" "Blender" entry
	   (file+olp+datetree "~/Dropbox/org/journal.org" "Blender")
	   "\n* %<%I:%M %p> - %^{Summary} :journal:blender:\n\n%?\n"
	   :empty-lines 0 :clock-in :clock-resume)
	  ("je" "Exercise" entry
	   (file+olp+datetree "~/Dropbox/org/journal.org" "Exercise")
	   "\n* %<%I:%M %p> - %^{Summary} :journal:exercise:\n\n%?\n"
	   :empty-lines 0 :clock-in :clock-resume)
	  ("jg" "Guitar" entry
	   (file+olp+datetree "~/Dropbox/org/journal.org" "Guitar")
	   "\n* %<%I:%M %p> - %^{Summary} :journal:guitar:\n\n%?\n"
	   :empty-lines 0 :clock-in :clock-resume)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package beancount-mode
  :straight (beancount-mode
	     :type git
	     :host github
	     :repo "beancount/beancount-mode")
  :hook
  (beancount-mode . outline-minor)
  :bind
  ("C-c C-n" . outline-next-visible-heading)
  ("C-c C-p" . outline-previous-visible-heading)
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package yasnippet
  :straight t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :straight t)

;; Look into using Forge by same author as magit


;; Figure out how to manage this shit.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-describe-function-function #'helpful-callable nil nil "Customized with use-package helpful")
 '(counsel-describe-variable-function #'helpful-variable nil nil "Customized with use-package helpful")
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" default))
 '(fci-rule-color "#676E95")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f2b" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f2b" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f2b" "#676E95"))
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1 nil nil "Customized with use-package magit")
 '(objed-cursor-color "#ff5370")
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(beancount-mode markdown-mode diff-hl spacemacs-theme org org-bullets evil-magit magit counsel-projectile projectile hydra evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters use-package ivy doom-modeline))
 '(rustic-ansi-faces
   ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(vc-annotate-background "#292D3E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7de81")
    (cons 60 "#ebd476")
    (cons 80 "#ffcb6b")
    (cons 100 "#fcb66b")
    (cons 120 "#f9a16b")
    (cons 140 "#f78c6c")
    (cons 160 "#e78e96")
    (cons 180 "#d690c0")
    (cons 200 "#c792ea")
    (cons 220 "#d97dc1")
    (cons 240 "#ec6898")
    (cons 260 "#ff5370")
    (cons 280 "#d95979")
    (cons 300 "#b36082")
    (cons 320 "#8d678b")
    (cons 340 "#676E95")
    (cons 360 "#676E95")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Init.el ends here
