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

;; Packages

;; Latest version of org-mode
(defvar nl/org-directory "~/dev/github.com/ndland/org/")

(defun nl/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 1))

(use-package org
  :after evil
  :hook (org-mode . nl/org-mode-setup)
  :bind (("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture))
  :config
  (evil-define-key '(normal visual) org-mode-map
  "TAB" 'org-cycle)

  (evil-define-key '(normal insert visual emacs) org-agenda-mode-map
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line)

  (setq org-log-into-drawer t)
  (setq org-agenda-files `(,(concat nl/org-directory "inbox.org")))
  (setq org-contacts-files `(,(concat nl/org-directory "inbox.org")))

  (setq org-ellipsis " ⌄"
        org-hide-emphasis-markers t)

  (setq org-habit-graph-column 60)
  (setq org-tags-column 120
        org-auto-align-tags t)
  (setq org-startup-with-inline-images t
        org-image-actual-width 500)

  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil     :inherit 'fixed-pitch)
  (set-face-attribute 'org-link nil     :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox-statistics-todo nil :inherit 'fixed-pitch :foreground "DarkOrange1")
  (set-face-attribute 'org-checkbox-statistics-done nil :inherit 'fixed-pitch :foreground "lime green")
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-agenda-calendar-sexp nil :inherit 'variable-pitch :foreground "LemonChiffon")

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
          (sequence "BACKLOG(b!)" "PLAN(p!)" "READY(r!)" "ACTIVE(a!)" "REVIEW(e!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "COMPLETED(c!)" "CANCELLED(l@/!)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
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
	`(("t" "Tasks")
	  ("tt" "Task" entry
	   (file+olp ,(concat nl/org-directory "inbox.org") "Inbox")
	   "* TODO %?\nCaptured: %U\n%a\n %i"
	   :empty-lines 0)
	  ("td" "Task Today" entry
	   (file+olp ,(concat nl/org-directory "inbox.org") "Inbox")
	   "* TODO %?\nSCHEDULED: %t\nCaptured: %U\n%a\n %i"
	   :empty-lines 0)
	  ("c" "Contacts")
	  ("cf" "Family" entry (file+headline ,(concat nl/org-directory "inbox.org") "Family")
	   "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %^{Email}
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE: %^{Note}
:END:"
	   :empty-lines 0)
	  ("cr" "Friends" entry (file+olp ,(concat nl/org-directory "inbox.org") "Contacts" "Friends")
	   "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %^{Email}
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE: %^{Note}
:END:"
	   :empty-lines 0)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)))

  (setq org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("@home" . ?H)
          ("@errand" . ?E)
          ("@work" . ?W)
          ("finance" . ?F)
          ("event" . ?v)
          ("habit" . ?a)
          ("chore" . ?C)
          ("plex" . ?P)
          ("hobbies" . ?h)
          ("productivity" . ?p)
          ("emacs" . ?e)
          ("repair" . ?r))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun nl/org-mode-visual-fill ()
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nl/org-mode-visual-fill))

(use-package org-contrib
  :after org
  :config
  (require 'org-contacts))

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/dev/github.com/ndland/org-roam"))
  (org-roam-db-autosync-mode)
  :bind (("C-c n t" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)))

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

(use-package emojify
  :hook (after-init . global-emojify-mode))
