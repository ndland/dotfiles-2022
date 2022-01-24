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

(straight-use-package 'use-package)
;; Ok, putting this here gives me the latest version
(straight-use-package 'org)
(setq straight-use-package-by-default t)

(require 'use-package)

(use-package spacemacs-theme
  :defer t)

(use-package one-themes
  :defer t)

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode -1) ; Disable toolbar
(tooltip-mode -1) ; Disable tooltips

(menu-bar-mode -1) ; Disable the menu bar

;; Set up the visual bell
(setq visible-bell t)

(electric-pair-mode 1)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                nov-mode-hook
                treemacs-mode-hook
                org-agenda-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Start Emacs fullscreen
(custom-set-variables
 '(initial-frame-alist '((fullscreen . maximized))))

;; Set the font a little bigger in OS X
(if (memq window-system '(mac ns))
    (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 135)
  (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 100))

;; Set the variable pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "FiraCode Nerd Font Mono"
                    :height 135)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Etoile"
                    :height 135)

(setq
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 create-lockfiles nil
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))



(defvar nl/emacs-dotfile-directory "/Users/nland/dev/github.com/ndland/dotfiles/emacs/")
(defvar nl/emacs-linux-dotfile-directory "/home/nland/dev/github.com/ndland/dotfiles/emacs/")

;; Automatically tangle our Emacs.org config file when we save it
(defun nl/org-babel-tangle-config ()
  "This function is to automatically write to '~/.emacs.d/init.el'."
  (when
      (or
       (string-equal (file-name-directory (buffer-file-name)) nl/emacs-dotfile-directory)
       (string-equal (file-name-directory (buffer-file-name)) nl/emacs-linux-dotfile-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nl/org-babel-tangle-config)))

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun nl/search-org-notes ()
  (interactive)
  (counsel-rg "" "~/Dropbox/org" nil "Search org notes: "))

(server-start)

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

  ;; nl is just an arbitrary name space can be changed
  (nl/leader-keys
    "a" '(:ignore t :which-key "Applications")
    ;; This comes after the prefix is triggered
    "at" '(vterm :which-key "Vterm")

    ;; This is the prefix
    "t" '(:ignore t :which-key "Toggles")
    ;; This comes after the prefix is triggered
    "tt" '(counsel-load-theme :which-key "Choose Theme")

    "f" '(:ignore t :which-key "File")
    "ff" '(counsel-find-file :which-key "Find File")
    "fs" '(save-buffer :which-key "Save File")
    "ft" '(treemacs :which-key "Toggle Treemacs")
    "fr" '(counsel-recentf :which-key "Recent Files")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Git Status")
    "gf" '(magit-gitflow-popup :which-key "Git Flow")

    "n" '(:ignore t :which-key "Notes")
    "ns" '(nl/search-org-notes :which-key "Search org notes")

    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Org Agenda")
    "oc" '(org-capture :which-key "Org Capture")
    "oh" '(org-archive-subtree-default :which-key "Org Archive")
    "or" '(org-refile :which-key "Org Refile")
    "ot" '(org-todo :which-key "Org TODO")
    "oi" '(org-time-stamp-inactive :which-key "Org timestamp inactive")

    "s" '(:ignore t :which-key "Snippets")
    "si" '(yas-insert-snippet :which-key "Insert Snippet")
    "sn" '(yas-new-snippet :which-key "New Snippet")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(counsel-ibuffer :which-key "Switch Buffers")))

(defun nl/org-heading-setup ()
  ;; Scale headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "Iosevka Etoile" :weight 'normal :height (cdr face))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(defun nl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1))

(use-package org
  :bind ([remap org-set-tags-command] . #'counsel-org-tag)
  :hook (org-mode . nl/org-mode-setup)
  :config
  (setq org-log-into-drawer t)
  ;; (setq org-agenda-files '("~/Dropbox/org/inbox.org"))
  (setq org-contacts-files '("~/Dropbox/org/inbox.org"))

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
        '(("t" "Tasks")
          ("tt" "Task" entry
           (file+olp "~/Dropbox/org/inbox.org" "Inbox")
           "* TODO %?\nCaptured: %U\n%a\n %i"
           :empty-lines 0)
          ("td" "Task Today" entry
           (file+olp "~/Dropbox/org/inbox.org" "Inbox")
           "* TODO %?\nSCHEDULED: %t\nCaptured: %U\n%a\n %i"
           :empty-lines 0)
          ("c" "Contacts")
          ("cf" "Family" entry (file+headline "~/Dropbox/org/inbox.org" "Family")
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
          ("cr" "Friends" entry (file+olp "~/Dropbox/org/inbox.org" "Contacts" "Friends")
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
          ("repair" . ?r)))

  (nl/org-heading-setup)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "TAB" 'org-cycle)

  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'org-agenda-mode-map
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line))

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
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t))

(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :init (counsel-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package magit-gitflow
  :after magit
  :hook
  (magit-mode . turn-on-magit-gitflow))

(use-package diff-hl
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(if (memq window-system '(mac ns))
    (use-package magit-delta
      :after magit
      :config
      (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package company
  :ensure t
  :after lsp-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :after company-mode
  :hook (company-mode . company-box-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

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

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(use-package restart-emacs)

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package golden-ratio
  :config (golden-ratio-mode 1))

(define-advice select-window (:after (window &optional no-record) golden-ratio-resize-window)
    (golden-ratio)
    nil)

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config (ace-window-display-mode 1))

(use-package vterm)

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 120))

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

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :after company
  :init
  (add-hook 'ledger-mode-hook 'company-mode)
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete)
              (setq-local completion-cycle-threshold t)
              (setq-local ledger-complete-in-steps t)))
  (setq ledger-post-amount-alignment-at :end)
  (setq ledger-post-amount-alignment-column 100))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package json-mode
  :mode "\\.json$")

(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)
         ("\\.tsx\\'" . rjsx-mode)
         ("\\.ts\\'" . rjsx-mode))
  :config (setq js-indent-level 2))

;; (use-package typescript-mode
;;  :mode (("\\.tsx\\'" . typescript-mode)))

(defun setup-tide-mode()
  "Setup function for tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (company-mode +1))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . setup-tide-mode))

;; (use-package js2-mode)

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ;; ("\\.jsx\\'" . web-mode)
         ;; ("\\.ts\\'" . web-mode)
         ("\\.html\\'" . web-mode))
         ;; ("\\.tsx\\'" . web-mode))
  :hook ((web-mode . lsp-deferred))
  :config
  (setq company-tooltip-align-annotations t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
  ;; (setq web-mode-content-types-alist
  ;;       '(("tsx" . "\\.ts[x]?\\'"))))

(use-package prettier-js
  :after (rjsx-mode)
  :hook ((json-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :mode "\\.go$"
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (lsp-go-install-save-hooks)

  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))
