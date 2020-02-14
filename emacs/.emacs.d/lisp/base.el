;;; base.el -- Base configuration for Emacs

;;; Commentary:

;;; Code:

;; UI Improvements
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)

;; Set custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Bootstrap Straight Package Manager
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

(setq inhibit-startup-screen t)

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Add auto-complete
(use-package company
  :straight t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Set color theme
(use-package material-theme
  :straight t
  :config
  (load-theme 'material t))

;; Git
(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status))

;; Access Developer documentation from Emacs
(use-package devdocs
  :straight t
  :bind
  ("C-x d" . devdocs-search))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package helm
  :straight t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
	 ("C-x C-b" . helm-buffers-list))
  :bind (:map helm-map
	      ("<tab>" . helm-execute-persistent-action)
	      ("TAB" . helm-execute-persistent-action)
	      ("C-z" . helm-select-next-action)
	      ("<backspace>" . helm-find-files-up-one-level)))

(use-package restart-emacs
  :straight t
  :bind
  ("C-x r" . restart-emacs))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 6)
			  (bookmarks . 5)
			  (agenda . 5)
			  (registers . 5))))

(use-package all-the-icons
  :straight t)

(use-package treemacs
  :straight t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(set-frame-font "Victor Mono 13" nil t)

;; Save backups to tree structure
(defun my-backup-file-name(fpath)
  "Return a new file path of a given file path.
If the new path's directories do not exist, create them."
  (let* (
	 (backupRootDir "~/.emacs.d/backup")
	 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
	 (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

(setq ispell-program-name "/usr/local/bin/ispell")
(global-set-key (kbd "<f8>") 'ispell-word)

(provide 'base)

;;; base.el ends here
