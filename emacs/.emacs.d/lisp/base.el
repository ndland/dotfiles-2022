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

(use-package linum-relative
  :straight t
  :bind
  ("C-x l" . linum-mode)
  ("C-x t" . linum-relative-toggle))

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
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("<tab>" . helm-execute-persistent-action)
	      ("TAB" . helm-execute-persistent-action)
	      ("C-z" . helm-select-next-action)
	      ("<backspace>" . helm-find-files-up-one-level)))

(use-package restart-emacs
  :straight t
  :bind
  ("C-x r" . restart-emacs))

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

(provide 'base)

;;; base.el ends here
