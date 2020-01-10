;;; UI Improvements
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)

;;; Set custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'base)
(require 'lang-ruby)
(require 'lang-org)

;;; Org mode
(use-package org-bullets
  :straight t)

;;; Miscellaneous
(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status))

(use-package base16-theme
  :straight t
  :config
  (load-theme 'base16-materia))

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

;;; Save backups to tree structure
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