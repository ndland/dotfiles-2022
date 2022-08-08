;;; kemaps.el --- Keymaps I've defined

;;; Commentary:

;;; Code:

(require 'package)
(unless (package-installed-p 'general)
  (package-install 'general))

(require 'general)

(general-unbind 'normal
  ",")

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
			:states '(normal insert motion emacs)
			;; :prefix my-leader
			:prefix ",")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix ", m")

;; (general-define-key
;;  :states 'motion
;;  ";" 'evil-ex
;;  ":" 'evil-repeat-find-char)

(my-leader-def
  :keymaps 'normal
  ;; bind ", a"
  "re" 'restart-emacs
  "a" 'org-agenda
  "b" 'counsel-bookmark
  "c" 'org-capture)

;; to prevent your leader keybindings from ever being overridden (e.g. an evil
;; package may bind "SPC"), use :keymaps 'override
(my-leader-def
  :states 'normal
  :keymaps 'override
  "re" 'restart-emacs
  "b" 'switch-to-buffer
  "g" 'magit-status
  "pf" 'projectile-find-file
  "f" 'find-file)

;; ** Mode Keybindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link)

;; change evil's search module after evil has been loaded (`setq' will not work)
(general-setq evil-search-module 'evil-search)

(provide 'keymaps)

;;; keymaps.el ends here
