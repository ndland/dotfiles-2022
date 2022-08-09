;;; org-config.el --- Configuration for org-mode

;;; Commentary:

;;; Code:
(require 'use-package)
(require 'evil)
(require 'olivetti)

(add-hook 'org-mode-hook 'flyspell-mode)

(setq fill-column 120)

;; Latest version of org-mode
(defvar nl/org-directory "~/code/github.com/ndland/org/")

(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)

(defun nl/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook (lambda () (nl/org-mode-setup)))
(add-hook 'org-mode-hook 'olivetti-mode)

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
	("repair" . ?r)))

;; TODO: Add these packages to packages.el
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; (defun nl/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 130
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . nl/org-mode-visual-fill))

(use-package org-contrib
  :after org
  :config
  (require 'org-contacts))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/code/github.com/ndland/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'org-config)

;;; org-config.el ends here
