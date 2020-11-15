;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nicholas Land"
      user-mail-address "land.d.nicholas@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "VictorMono Nerd Font" :size 13 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")

(add-hook 'org-mode-hook 'org-hide-block-all)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(require 'ledger-mode)
(setq ledger-clear-whole-transactions 1)
(setq ledger-copy-transaction-insert-blank-line-after -1)
(setq ledger-post-amount-alignment-column 70)
(add-to-list 'evil-emacs-state-modes 'ledger-report-mode)

(add-hook 'ledger-mode-hook
          (lambda ()
            (setq-local tab-always-indent 'complete)
            (setq-local completion-cycle-threshold t)
            (setq-local ledger-complete-in-steps t)))


(require 'evil-ledger)
(add-hook 'ledger-mode-hook #'evil-ledger-mode)

;; Prettier org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(eval-after-load 'flycheck
  '(require 'flycheck-ledger))

;; Keybinds
(map! :after ledger-mode
        (:map ledger-mode-map
          :localleader                  ; Use local leader
          :desc "Reports" "r" #'ledger-report
          :desc "Reconcile" "e" #'ledger-reconcile
          :desc "Clean buffer" "c" #'ledger-mode-clean-buffer))

(setq org-ellipsis "⤵")

(setq projectile-mode-line "Projectile")

;; Start Emacs full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))

(require 'elfeed)
(map! :leader
      :desc "Elfeed" "e" #'elfeed)

(map! :after elfeed-show
        (:map elfeed-show-mode-map
          :localleader                  ; Use local leader
          :desc "Elfeed" "e" #'elfeed
          :desc "Update" "u" #'elfeed-update
          :desc "Next Artical" "n" #'elfeed-show-next
          :desc "Previous Artical" "p" #'elfeed-show-prev))

(map! :after elfeed-search
        (:map elfeed-search-mode-map
          :localleader                  ; Use local leader
          :desc "Update" "u" #'elfeed-update))

(setq elfeed-feeds
      '(("https://xkcd.com/atom.xml" comic)
        ("https://news.ycombinator.com/rss" tech)
        ("https://www.espn.com/espn/rss/news" sports)
        ("https://blog.docker.com/feed" tech)
        ("http://feeds.arstechnica.com/arstechnica/index" tech)))

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
