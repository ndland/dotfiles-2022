;;; lang-markdown.el -- Configuration for Markdown

;;; Commentary:

;;; Code:

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-commannd "mulitmarkdown"))

(provide 'lang-markdown)

;;; lang-markdown.el ends here
