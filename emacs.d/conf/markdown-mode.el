;; Markdown
(use-package markdown-mode
  :mode (("\\.md$" . gfm-mode))
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (electric-indent-local-mode -1))))
