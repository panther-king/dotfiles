;; Markdown
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'whitespace-action) nil))))
