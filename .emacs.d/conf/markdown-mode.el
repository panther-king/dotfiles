;; Markdown
(el-get-bundle markdown-mode
  :features markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'whitespace-action) nil))))
