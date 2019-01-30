;; ripgrepでの検索
(use-package ripgrep
  :ensure t
  :bind (("C-c r g" . ripgrep-regexp))
  :config
  (setq ripgrep-executable "~/.cargo/bin/rg")
  (setq ripgrep-arguments '("-S")))
