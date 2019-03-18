;; gitの編集箇所ハイライト
(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :config
  (global-git-gutter-mode t))
