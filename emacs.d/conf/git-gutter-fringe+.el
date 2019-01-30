;; gitの編集箇所ハイライト
(use-package git-gutter-fringe+
  :ensure t
  :config
  (global-git-gutter+-mode)
  (setq-default left-fringe-width 20)
  (fringe-helper-define 'git-gutter-fr+-modified nil
                        "...xx..."
                        "...xx..."
                        "...xx..."
                        "...xx..."
                        "...xx..."
                        "........"
                        "...xx..."
                        "...xx...")
  (set-face-foreground 'git-gutter-fr+-modified "green")
  (set-face-foreground 'git-gutter-fr+-added "yellow")
  (set-face-foreground 'git-gutter-fr+-deleted "red"))
