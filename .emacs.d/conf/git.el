;; git
(el-get-bundle git-commit
  :features git-commit)

(el-get-bundle gitconfig)

(el-get-bundle gitignore-mode
  :features gitignore-mode)

(el-get-bundle git-gutter-fringe+
  :depends (with-editor)
  :features git-gutter-fringe+
  (with-eval-after-load-feature 'git-gutter-fringe+
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
    (set-face-foreground 'git-gutter-fr+-deleted "red")))
