;; ディレクトリツリーの表示
(el-get-bundle neotree
  :features neotree
  (with-eval-after-load-feature 'neotree
    (setq neo-show-hidden-files t)
    (setq neo-create-file-auto-open t)
    (setq neo-smart-open t)
    (setq neo-vc-integration '(face char))
    (setq neo-window-position '(quote right))
    (global-set-key [f8] 'neotree-toggle)))
