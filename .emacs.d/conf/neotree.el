;; ディレクトリツリーの表示
(use-package neotree
  :config
  (bind-key* [f8] 'neotree-toggle)
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open t)
  (setq neo-vc-integration '(face char))
  (setq neo-window-position '(quote right)))
