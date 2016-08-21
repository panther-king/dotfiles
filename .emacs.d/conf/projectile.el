;; プロジェクト単位での便利操作
(el-get-bundle projectile
  :features projectile)

(el-get-bundle helm-projectile
  :features helm-projectile
  (with-eval-after-load-feature 'helm-projectile
    (projectile-global-mode)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))
