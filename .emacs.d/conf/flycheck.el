;; 構文チェック
(el-get-bundle flycheck
  :features flycheck
  (with-eval-after-load-feature 'flycheck
    (add-hook 'after-init-hook #'global-flycheck-mode)))

(el-get-bundle flycheck-pos-tip
  :features flycheck-pos-tip
  (setq flycheck-pos-tip-timeout -1)
  (with-eval-after-load-feature 'flycheck-pos-tip
    (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
