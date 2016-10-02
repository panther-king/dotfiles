;; 構文チェック
(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ヒントはポップアップで表示させる
(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-timeout 10)
  (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
