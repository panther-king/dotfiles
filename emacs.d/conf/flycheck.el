;; 構文チェック
(use-package flycheck
  :bind (("<f12>" . flycheck-next-error)
         ("<f11>" . flycheck-previous-error))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ヒントはポップアップで表示させる
(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-timeout 16)
  (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
