;; 構文チェック
(use-package flycheck
  :ensure t
  :bind (("<f12>" . flycheck-next-error)
         ("<f11>" . flycheck-previous-error))
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; ヒントはポップアップで表示させる
(use-package flycheck-pos-tip
  :ensure t
  :requires flycheck
  :config
  (setq flycheck-pos-tip-timeout 16)
  (custom-set-variables '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
