;; ポップアップウインドウ
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (setq special-display-function 'popwin:display-buffer))
