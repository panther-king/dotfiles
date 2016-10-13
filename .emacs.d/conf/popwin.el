;; ポップアップウインドウ
(use-package popwin
  :config
  (popwin-mode 1)
  (setq special-display-function 'popwin:display-buffer))
