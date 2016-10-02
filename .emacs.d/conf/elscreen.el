;; 複数ウインドウ制御
(use-package elscreen
  :config
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start))
