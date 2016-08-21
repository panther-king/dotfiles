;; 複数ウインドウ制御
(el-get-bundle elscreen
  :features elscreen
  (setq elscreen-prefix-key (kbd "C-z"))
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (with-eval-after-load-feature 'elscreen
    (elscreen-start)))
