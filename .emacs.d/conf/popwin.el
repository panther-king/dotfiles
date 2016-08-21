;; ポップアップウインドウ
(el-get-bundle popwin
  :features popwin
  (with-eval-after-load-feature 'popwin
    (popwin-mode 1)
    (setq special-display-function 'popwin:display-buffer)))
