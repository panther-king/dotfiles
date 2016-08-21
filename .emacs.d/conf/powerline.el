;; モードラインのリッチ化
(el-get-bundle powerline
  :features powerline
  (with-eval-after-load-feature 'powerline
    (powerline-default-theme)))
