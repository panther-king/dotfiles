;; ローマ字で日本語検索
(el-get-bundle migemo
  :features migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (with-eval-after-load-feature 'migemo
    (load-library "migemo")
    (migemo-init)))
