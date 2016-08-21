;; C-a/C-eでファイルの先頭末尾へジャンプ
(el-get-bundle sequential-command-config
  :depends sequential-command
  :features sequential-command-config
  (global-set-key (kbd "C-a") 'seq-home)
  (global-set-key (kbd "C-e") 'seq-end))
