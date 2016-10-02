;; C-a/C-eでファイルの先頭末尾へジャンプ
(use-package sequential-command-config
  :load-path "elisp"
  :bind (("C-a" . seq-home)
         ("C-e" . seq-end)))
