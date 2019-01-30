;; 使い捨てファイルの生成
(use-package open-junk-file
  :ensure t
  :config
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))
