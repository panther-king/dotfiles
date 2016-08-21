;; 使い捨てファイルの生成
(el-get-bundle open-junk-file
  :type "emacswiki"
  :features open-junk-file
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))
