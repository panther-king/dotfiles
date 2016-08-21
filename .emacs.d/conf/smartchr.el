;; 引用符と各種カッコの補完
(el-get-bundle smartchr
  :features smartchr
  (with-eval-after-load-feature 'smartchr
  (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))))
