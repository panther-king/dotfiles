;; 引用符と各種カッコの補完
(use-package smartchr
  :load-path "github/emacs-smartchr"
  :config
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"")))
  (bind-key "(" (smartchr '("(`!!')" "(")))
  (bind-key "[" (smartchr '("[`!!']" "[")))
  (bind-key "{" (smartchr '("{`!!'}" "{"))))
