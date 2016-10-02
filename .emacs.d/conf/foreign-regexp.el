;; 正規表現検索・置換
;; M-s M-s incremental search
;; M-s M-r decremental search
;; M-s M-% replacement
(use-package foreign-regexp
  :config
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp)))
