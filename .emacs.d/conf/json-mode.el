;; JSON
(el-get-bundle json-mode
  :features json-mode
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (setq js-indent-level 2))
