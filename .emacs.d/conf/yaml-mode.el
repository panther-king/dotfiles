;; YAML
(el-get-bundle yaml-mode
  :features yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))
