;; Ruby
(el-get-bundle ruby-mode
  :features ruby-mode
  (add-to-list 'auto-mode-alist '("\\.\\(rb\\|rake\\|jbuilder\\)$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\(Gemfile\\|Guardfile\\)$" . ruby-mode)))
