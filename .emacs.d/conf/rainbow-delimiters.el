;; 対応するカッコの色付け
(el-get-bundle rainbow-delimiters
  :features rainbow-delimiters
  (with-eval-after-load-feature 'rainbow-delimiters
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))
