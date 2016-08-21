;; redo
(el-get-bundle redo+
  :features redo+
  (global-set-key (kbd "C-M-/") 'redo)
  (setq undo-no-redo t)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))
