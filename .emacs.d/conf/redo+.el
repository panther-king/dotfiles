;; redo
(use-package redo+
  :bind (("C-M-/" . redo))
  :config
  (setq undo-no-redo t)
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))
