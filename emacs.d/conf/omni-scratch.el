;; メジャーモードごとのscratchバッファ
(use-package omni-scratch
  :ensure t
  :bind (("C-M-s" . omni-scratch-new-scratch-major-buffer)))
