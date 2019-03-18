;; 現在行のハイライト
(when (eq window-system 'x)
  (global-hl-line-mode t))

;; カッコの強調表示
(use-package paren
  :ensure nil
  :init
  (show-paren-mode t)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; 操作結果のハイライト
(use-package volatile-highlights
  :ensure t
  :diminish
  :hook
  (after-init-hook . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#ff3333" :background "#ffcdcd")))))
