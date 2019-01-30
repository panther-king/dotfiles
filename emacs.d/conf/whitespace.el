;; 全半角のスペース可視化
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(face
                           empty
                           space-mark
                           spaces
                           tab-mark
                           tabs
                           trailing))

  (setq whitespace-space-regexp "\\(\u0020+\\|\u3000+\\)")
  (setq whitespace-tab-regexp "\\(\u0009+\\)")

  (setq whitespace-display-mappings
        '((space-mark ?\u0020 [?.])
          (space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\u0009 [?\xBB ?\t])))

  (setq whitespace-action '(auto-cleanup))

  (set-face-attribute 'whitespace-trailing nil
                      :background nil
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background nil
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background nil
                      :foreground "Gray22")
  (set-face-attribute 'whitespace-empty nil
                      :background nil
                      :foreground "GreenYellow"
                      :underline t)

  (global-whitespace-mode t))
