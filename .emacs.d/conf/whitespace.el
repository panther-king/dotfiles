;; 全半角のスペース可視化
(el-get-bundle whitespace
  :features whitespace
  (global-whitespace-mode t)
  (setq whitespace-style
        '(face tabs tab-mark spaces space-mark))
  (setq whitespace-space-regexp "\\(\u0020+\\|\u3000+\\)")
  (setq whitespace-tab-regexp "\\(\u0009+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\u0020 [?\uff65])
          (space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\u0009 [?\xBB ?\t])))
  (set-face-foreground 'whitespace-space "#666666")
  (set-face-background 'whitespace-space 'nil)
  (set-face-foreground 'whitespace-tab "#666666")
  (set-face-background 'whitespace-tab 'nil))
