;; モードラインのリッチ化
(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode))

(use-package doom-modeline
  :ensure t
  :after nyan-mode
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-mode nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (doom-modeline-def-modeline 'main
    '(bar buffer-info matches buffer-position selection-info)
    '(minor-modes input-method major-mode vcs checker bar)))
