;; magit
(use-package magit
  :ensure t
  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-blame-addition))
  :config
  (defadvice git-commit-commit (after move-to-magit-buffer activate)
    (delete-window))
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (bind-key "q" 'my/magit-quit-session magit-status-mode-map))
