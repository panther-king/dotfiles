;; magit
(use-package magit
  :ensure t
  :init (defun my/magit-quit-session ()
          (interactive)
          (kill-buffer)
          (jump-to-register :magit-fullscreen))
  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-blame-addition)
         :map magit-status-mode-map
         ("q" . my/magit-quit-session))
  :config
  (defadvice git-commit-commit (after move-to-magit-buffer activate)
    (delete-window))
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))
