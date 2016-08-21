;; git
(el-get-bundle magit
  :features magit
  (with-eval-after-load-feature 'magit
    (global-set-key (kbd "C-M-g") 'magit-status)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defun my/magit-quit-session ()
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)
    (defadvice git-commit-commit (after move-to-magit-buffer activate)
      (delete-window))))

(el-get-bundle magit-gitflow
  :features magit-gitflow
  (with-eval-after-load-feature 'magit-gitflow
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))
