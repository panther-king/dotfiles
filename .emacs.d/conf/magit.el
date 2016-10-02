;; magit
;; 依存ライブラリを先にロード
(load-library "dash")
(require 'with-editor)
(when (require 'magit nil t)
  ;; 依存ライブラリを先にロード
;  (load-library "dash")
;  (require 'with-editor)
  (global-set-key (kbd "C-M-g") 'magit-status)
  (add-hook 'git-commit-mode-hook (setq auto-fill-mode nil))
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
    (delete-window)))
