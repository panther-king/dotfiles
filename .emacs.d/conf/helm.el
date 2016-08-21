;; helm
(el-get-bundle helm
  :features helm-config
  (with-eval-after-load-feature 'helm-config
    (helm-mode 1)
    (helm-migemo-mode 1)

    (define-key global-map (kbd "M-x") 'helm-M-x)
    (define-key global-map (kbd "C-;") 'helm-mini)
    (define-key global-map (kbd "C-x C-f") 'helm-find-files)
    (define-key global-map (kbd "C-x C-r") 'helm-recentf)
    (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key global-map (kbd "C-c i") 'helm-imenu)
    (define-key global-map (kbd "C-x b") 'helm-buffers-list)

    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

    (setq helm-delete-minibuffer-contents-from-point t)
    (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
      "Emulate `kill-line' in helm minibuffer"
      (kill-new (buffer-substring (point) (field-end))))

    (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exists activate)
      "Execute command only if CANDIDATE exists"
      (when (file-exists-p candidate)
        ad-do-it))))

(el-get-bundle helm-ag
  :features helm-ag)
(el-get-bundle helm-core
  :features helm-core)
