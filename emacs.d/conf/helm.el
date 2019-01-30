;; helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-;" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c i" . helm-imenu)
         ("C-x b" . helm-buffers-list))
  :config
  (helm-mode 1)
  (helm-migemo-mode 1)
  (bind-key "C-h" 'delete-backward-char helm-map)
  (bind-key "TAB" 'helm-execute-persistent-action helm-read-file-map)
  (bind-key "C-h" 'delete-backward-char helm-find-files-map)
  (bind-key "TAB" 'helm-execute-persistent-action helm-find-files-map)

  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exists activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it)))
