;; helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-;" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c i" . helm-imenu)
         ("C-x b" . helm-buffers-list)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action)
         :map helm-find-files-map
         ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1)
  (helm-migemo-mode 1)
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exists activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it)))
