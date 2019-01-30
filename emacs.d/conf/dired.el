;; dired強化
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :requires all-the-icons
  :bind (:map dired-mode-map
              ("C-m" . 'dired-open-in-accordance-with-situation)
              ("a" . dired-find-file))
  :hook (dired-mode-hook)
  :init (defun dired-open-in-accordance-with-situation ()
          ;; ファイルなら別バッファ・ディレクトリなら同一バッファ
          (interactive)
          (cond ((string-match "\\.\\.?$"
                               (format "%s" (thing-at-point 'filename)))
                 (dired-find-alternate-file))
                ((file-directory-p (dired-get-filename))
                 (dired-find-alternate-file))
                (t
                 (dired-find-file))))
  :config
  (put 'dired-find-alternate-file 'disabled nil))
