;; dired強化
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (defun dired-open-in-accordance-with-situation ()
    ;; ファイルなら別バッファ・ディレクトリなら同一バッファ
    (interactive)
    (cond ((string-match "\\.\\.?$"
                         (format "%s" (thing-at-point 'filename)))
           (dired-find-alternate-file))
          ((file-directory-p (dired-get-filename))
           (dired-find-alternate-file))
          (t
           (dired-find-file))))
  (define-key dired-mode-map (kbd "C-m") 'dired-open-in-accordance-with-situation)
  (define-key dired-mode-map (kbd "a") 'dired-find-file)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
