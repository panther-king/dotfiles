;; Python settings
(defun my-python-mode-hook ()
  (setq py-indent-offset 4)
  (setq tab-width py-indent-offset)
  (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
  (define-key python-mode-map (kbd "\'") (smartchr '("'`!!''" "'" "'''`!!''''")))
  (define-key python-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
  (add-to-list 'auto-mode-alist '("\\.pt$" . html-mode)))
(add-hook 'python-mode-hook 'my-python-mode-hook)
