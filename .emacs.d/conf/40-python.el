;; Python settings
;(add-to-list 'auto-mode-alist '("\\.\\(py\\|wsgi\\)$" . python-mode))
(defun my-python-mode-hook ()
  (setq py-indent-offset 4)
  (setq tab-width py-indent-offset)
  (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
  (define-key python-mode-map (kbd "\'") (smartchr '("'`!!''" "'''`!!''''")))
  (define-key python-mode-map (kbd "=") (smartchr '(" = " " == " "="))))
(add-hook 'python-mode-hook 'my-python-mode-hook)
