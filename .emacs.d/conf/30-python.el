;; Python settings
(setq py-indent-offset 4)
(add-to-list 'auto-mode-alist '("\\.\\(py\\|wsgi\\)$" . python-mode))
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq tab-width py-indent-offset)
                      (setq indent-tabs-mode nil)
                      (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
                      (define-key python-mode-map (kbd "\'") (smartchr '("'`!!'" "'''`!!''''")))
                      (define-key python-mode-map (kbd "=") (smartchr '(" = " " == "))))))

