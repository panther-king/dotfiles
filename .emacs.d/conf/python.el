;; Python
(el-get-bundle python-mode
  :features python-mode
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  (setq py-indent-offset 4)
  (setq tab-width py-indent-offset)
  (with-eval-after-load-feature 'python-mode
    (defun my-python-mode-hook ()
      (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
      (define-key python-mode-map (kbd "\'") (smartchr '("'`!!''" "'" "'''`!!''''")))
      (define-key python-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
      (define-key python-mode-map (kbd "!") (smartchr '("!" " != ")))
      (define-key python-mode-map (kbd "-") (smartchr '("-" " -= ")))
      (define-key python-mode-map (kbd "+") (smartchr '("+" " += "))))
    (add-hook 'python-mode-hook 'my-python-mode-hook)))
