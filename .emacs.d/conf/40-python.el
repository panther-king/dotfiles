;; Python settings
(when (require 'python-mode nil t)
  (defun my-python-mode-hook ()
    (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
    (setq interpreter-mode-alist (cons '("python" . python-mode)
                                       interpreter-mode-alist))
    (autoload 'python-mode "python-mode" "Python editing mode." t)
    (setq py-indent-offset 4)
    (setq tab-width py-indent-offset)
    (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
    (define-key python-mode-map (kbd "\'") (smartchr '("'`!!''" "'" "'''`!!''''")))
    (define-key python-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
    (define-key python-mode-map (kbd "!") (smartchr '("!" " != ")))
    (define-key python-mode-map (kbd "-") (smartchr '("-" " -= ")))
    (define-key python-mode-map (kbd "+") (smartchr '("+" " += "))))
  (add-hook 'python-mode-hook 'my-python-mode-hook))

;; flymake for Python
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pychecker" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-next-error)
(global-set-key [f11] 'flymake-goto-prev-error)
