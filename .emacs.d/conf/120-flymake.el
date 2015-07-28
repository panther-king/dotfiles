;; flymake settings
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  ;; for Python
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/bin/flake8" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  ;; for HTML
  (defun flymake-html-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "tidy" (list "-utf8" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.html$" flymake-html-init))
  (add-to-list 'flymake-err-line-patterns
               '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
                 nil 1 2 4)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-next-error)
(global-set-key [f11] 'flymake-goto-prev-error)
