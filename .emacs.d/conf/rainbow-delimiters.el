;; 対応するカッコの色付け
(el-get-bundle rainbow-delimiters
  :features rainbow-delimiters
  (with-eval-after-load-feature 'rainbow-delimiters
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (require 'cl-lib)
    (require 'color)
    (defun rainbow-delimiters-using-stronger-colors ()
      (interactive)
      (cl-loop
       for index from 1 to rainbow-delimiters-max-face-count
       do
       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
         (cl-callf color-saturate-name (face-foreground face) 30))))
    (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)))
