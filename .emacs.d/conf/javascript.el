;; JavaScript
(el-get-bundle js2-mode
  :features js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-basic-offset 2)
  (with-eval-after-load-feature 'js2-mode
    (defun my-js2-mode-hook ()
      (define-key js2-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
      (define-key js2-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
      (define-key js2-mode-map (kbd "+") (smartchr '("+" "++" " += ")))
      (define-key js2-mode-map (kbd "-") (smartchr '("-" "--" " -= ")))
      (define-key js2-mode-map (kbd "<") (smartchr '("<" " < " " <= ")))
      (define-key js2-mode-map (kbd ">") (smartchr '(">" " => " " > " " >= "))))
    (add-hook 'js2-mode-hook 'my-js2-mode-hook)))

(el-get-bundle typescript-mode
  :features typescript-mode
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (with-eval-after-load-feature 'typescript-mode
    (defun my-typescript-mode-hook ()
      (define-key typescript-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
      (define-key typescript-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
      (define-key typescript-mode-map (kbd "+") (smartchr '("+" "++" " += ")))
      (define-key typescript-mode-map (kbd "-") (smartchr '("-" "--" " -= ")))
      (define-key typescript-mode-map (kbd "<") (smartchr '("<" " < " " <= ")))
      (define-key typescript-mode-map (kbd ">") (smartchr '(">" " => " " > " " >= "))))
    (add-hook 'typescript-mode-hook 'my-typescript-mode-hook)))
