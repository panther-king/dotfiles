;; JavaScript settings
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (defun my-js2-mode-hook ()
    (setq js2-basic-offset 2)
    (define-key js2-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
    (define-key js2-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
    (define-key js2-mode-map (kbd "+") (smartchr '("+" "++" " += ")))
    (define-key js2-mode-map (kbd "-") (smartchr '("-" "--" " -= ")))
    (define-key js2-mode-map (kbd "<") (smartchr '("<" " < " " <= ")))
    (define-key js2-mode-map (kbd ">") (smartchr '(">" " > " " >= "))))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

;; CoffeeScript settings
(autoload 'coffee-mode "coffee-mode"
  "Major mode for editing CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(defun my-coffee-mode-hook ()
  (setq coffee-tab-width 2)
  (define-key coffee-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
  (define-key coffee-mode-map (kbd "!") (smartchr '("!" " != ")))
  (define-key coffee-mode-map (kbd ">") (smartchr '(" -> " " > " " => " ">")))
  (define-key coffee-mode-map (kbd "{") (smartchr '("{`!!'}" "{"))))
(add-hook 'coffee-mode-hook 'my-coffee-mode-hook)

;; TypeScript settings
(when (require 'typescript-mode nil t)
  (defun my-typescript-mode-hook ()
    (define-key typescript-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
    (define-key typescript-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
    (define-key typescript-mode-map (kbd "+") (smartchr '("+" "++" " += ")))
    (define-key typescript-mode-map (kbd "-") (smartchr '("-" "--" " -= ")))
    (define-key typescript-mode-map (kbd "<") (smartchr '("<" " < " " <= ")))
    (define-key typescript-mode-map (kbd ">") (smartchr '(">" " => " " > " " >= "))))
  (add-hook 'typescript-mode-hook 'my-typescript-mode-hook))
