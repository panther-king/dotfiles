;; JavaScript
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (bind-key "=" (smartchr '(" = " " === " " = " "=" " == ")) js2-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) js2-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) js2-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) js2-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) js2-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) js2-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) js2-mode-map)
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook
            '(lambda ()
               (hs-minor-mode 1))))

;; CoffeeScript
(use-package coffee-mode
  :mode (("\\.coffee$" . coffee-mode))
  :config
  (bind-key "=" (smartchr '(" = " " == " " = " "=")) coffee-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) coffee-mode-map)
  (bind-key "+" (smartchr '("+" " += " "++")) coffee-mode-map)
  (bind-key "-" (smartchr '("-" " -= " "--")) coffee-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) coffee-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) coffee-mode-map)
  (bind-key ">" (smartchr '(">" " -> " " > " " >= ")) coffee-mode-map)
  (defun coffee-custom ()
    (and (set (make-local-variable 'tab-width) 2)
         (set (make-local-variable 'coffee-tab-width) 2)))
  (add-hook 'coffee-mode-hook
            '(lambda () (coffee-custom))))
