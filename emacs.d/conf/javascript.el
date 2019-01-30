;; JavaScript
(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :config
  (bind-key "=" (smartchr '(" = " " === " " = " "=" " == ")) js2-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) js2-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) js2-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) js2-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) js2-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) js2-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) js2-mode-map)
  (setq js2-basic-offset 2))

;; CoffeeScript
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee$"
  :init (defun coffee-custom ()
          (and (set (make-local-variable 'tab-width) 2)
               (set (make-local-variable 'coffee-tab-width) 2)))
  :hook (coffee-custom)
  :config
  (bind-key "=" (smartchr '(" = " " == " " = " "=")) coffee-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) coffee-mode-map)
  (bind-key "+" (smartchr '("+" " += " "++")) coffee-mode-map)
  (bind-key "-" (smartchr '("-" " -= " "--")) coffee-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) coffee-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) coffee-mode-map)
  (bind-key ">" (smartchr '(">" " -> " " > " " >= ")) coffee-mode-map))
