;; JavaScript
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (bind-key "=" (smartchr '(" = " " === " " = " " == ")) js2-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) js2-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) js2-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) js2-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) js2-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) js2-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) js2-mode-map)
  (setq js2-basic-offset 2))
