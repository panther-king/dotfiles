;; TypeScript
(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode))
  :config
  (bind-key "=" (smartchr '(" = " " === " "=" " == ")) typescript-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) typescript-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) typescript-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) typescript-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) typescript-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) typescript-mode-map))
