;; TypeScript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts$"
  :config
  (bind-key "=" (smartchr '(" = " " === " "=" " == ")) typescript-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) typescript-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) typescript-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) typescript-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) typescript-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) typescript-mode-map)
  (add-hook 'typescript-mode-hook
            '(lambda ()
               (hs-minor-mode 1))))
