;; OCaml
(use-package tuareg
  :ensure t
  :mode (("\\.ml[iylp]?$" . tuareg-mode))
  :config
  (autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
  (autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger." t)
  (bind-key "=" (smartchr '(" = " "=")) tuareg-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " >= " ">")) tuareg-mode-map)
  (bind-key "<" (smartchr '(" < " " <- " " <= " "<")) tuareg-mode-map)
  (bind-key "|" (smartchr '("| " " | " "|")) tuareg-mode-map)
  (bind-key "^" (smartchr '(" ^ " "^")) tuareg-mode-map)
  :init
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (electric-indent-mode 0))))
