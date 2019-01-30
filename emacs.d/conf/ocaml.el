;; OCaml
(use-package tuareg
  :ensure t
  :mode (("\\.ml[iylp]?$" . tuareg-mode))
  :init (defun my-tuareg-mode-hook ()
          (electric-indent-mode 0))
  :hook (my-tuareg-mode-hook)
  :config
  (autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
  (autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger." t)
  (bind-key "=" (smartchr '(" = " "=")) tuareg-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " >= " ">")) tuareg-mode-map)
  (bind-key "<" (smartchr '(" < " " <- " " <= " "<")) tuareg-mode-map)
  (bind-key "|" (smartchr '("| " " | " "|")) tuareg-mode-map)
  (bind-key "^" (smartchr '(" ^ " "^")) tuareg-mode-map))
