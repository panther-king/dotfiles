;; Haskell
(use-package haskell-mode
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) haskell-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " => " ">")) haskell-mode-map)
  (bind-key "<" (smartchr '(" < " " <- " " <= " "<")) haskell-mode-map)
  (bind-key "+" (smartchr '(" + " " ++ " "+")) haskell-mode-map)
  (bind-key ":" (smartchr '(" :: " ":")) haskell-mode-map)

  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'font-lock-mode)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (hs-minor-mode 1)))

  ;; (custom-set-variables
  ;;  '(haskell-indent-after-keywords (quote (("where" 4 0) ("of" 4) ("do" 4) ("mdo" 4) ("rec" 4) ("in" 4 0) ("{" 4) "if" "then" "else" "let")))
  ;;  '(haskell-indent-offset 4)
  ;;  '(haskell-indent-spaces 4))))
  (custom-set-variables
   '(haskell-indentation-layout-offset 4)
   '(haskell-indentation-starter-offset 4)
   '(haskell-indentation-left-offset 4)
   '(haskell-indentation-ifte-offset 4)
   '(haskell-indentation-where-pre-offset 4)
   '(haskell-indentation-where-post-offset 4)))
