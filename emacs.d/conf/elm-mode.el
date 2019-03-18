;; Elm
(use-package elm-mode
  :ensure t
  :after (company flycheck)
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) elm-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " >> " ">")) elm-mode-map)
  (bind-key "<" (smartchr '(" < " " <- " " << " "<")) elm-mode-map)
  (bind-key "+" (smartchr '(" ++ " " + " "+")) elm-mode-map)
  (bind-key ":" (smartchr '(" : " " :: " ":")) elm-mode-map)
  (bind-key "|" (smartchr '(" | " "|> " " <|" "|")) elm-mode-map)
  (add-to-list 'company-backends 'company-elm)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package flycheck-elm
  :ensure t
  :after elm-mode
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))
