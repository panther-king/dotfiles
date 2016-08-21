;; Haskell
(el-get-bundle haskell-mode
  :features haskell-mode
  (with-eval-after-load-feature 'haskell-mode
    (defun my-haskell-mode-hook ()
      (define-key haskell-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
      (define-key haskell-mode-map (kbd ">") (smartchr '(" > " " -> " " => " ">")))
      (define-key haskell-mode-map (kbd "<") (smartchr '(" < " " <- " " <= " "<")))
      (define-key haskell-mode-map (kbd "+") (smartchr '(" + " " ++ " "+")))
      (define-key haskell-mode-map (kbd ":") (smartchr '(" :: " ":"))))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook #'turn-on-haskell-doc-mode)))
