;; Ruby
(use-package ruby-mode
  :config
  (bind-key "'" (smartchr '("'`!!''" "'")) ruby-mode-map)
  (bind-key "=" (smartchr '(" = " " == " "=")) ruby-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) ruby-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) ruby-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) ruby-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > ")) ruby-mode-map)
  (bind-key "<" (smartchr '("<" " <<< " " < ")) ruby-mode-map)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (hs-minor-mode 1)))

  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.jbuilder$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))
