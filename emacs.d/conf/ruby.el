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

  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.jbuilder$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))
