;; Ruby
(use-package ruby-mode
  :ensure t
  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.jbuilder$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode))
  :config
  (bind-key "'" (smartchr '("'`!!''" "'")) ruby-mode-map)
  (bind-key "=" (smartchr '(" = " " == " "=")) ruby-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) ruby-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) ruby-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) ruby-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > ")) ruby-mode-map)
  (bind-key "<" (smartchr '("<" " <<< " " < ")) ruby-mode-map))
