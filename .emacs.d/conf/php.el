;; PHP
(use-package php-mode
  :mode (("\\.php$" . php-mode))
  :config
  (bind-key "=" (smartchr '(" = " " === " "=" " == ")) php-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) php-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) php-mode-map)
  (bind-key ">" (smartchr '(">" "->" " => " " >>> ")) php-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) php-mode-map)
  (bind-key "(" (smartchr '("(`!!')" "(")) php-mode-map)
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style))
