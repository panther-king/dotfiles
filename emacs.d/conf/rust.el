;; Rust
(use-package rust-mode
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) rust-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) rust-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) rust-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) rust-mode-map)
  (bind-key ">" (smartchr '(">" " > " " -> " " => " " >= ")) rust-mode-map)
  (bind-key "<" (smartchr '("<`!!'>" "<" " < " " <- " " <= ")) rust-mode-map)
  (bind-key "|" (smartchr '("|`!!'|" "||" " | " "|")) rust-mode-map)
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save)
  (add-hook 'rust-mode-hook 'racer-mode))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'company-mode))
