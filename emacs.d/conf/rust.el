;; Rust
(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook '(lambda ()
                               (hs-minor-mode 1)))
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) rust-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) rust-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) rust-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) rust-mode-map)
  (bind-key ">" (smartchr '(">" " > " " -> " " => " " >= ")) rust-mode-map)
  (bind-key "<" (smartchr '("<`!!'>" "<" " < " " <- " " <= ")) rust-mode-map)
  (bind-key "|" (smartchr '("|`!!'|" "||" " | " "|")) rust-mode-map)
  (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
