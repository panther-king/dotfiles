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
  (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  (eval-after-load "rust-mode"
    '(setq-default rust-format-on-save t))
  (add-hook 'rust-mode-hook 'rust-enable-format-on-save)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook
            '(lambda ()
               (hs-minor-mode 1))))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'company-mode))
