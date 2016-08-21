;; Rust
(el-get-bundle rust-mode
  :depends company
  :features rust-mode
  (with-eval-after-load-feature 'rust-mode
    (define-key rust-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
    (define-key rust-mode-map (kbd "+") (smartchr '(" + " "+" " += ")))
    (define-key rust-mode-map (kbd "-") (smartchr '(" - " "-" " -= ")))
    (define-key rust-mode-map (kbd "!") (smartchr '("!" " != ")))
    (define-key rust-mode-map (kbd ">") (smartchr '(">" " > " " -> " " => " " >= ")))
    (define-key rust-mode-map (kbd "<") (smartchr '("<`!!'>" "<" " < " " <- " " <= ")))
    (define-key rust-mode-map (kbd "|") (smartchr '("|`!!'|" "||" " | " "|")))
    (add-hook 'rust-mode-hook 'rust-enable-format-on-save))
  (with-eval-after-load-feature 'company
    (setq company-tooltip-align-annotations t)
    (local-set-key (kbd "C-i") #'company-indent-or-complete-common)))

(el-get-bundle cargo
  :features cargo
  (with-eval-after-load-feature 'cargo
    (add-hook 'rust-mode-hook #'cargo-minor-mode)))

(el-get-bundle racer
  :features racer
  (with-eval-after-load-feature 'racer-mode
    (setq racer-rust-src-path "~/.racer-src/rustc-1.10.0/src/")
    (add-hook 'rust-mode-hook #'racer-mode))
  (with-eval-after-load-feature 'company
    (add-hook 'racer-mode-hook #'company-mode)))
