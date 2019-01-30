;; Python
(use-package python-mode
  :ensure t
  :config
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")) python-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'" "'''`!!''''")) python-mode-map)
  (bind-key "=" (smartchr '(" = " " == " "=")) python-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) python-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) python-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) python-mode-map)
  (bind-key ">" (smartchr '(">" " -> " " > " " >= ")) python-mode-map)
  (bind-key "<" (smartchr '("<" " <= ")) python-mode-map)
  (setq py-indent-offset 4)
  (setq tab-width py-indent-offset))

;; コード補完
(use-package jedi-core
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook
            '(lambda ()
               (hs-minor-mode 1)))
  (add-to-list 'company-backends 'company-jedi))
