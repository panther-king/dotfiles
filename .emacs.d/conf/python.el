;; Python
(use-package python-mode
  :mode (("\\.py$" . python-mode))
  :config
  (bind-key "\"" (smartchr '("\"`!!'\"")) python-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'" "'''`!!''''")) python-mode-map)
  (bind-key "=" (smartchr '(" = " " == " "=")) python-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) python-mode-map)
  (bind-key "-" (smartchr '("-" " -= ")) python-mode-map)
  (bind-key "+" (smartchr '("+" " += ")) python-mode-map)
  (setq py-indent-offset 4)
  (setq tab-width py-indent-offset))
