;; auto-install settings
(when (require 'auto-install)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; grep-edit settings
(require 'grep-edit)

;; undo-tree settings
(when (require 'undo-tree)
  (global-undo-tree-mode))

;; wdired settings
(when (require 'wdired)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

;; multi-term settings
(when (require 'multi-term)
  (setq multi-term-program "/usr/bin/zsh"))

;; open-junk-file settings
(when (require 'open-junk-file)
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))

;; yasnippet settings
(when (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets"))

;; yaml-mode settings
(when (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; rst settings
(when (require 'rst)
  (setq auto-mode-alist
        (append '(("\\.rst$" . rst-mode)
                  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (setq frame-background-mode 'dark)
  (add-hook 'rst-mode-hook '(lambda ()
                              (setq tab-width 3))))
