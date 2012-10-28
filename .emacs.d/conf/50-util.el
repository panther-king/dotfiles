;; auto-install settings
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; grep-edit settings
(require 'grep-edit)

;; undohist settings
;; http://cx4a.org/pub/undohist.el
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-tree settings
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+ settings
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-'") 'redo))

;; wdired settings
(when (require 'wdired nil t)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

;; multi-term settings
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/zsh"))

;; open-junk-file settings
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))

;; yasnippet settings
(when (require 'yasnippet nil t)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elisp/yasnippet-0.6.1c/snippets"))

;; yaml-mode settings
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; rst settings
(when (require 'rst nil t)
  (setq auto-mode-alist
        (append '(("\\.rst$" . rst-mode)
                  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (setq frame-background-mode 'dark))

;; git settings
(when (executable-find "git")
  (require 'egg nil t))

;; ctags settings
(require 'ctags nil t)
(setq tags-revert-without-query t)
(setq ctags-command "ctags -e -R ")
;(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
