;;
;; Install from emacswiki
;; http://www.emacswiki.org/
;;

;; install-elisp-from-emacswiki auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; install-elisp-from-emacs-wiki open-junk-file.el
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))

;;
;; Install from ELPA
;; list-packages
;;

;; multi-term settings
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/zsh"))

;; yasnippet settings
(when (require 'yasnippet nil t)
  (yas-global-mode 1))

;; open-junk-file settings
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))

;; yaml-mode settings
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;; undo-tree settings
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+ settings
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-'") 'redo))

;; markdown settings
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;;
;; Install from other site
;;

 ;; undohist settings
;; install-elisp http://cx4a.org/pub/undohist.el
(when (require 'undohist nil t)
  (undohist-initialize))

;; wdired settings
(when (require 'wdired nil t)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

;; rst settings
(when (require 'rst nil t)
  (setq auto-mode-alist
        (append '(("\\.rst$" . rst-mode)
                  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (setq frame-background-mode 'dark))

;; egg settings
;; git clone git://github.com/byplayer/egg.git
(when (executable-find "git")
  (require 'egg nil t))

;; ctags settings
(when (require 'ctags nil t)
  (setq tags-revert-without-query t)
  (setq ctags-command "ctags -e -R ")
;(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
  (global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table))

;; sql-mode settings
(defun my-sql-mode-hook ()
  (load-library "sql-indent")
  (setq sql-indent-offset 4)
  (setq sql-indent-maybe-tab nil))
(add-hook 'sql-mode-hook 'my-sql-mode-hook)
