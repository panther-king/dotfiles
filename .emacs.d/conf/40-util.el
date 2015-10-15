;; Change answer from yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Completion settings
(icomplete-mode 1)
(global-set-key (kbd "C-j") 'dabbrev-expand)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;
;; Install from ELPA
;; list-packages
;;

;; auto-install settings
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; open-junk-file settings
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/.emacs.d/.junk/%Y%m%d%H%M%S."))

;; multi-term settings
(when (require 'multi-term nil t)
  (setq multi-term-program "/usr/bin/zsh"))

;; yaml-mode settings
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(yml\\|raml\\)$" . yaml-mode)))

;; undo-tree settings
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+ settings
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-'") 'redo))

;; markdown settings
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;; wdired settings
(when (require 'wdired nil t)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

;; rst settings
(when (require 'rst nil t)
  (setq auto-mode-alist
        (append '(("\\.rst$" . rst-mode)
                  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (setq frame-background-mode 'dark))

;; ctags settings
(when (require 'ctags nil t)
  (setq tags-revert-without-query t)
  (setq ctags-command "ctags -e -R ")
  (global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table))

;; regexp settings
;; M-s M-s incremental search
;; M-s M-r decremental search
;; M-s M-% replacement
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp)))

;; JSON settings
(when (require 'json-mode nil t)
  (add-to-list 'auto-mode-alist '("//.json$" . json-mode))
  (setq js-indent-level 2))

;; git settings
(require 'magit)

;; twitter settings
(when (require 'twittering-mode nil t)
  (setq twittering-use-master-password t))

;; undohist settings
(when (require 'undohist nil t)
  (undohist-initialize))
