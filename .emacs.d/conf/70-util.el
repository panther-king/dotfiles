;; Change answer from yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

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
  (setq multi-term-program "/usr/bin/zsh"))

;; yasnippet settings
(when (require 'yasnippet nil t)
  (yas-global-mode 1))

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

;; zencoding settings
(when (require 'zencoding-mode)
  ; @see http://fukuyama.co/zencoding
  (setq zencoding-block-tags
        (append (list
                 "article"
                 "section"
                 "aside"
                 "nav"
                 "figure"
                 "address"
                 "header"
                 "footer")
                zencoding-block-tags))
  (setq zencoding-inline-tags
        (append (list
                 "textarea"
                 "small"
                 "time" "del" "ins"
                 "sub"
                 "sup"
                 "i" "s" "b"
                 "ruby" "rt" "rp"
                 "bdo"
                 "iframe" "canvas"
                 "audio" "video"
                 "object" "embed"
                 "map")
                zencoding-inline-tags))
  (add-hook 'html-mode-hook 'zencoding-mode)
  (add-hook 'php-mode-hook 'zencoding-mode)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'nxhtml-mode-hook 'zencoding-mode)
  (add-hook 'nxml-mode-hook 'zencoding-mode))

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

;; sql-mode settings
(defun my-sql-mode-hook ()
  (load-library "sql-indent")
  (setq sql-indent-offset 4)
  (setq sql-indent-maybe-tab nil))
(add-hook 'sql-mode-hook 'my-sql-mode-hook)

;; regexp settings
(when (require 'foreign-regexp nil t)
  (custom-set-variables
   '(foreign-regexp/regexp-type 'ruby)
   '(reb-re-syntax 'foreign-regexp)))

;; JSON settings
(require 'json-mode)

;; git settings
(require 'magit)

;;
;; Install from other site
;;

;; undohist settings
;; install-elisp http://cx4a.org/pub/undohist.el
(when (require 'undohist nil t)
  (undohist-initialize))
