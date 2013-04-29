;; ELPA settings
(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  (require 'melpa))

;; Backup settings
(setq backup-inhibited t)
(setq delete-auto-save-files t)


;; File name settings
(when (eq window-system 'w32)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; Add permittion "Execute" to shell script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
