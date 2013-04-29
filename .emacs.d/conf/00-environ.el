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

;; Frame settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)
(when (eq window-system 'w32)
  (setq initial-frame-alist
        (append (list
                 '(width . 196)
                 '(height . 51)
                 '(top . 0)
                 '(left . 0)
                 )
                initial-frame-alist)))

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
