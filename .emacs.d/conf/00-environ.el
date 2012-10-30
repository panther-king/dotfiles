;; ELPA settings
(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;; Backup settings
(setq backup-inhibited t)
(setq delete-auto-save-files t)


;; File name settings
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; Add permittion "Execute" to shell script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
