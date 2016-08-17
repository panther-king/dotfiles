;; ELPA settings
(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; PATH settings
(exec-path-from-shell-initialize)

;; Backup settings
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; Frame settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)

;; Add permission "Execute" to shell script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Reload buffer settings
(global-auto-revert-mode 1)

;; Ignore warning sound
(setq ring-bell-function 'ignore)
