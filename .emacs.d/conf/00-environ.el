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

;; Don't use tab indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Key binds settings
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "C-t") 'other-window)

;; smartchr settings
(when (require 'smartchr nil t)
  (require 'smartchr)
  (global-set-key (kbd "\'") (smartchr '("'`!!''" "'")))
  (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (global-set-key (kbd "{") (smartchr '("{`!!'}" "{"))))

;; File name settings
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; Remove spaces at end of line
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Lines settings
(setq kill-whole-line t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq fill-column 80)
(setq-default auto-fill-mode t)

;; Indent settings
(defun c-mode-indent-hook ()
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map (kbd "C-m") 'newline-and-indent))
(add-hook 'c-mode-common-hook 'c-mode-indent-hook)

;; Add permittion "Execute" to shell script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
