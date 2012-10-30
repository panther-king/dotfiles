;; Key bind settings
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "C-t") 'other-window)

;; Don't use tab indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; smartchr settings
(when (require 'smartchr nil t)
  (require 'smartchr)
  (global-set-key (kbd "\'") (smartchr '("'`!!''" "'")))
  (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (global-set-key (kbd "{") (smartchr '("{`!!'}" "{"))))

;; Lines settings
(setq kill-whole-line t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq fill-column 80)
(setq-default auto-fill-mode t)

;; Remove spaces at end of line
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Indent settings
(defun my-c-mode-common-hook ()
  (c-toggle-auto-hungry-state 1)
  (define-key c-mode-base-map (kbd "C-m") 'newline-and-indent))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; C-mode settings
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'my-c-mode-hook)
