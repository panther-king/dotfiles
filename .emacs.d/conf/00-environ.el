;; Backup settings
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; Don't use tab indent
(setq-default indent-tabs-mode nil)

;; Key binds settings
(define-key global-map (kbd "C-h") 'delete-backward-char)

;; smartchr settings
(require 'smartchr)
(global-set-key (kbd "\'") (smartchr '("'`!!''" "'")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
(global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))

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
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-auto-hungry-state 1)
             (define-key c-mode-base-map (kbd "C-m") 'newline-and-indent)))

;; Add permittion "Execute" to shell script
(add-hook 'after-save-hook
          '(lambda ()
             (save-restriction
               (widen)
               (if (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let ((name (buffer-file-name)))
                     (or (char-equal ?. (string-to-char (file-name-nondirectory name)))
                         (let ((mode (file-modes name)))
                           (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                           (message (concat "Wrote " name " (+x)"))))
                     )))))
