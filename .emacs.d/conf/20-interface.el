;; Window settings
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq scroll-step 1)
(set-face-foreground 'mode-line "blue")
(global-linum-mode t)
(column-number-mode t)
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; paren-mode settings
(setq show-paren-delay 0)
(show-paren-mode t)
(defface hlline-face
  '((((class color) (background dark))
     (:background "gray15" t))
    (((class color) (background light))
     (:background "ForestGreen" t))
    (t
     ()))
  "*Face used by hl-line.")
(global-hl-line-mode t)

;; color-theme settings
(load-theme 'obsidian t)
;;(load-theme 'smyx t)

;; elscreen settings
(when (require 'elscreen nil t)
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil))

;; Default font settings
(progn
  (set-default-font "RictyDiminishedDiscord-18")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208 '("RictyDiminishedDiscord" . "unicode-bmp")))

;; Frame settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)

;; Mode line settings
(when (require 'powerline nil t)
  (powerline-default-theme))

;; Show tabs and multi-byte whitespace
(when (require 'whitespace nil t)
  (global-whitespace-mode t)
  (setq whitespace-style
        '(face tabs tab-mark spaces space-mark))
  (setq whitespace-space-regexp "\\(\u0020+\\|\u3000+\\)")
  (setq whitespace-tab-regexp "\\(\u0009+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\u0020 [?\uff65])
          (space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\u0009 [?\xBB ?\t])))
  (set-face-foreground 'whitespace-space "#666666")
  (set-face-background 'whitespace-space 'nil)
  (set-face-foreground 'whitespace-tab "#666666")
  (set-face-background 'whitespace-tab 'nil))

;; Echo area settings
(defun emacs-eldoc-hook ()
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'emacs-eldoc-hook)

(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; Directory tree settings
(when (require 'direx nil t)
  (global-set-key (kbd "C-x C-k") 'direx:jump-to-directory))

;; popwin settings
(when (require 'popwin)
  (setq special-display-function 'popwin:display-buffer))

;; rainbow-mode settings
(when (require 'rainbow-mode nil t)
  (rainbow-mode t))

;; neotree settings
(when (require 'neotree)
  (global-set-key [f8] 'neotree-toggle))
