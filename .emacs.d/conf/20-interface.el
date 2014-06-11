;; Window settings
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq scroll-step 1)
(set-face-foreground 'mode-line "blue")
(global-linum-mode t)
(column-number-mode t)

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
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-deep-blue))

;; elscreen settings
(load "elscreen")

;; Default font settings
(when (eq window-system 'w32)
  (progn
    (set-default-font "Ricty-12")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208 '("Ricty" . "unicode-bmp"))))
(when (eq window-system 'x)
  (progn
    (set-default-font "Inconsolata-12")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208 '("Migu-1M-regular" . "unicode-bmp"))))

;; Frame settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)

;; Mode line settings
(display-time)
(which-function-mode 1)

;; Show tabs and multi-byte whitespace
(when (require 'whitespace nil t)
  (global-whitespace-mode t)
  (setq whitespace-style
        '(face tabs tab-mark spaces space-mark))
  (set-face-foreground 'whitespace-space "#666666")
  (set-face-background 'whitespace-space 'nil)
  (set-face-foreground 'whitespace-tab "#666666")
  (set-face-background 'whitespace-tab 'nil))

(when (require 'jaspace nil t)
  (setq jaspace-alternate-jaspace-string "□")
  (setq jaspace-highlight-tabs t))

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
;; git clone https://github.com/m2ym/direx-el.git
(when (require 'direx nil t)
  (global-set-key (kbd "C-x C-k") 'direx:jump-to-directory))

;; popwin settings
;; git clone git://github.com/m2ym/popwin-el.git
(when (require 'popwin)
  (setq special-display-function 'popwin:display-buffer))
