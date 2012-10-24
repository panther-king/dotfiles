;; Window settings
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq scroll-step 1)
(set-face-foreground 'modeline "blue")
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
  ;(color-theme-dark-laptop)
  (when (require 'color-theme-solarized nil t)
    (color-theme-solarized-dark)))

;; elscreen settings
(load "elscreen")

;; Default font settings
(progn
  (set-default-font "Inconsolata-12")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208 '("Migu-1M-regular" . "unicode-bmp")))

;; Frame settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)

;; Mode line settings
(display-time)
(which-function-mode 1)

;; Show tabs and multi-byte whitespace
(setq whitespace-style
      '(tabl tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\　])
        (tab-mark ?\t [94 ?\t])))
(global-whitespace-mode t)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space 'nil)
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab 'nil)
