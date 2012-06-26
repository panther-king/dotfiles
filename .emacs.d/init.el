;; Load path
(setq load-path (append '("~/.emacs.d/apel-10.7"
                          "~/.emacs.d/auto-install"
                          "~/.emacs.d/elisp"
                          "~/.emacs.d/elmo"
                          "~/.emacs.d/emacs-smartchr"
                          "~/.emacs.d/php-mode-1.5.0"
                          "~/.emacs.d/semi"
                          "~/.emacs.d/utils"
                          "~/.emacs.d/wl"
                          "~/.emacs.d/yaml-mode"
                          "~/.emacs.d/yasnippet-0.6.1c"
                          "~/.emacs.d/zencoding"
                          "/usr/share/emacs/site-lisp")
                        load-path))

;; Don't use tab indent
(setq-default indent-tabs-mode nil)

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;; Install elisp
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp")

;; Buffer moving settings
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; Default font settings
;; (add-to-list 'default-frame-alist
;;             '(font . "-unknown-Ricty-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"))
(when window-system
  (progn
    (set-default-font "Inconsolata-12")
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208 '("Migu-1M-regular" . "unicode-bmp")
)))

;; iBus settings
;; (require 'ibus)
;; (add-hook 'after-init-hook 'ibus-mode-on)
;; (global-set-key (kbd "C-SPC") 'ibus-toggle)
;; (setq ibus-cursor-color '("red" "yellow" "limegreen"))
;; (custom-set-variables '(ibus-python-shell-command-name "python2"))

;; smartchr settings
(require 'smartchr)
(global-set-key (kbd "\'") (smartchr '("'`!!''" "'")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
(global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))

;; Auto complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
;; (ac-config-default)

;; Auto indent
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-auto-hungry-state 1)
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

;; Key bind
(define-key global-map "\C-h" 'delete-backward-char)

;; Backup settings
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;; Display settings
(load "elscreen")
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq scroll-step 1)
(set-face-foreground 'modeline "blue")
(column-number-mode t)
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "gray15"))
   (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
;;(global-linum-mode)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)
(show-paren-mode t)
(add-to-list 'global-mode-string '("" default-directory "-"))
(setq initial-frame-alist
       (append (list
                '(width . 146)
                '(height . 44)
                '(top . 0)
                '(left . 0)
                '(alpha . (85 85 85 85))
                )
               initial-frame-alist))

;; Spaces of end of line
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show tabs and multi-byte whitespaces
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
;                (tab-mark ?\t [?\xBB ?\t])
        (tab-mark ?\t [94 ?\t])))
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space 'nil)
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab 'nil)

;; Lines settings
(setq kill-whole-line t)
(setq next-line-add-newlines nil)
(setq require-final-newline t)
(setq fill-column 80)
(setq-default auto-fill-mode t)
;(pc-selection-mode)

;; File settings
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))
(auto-compression-mode t)
(auto-image-file-mode t)
(recentf-mode)

;; Mode line settings
(display-time)
(which-function-mode 1)

;; PHP settings
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear nil)
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'php-completion)
             (php-completion-mode nil)
             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
             (when (require 'auto-complete nil t)
               (make-variable-buffer-local 'ac-sources)
               (add-to-list 'ac-sources 'ac-source-php-completion)
               (auto-complete-mode t))
             (c-set-style "python")
             (c-set-offset 'case-label 4)
             (c-set-offset 'arglist-intro 4)
             (c-set-offset 'arglist-cont-nonempty 4)
             (c-set-offset 'arglist-close 0)
;             (setq indent-tabs-mode nil)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)
             (define-key php-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
             (define-key php-mode-map (kbd ">") (smartchr '(">" "->" " => " " >>> ")))
             (define-key php-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
             (define-key php-mode-map (kbd "(") (smartchr '("(`!!')" "(")))
             (define-key php-mode-map (kbd "[") (smartchr '("[`!!']" "[")))))

;; Python settings
(setq py-indent-offset 4)
(add-to-list 'auto-mode-alist '("\\.\\(py\\|wsgi\\)$" . python-mode))
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq tab-width py-indent-offset)
                      (setq indent-tabs-mode nil)
                      (define-key python-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"" "\"")))
                      (define-key python-mode-map (kbd "\'") (smartchr '("'`!!''" "'''`!!'''" "'")))
                      (define-key python-mode-map (kbd "!") (smartchr '("!" " != ")))
                      (define-key python-mode-map (kbd "(") (smartchr '("(`!!')" "(")))
                      (define-key python-mode-map (kbd "[") (smartchr '("[`!!']" "[")))
                      (define-key python-mode-map (kbd "=") (smartchr '(" = " " == " "="))))))

;; C settings
(defun my-c-mode-hook ()
  (c-set-style "linux")
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; add permit "Execute" to shell script
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

;; JavaScript settings
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'espresso-mode "espresso")
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)
      (save-excursion
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))
      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))
(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda (oval)
                                   (delete-overlay ovl)) ovl)))))
(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda ()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (define-key js2-mode-map (kbd "=") (smartchr '(" = " " == " " === " "=")))
  (define-key js2-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (define-key js2-mode-map (kbd "'") (smartchr '("'`!!''" "'")))
  (define-key js2-mode-map (kbd "(") (smartchr '("(`!!')" "(")))
  (define-key js2-mode-map (kbd "[") (smartchr '("[`!!']" "[")))
  (define-key js2-mode-map (kbd "{") (smartchr '("{`!!'}" "{")))
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; AutoInstall settings
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; anything settings
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(setq anything-sources
      '(anything-c-source-buffers+
        anything-c-source-file-name-history
        anything-c-source-buffer-not-found
        anything-c-source-imenu
))
(setq imenu-auto-rescan t)
(global-set-key (kbd "C-;") 'anything)

;; Redo settings
; (require 'redo+)
; (global-set-key (kbd "C-'") 'redo)

;; grep-edit settings
(require 'grep-edit)

;; undo-tree settings
(require 'undo-tree)
(global-undo-tree-mode)

;; wdired settings
(require 'wdired)
(define-key dired-mode-map "r"
  'wdired-change-to-wdired-mode)

;; Egg settings
;; (executable-find "git")
;; (require 'egg)

;; Shell settings
(require 'multi-term)
(setq multi-term-program "/usr/bin/zsh")

;; view-mode settings
(setq view-read-only t)
(require 'view)
(define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
(define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
(define-key view-mode-map (kbd "G") 'View-goto-line-last)
(define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
(require 'viewer)
(viewer-stay-in-setup)

;; open-junk-file settings
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/.junk/%Y-%m-%d-%H_%M_%S.")

;; Summary settings
(require 'summarye)

;; Don't create ediff control panel
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Code snippets settings
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; yaml-mode settings
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; nxhtml-mode settings
(load "~/.emacs.d/nxhtml/autostart.el")
(custom-set-faces
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "dark1"))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "dark1"))))
)

;; lua mode settings
(autoload 'lua-mode "lua-mode" "Lua editting mode." t)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

;; HTML settings
;; (autoload 'html-mode "html-mode" "Yay HTML" t)
;; (setq auto-mode-alist (append '(("\\.html$" . html-mode)
;;                                 ("\\.twig$" . html-mode)
;;                                 ) auto-mode-alist))

;; mmm-mode settings
;; (require 'mmm-mode)
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (set-face-background 'mmm-default-submode-face "#000000")
;; (mmm-add-mode-ext-class nil "\\.php$" 'html-php)
;; (mmm-add-classes
;;  '((html-php
;;     :submode php-mode
;;     :front "<\\?php"
;;     :back "\(\\?>\)?")))
;; (add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

;; Git settings
(autoload 'magit-status "magit" nil t)

;; reStructuredText settings
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(setq frame-background-mode 'dark)
(add-hook 'rst-mode-hook '(lambda()
                            (setq indent-tabs-mode nil
                                  setq tab-width 3)))
