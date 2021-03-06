;;
;; Base settings
;;

(setq byte-compile-warnings '(not cl-functions obsolete))

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Do not modify init.el with package-selected-packages.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable backup.
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable beep.
(setq ring-bell-function 'ignore)

;; Auto reloading when files changed somewhere.
(global-auto-revert-mode 1)

;; Short hand (yes/no -> y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable unnecessary UI.
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Show column numbers on modeline.
(column-number-mode t)

;; Frame settings.
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))

;; Transparent.
(when window-system
  (set-frame-parameter nil 'alpha 90))

;; Font settings.
(when (member "Migu 2M" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Migu 2M 16")))

;; Global key-bindings.
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-'" 'set-mark-command)

;; Do not use tab indent.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Row control settings.
(setq-default kill-whole-line t)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline t)

;; Highlight current line and show line number.
(when (eq window-system 'x)
  (global-display-line-numbers-mode t)
  (custom-set-variables '(display-line-numbers-width-start t))
  (global-hl-line-mode t))

;;
;; environment
;;
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-check-startup-files nil)
  :ensure t)

;;
;; utilities
;;

;; icon
(use-package all-the-icons
  :custom (all-the-icons-scale-factor 1.0)
  :ensure t)

;; dired
(use-package all-the-icons-dired
  :bind
  (:map dired-mode-map
        ("C-m" . dired-open-in-accordance-with-situation)
        ("a" . dired-find-file))
  :config (put 'dired-find-alternate-file 'disabled nil)
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :init
  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (cond ((string-match "\\.\\.?$"
                         (format "%s" (thing-at-point 'filename)))
           (dired-find-alternate-file))
          ((file-directory-p (dired-get-filename))
           (dired-find-alternate-file))
          (t
           (dired-find-file)))))

;; Simple keybindings.
(use-package bind-key
  :ensure t)

;; Force emacs key bindings.
(use-package drill-instructor
  :load-path "github/emacs-drill-instructor"
  :custom (drill-instructor-global t))

;; Multi window settings.
(use-package elscreen
  :config (elscreen-start)
  :custom
  (elscreen-prefix-key (kbd "C-z"))
  (elscreen-tab-display-kill-screen nil)
  (elscreen-tab-display-control nil)
  :ensure t)

;; Guide indent
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

;; Show the directory tree.
(use-package neotree
  :config (bind-key* [f8] 'neotree-toggle)
  :custom
  (neo-show-hidden-files t)
  (neo-create-file-auto-open t)
  (neo-smart-open t)
  (neo-vc-integration '(face char))
  (neo-window-position '(quote right))
  :ensure t)

;; Disposable files.
(use-package open-junk-file
  :custom (open-junk-file-format "/tmp/junk/%Y%m%d%H%M%S.")
  :ensure t)

;; Search Japanese with Roman characters.
(use-package migemo
  :config
  (load-library "migemo")
  (migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :ensure t)

;; Japanese input.
(use-package skk
  :bind ("C-SPC" . skk-mode)
  :custom (skk-egg-like-newline t))

;; Emphasize brackets.
(use-package paren
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :custom-face (show-paren-match ((nil (:background "#44475a" :foreground "violet"))))
  :ensure nil
  :init (show-paren-mode t))

;; Window popup.
(use-package popwin
  :config (popwin-mode 1)
  :custom (special-display-function 'popwin:display-buffer)
  :ensure t)

;; Emphasis brackets.
(use-package rainbow-delimiters
  :config
  (require 'cl-lib)
  (require 'color)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ripgrep settings.
(use-package ripgrep
  :custom
  (ripgrep-executable "~/.cargo/bin/rg")
  (ripgrep-arguments '("-S"))
  :ensure t)

;; Jump to home/end of file.
(use-package sequential-command-config
  :bind
  (("C-a" . seq-home)
   ("C-e" . seq-end)))

;; Completion with brackets and quotations.
(use-package smartchr
  :config
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"")))
  (bind-key "(" (smartchr '("(`!!')" "(")))
  (bind-key "[" (smartchr '("[`!!']" "[")))
  (bind-key "{" (smartchr '("{`!!'}" "{")))
  :load-path "github/emacs-smartchr")

;; Enable to edit root permission files.
(use-package sudo-edit
  :ensure t)

;; Tree of undo.
(use-package undo-tree
  :config (global-undo-tree-mode)
  :ensure t)

;; Highlight results.
(use-package volatile-highlights
  :custom-face (vhl/default-face ((nil (:foreground "dark violet" :background "dark magenta"))))
  :ensure t
  :hook (after-init . volatile-highlights-mode))

;; Guide of key bindings.
(use-package which-key
  :ensure t
  :init (which-key-mode 1))

;; Show the whitespace.
(use-package whitespace
  :config (global-whitespace-mode t)
  :custom
  (whitespace-style '(face
                      empty
                      space-mark
                      spaces
                      tab-mark
                      tabs
                      trailing))
  (whitespace-space-regexp "\\(\u0020+\\|\u3000+\\)")
  (whitespace-tab-regexp "\\(\u0009+\\)")
  (whitespace-display-mappings '((space-mark ?\u0020 [?.])
                                 (space-mark ?\u3000 [?\u25a1])
                                 (tab-mark ?\u0009 [?\xBB ?\t])))
  (whitespace-action '(auto-cleanup))
  :custom-face
  (whitespace-space ((t (:background nil :foreground "Gray22"))))
  (whitespace-tab ((t (:background nil :foreground "LightSkyBlue" :underline t))))
  :ensure t)

;;
;; Completions
;;

;; Completions interface.
(use-package ivy
  :config
  (ivy-mode 1)
  (bind-key "C-;" 'ivy-switch-buffer)
  :custom
  (ivy-count-format "%d/%d ")
  (ivy-format-function 'ivy-format-function-arrow)
  (ivy-height 20)
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  :ensure t)

(use-package counsel
  :config
  (counsel-mode 1)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  :custom
  (enable-recursive-minibuffers t)
  (swiper-include-line-number-in-search t)
  :ensure t)

(use-package ivy-rich
  :config (ivy-rich-mode 1)
  :custom
  (ivy-rich--display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-candidate (:width 32))
       (ivy-rich-switch-buffer-size (:width 8))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :aligh right))
       (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
       (ivy-rich-switch-buffer-project (:width 16 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand)))
     counsel-M-x
     (:columns
      ((counsel-M-x-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer (:width 40))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer (:width 40))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))
  :ensure t)

;; Project interaction library.
(use-package projectile
  :ensure t)

(use-package counsel-projectile
  :ensure t)

;; Code completions.
(use-package company
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("C-i" . company-complete-selection)
   :map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-eslect-previous))
  :custom (company-selection-wrap-around t)
  :ensure t
  :hook (after-init . global-company-mode))

;;
;; Syntax checker.
;;

(use-package flymake
  :bind
  (("<f12>" . flymake-goto-next-error)
   ("<f11>" . flymake-goto-prev-error))
  :config (add-hook 'prog-mode-hook '(lambda () (flymake-mode t))))

(use-package flymake-diagnostic-at-point
  :after flymake
  :config (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  :ensure t)

;;
;; VCS
;;

;; magit
(use-package magit
  :bind
  (("C-c m s" . magit-status)
   ("C-c m b" . magit-blame-addition)
   :map magit-status-mode-map
   ("q" . my/magit-quit-session))
  :config
  (defadvice git-commit-commit (after move-to-magit-buffer activate)
    (delete-window))
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :ensure t
  :init
  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

;; Show the status of lines on fringe.
(use-package git-gutter
  :config (global-git-gutter-mode t)
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :ensure t)

;; LSP
(use-package eglot
  :ensure t)

;; EditorConfig
(use-package editorconfig
  :config (editorconfig-mode 1)
  :ensure t)

;;
;; Major mode
;;

(use-package coffee-mode
  :config
  (bind-key "=" (smartchr '(" = " " == " " = " "=")) coffee-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) coffee-mode-map)
  (bind-key "+" (smartchr '("+" " += " "++")) coffee-mode-map)
  (bind-key "-" (smartchr '("-" " -= " "--")) coffee-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) coffee-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) coffee-mode-map)
  (bind-key ">" (smartchr '(">" " -> " " > " " >= ")) coffee-mode-map)
  :custom (coffee-tab-width 2)
  :ensure t
  :mode ("\\.coffee$" . coffee-mode))

;; Docker
(use-package dockerfile-mode
  :mode (("Dockerfile$" . dockerfile-mode))
  :ensure t)

;; Elm
(use-package elm-mode
  :after company
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) elm-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " >> " ">")) elm-mode-map)
  (bind-key "<" (smartchr '(" < " " <- " " << " "<")) elm-mode-map)
  (bind-key "+" (smartchr '(" ++ " " + " "+")) elm-mode-map)
  (bind-key ":" (smartchr '(" : " " :: " ":")) elm-mode-map)
  (bind-key "|" (smartchr '(" | " "|> " " <|" "|")) elm-mode-map)
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")) elm-mode-map)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elm)
    (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode)
  :ensure t)

;; JavaScript
(use-package js2-mode
  :config
  (bind-key "=" (smartchr '(" = " " === " " = " "=" " == ")) js2-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) js2-mode-map)
  (bind-key "+" (smartchr '("+" "++" " += ")) js2-mode-map)
  (bind-key "-" (smartchr '("-" "--" " -= ")) js2-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) js2-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= ")) js2-mode-map)
  (bind-key ">" (smartchr '(">" " => " " > " " >= ")) js2-mode-map)
  :custom (js-indent-level 2)
  :ensure t
  :mode ("\\.js$" . js2-mode))

;; JSON
(use-package json-mode
  :config (setq js-indent-level 2)
  :ensure t
  :mode ("\\.json$" . json-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" . gfm-mode))

;; PHP
(use-package php-mode
  :config
  (bind-key "=" (smartchr '(" = " " === " "=" " == ")) php-mode-map)
  (bind-key "!" (smartchr '("!" " !== " " != ")) php-mode-map)
  (bind-key "<" (smartchr '("<" " < " " <= " " <<< ")) php-mode-map)
  (bind-key ">" (smartchr '(">" "->" " => ")) php-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'")) php-mode-map)
  (bind-key "(" (smartchr '("(`!!')" "(")) php-mode-map)
  :ensure t
  :hook (php-mode . php-enable-psr2-coding-style)
  :mode ("\\.php$" . php-mode))

;; PlantUML
(use-package plantuml-mode
  :custom (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  :ensure t
  :mode ("\\.p?uml$" . plantuml-mode))

;; Python
(use-package python-mode
  :config
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")) python-mode-map)
  (bind-key "'" (smartchr '("'`!!''" "'" "'''`!!''''")) python-mode-map)
  (bind-key "=" (smartchr '(" = " " == " "=")) python-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) python-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) python-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) python-mode-map)
  (bind-key ">" (smartchr '(">" " -> " " > " " >= ")) python-mode-map)
  (bind-key "<" (smartchr '("<" " <= ")) python-mode-map)
  :ensure t
  :mode ("\\.py$" . python-mode))

(use-package elpy
  :after (flycheck python-mode)
  :ensure t
  :init (elpy-enable))

(use-package pipenv
  :custom
  (pipenv-with-flycheck nil)
  (pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
  :ensure t
  :hook (python-mode . pipenv-mode))

;; Rust
(use-package rust-mode
  :config
  (bind-key "=" (smartchr '(" = " " == " "=")) rust-mode-map)
  (bind-key "+" (smartchr '("+" " + " " += ")) rust-mode-map)
  (bind-key "-" (smartchr '("-" " - " " -= ")) rust-mode-map)
  (bind-key "!" (smartchr '("!" " != ")) rust-mode-map)
  (bind-key ">" (smartchr '(">" " > " " -> " " => " " >= ")) rust-mode-map)
  (bind-key "<" (smartchr '("<`!!'>" "<" " < " " <- " " <= ")) rust-mode-map)
  (bind-key "|" (smartchr '("|`!!'|" "||" " | " "|")) rust-mode-map)
  :custom (rust-format-on-save t)
  :ensure t
  :hook (rust-mode . eglot-ensure))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; SCSS
(use-package scss-mode
  :custom (scss-compile-at-save nil)
  :ensure t
  :hook (scss-mode . my-scss-mode-hook)
  :init
  (defun my-scss-mode-hook ()
    (set (make-local-variable 'css-indent-offset) 2))
  :mode ("\\.scss$" . scss-mode))

;; Shell
(use-package sh-mode
  :mode
  (("\\.z?sh$" . sh-mode)
   ("\\.env$" . sh-mode)
   ("\\.sample$" . sh-mode)
   ("rc$" . sh-mode)))

;; TOML
(use-package toml-mode
  :ensure t
  :mode
  (("\\.toml$" . toml-mode)
   ("^Pipfile$" . toml-mode)))

;; HTML/CSS
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  :custom-face
  (web-mode-doctype-face ((t (:foreground "#82AE46"))))
  (web-mode-html-tag-face ((t (:foreground "#E6B422"))))
  (web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
  (web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
  (web-mode-comment-face ((t (:foreground "#D9333F"))))
  (web-mode-server-comment-face ((t (:foreground "#D9333F"))))
  (web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
  (web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
  (web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
  :ensure t
  :mode
  (("\\.html$" . web-mode)
   ("\\.css$" . web-mode)))

;; YAML
(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode)
  :ensure t)

;; F#
(use-package fsharp-mode
  :config
  (use-package eglot-fsharp)
  (bind-key "=" (smartchr '(" = " "=")) fsharp-mode-map)
  (bind-key ">" (smartchr '(" > " " -> " " >> " " >=> " ">")) fsharp-mode-map)
  (bind-key "<" (smartchr '("<`!!'>" " < " " <- " " << " "<")) fsharp-mode-map)
  (bind-key "+" (smartchr '(" + " "+")) fsharp-mode-map)
  (bind-key "*" (smartchr '(" * " " ** " "*")) fsharp-mode-map)
  (bind-key ":" (smartchr '(": " " :: " ":")) fsharp-mode-map)
  (bind-key "|" (smartchr '("|" "|> " "<| " " | ")) fsharp-mode-map)
  (bind-key "`" (smartchr '("\``!!'\`" "\`\``!!'\`\`" "\`")) fsharp-mode-map)
  (bind-key "\"" (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")) fsharp-mode-map)
  (modify-coding-system-alist 'file "\\.fs\\'" 'utf-8-dos)
  :ensure t
  :hook (fsharp-mode . eglot-ensure))

;;
;; UI
;;
(use-package doom-themes
  :config (load-theme 'doom-dark+ t)
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :ensure t)

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))

(use-package doom-modeline
  :commands (doom-modeline-def-modeline)
  :config (doom-modeline-def-modeline 'my-modeline
            '(buffer-info buffer-encoding buffer-position selection-info major-mode vcs checker))
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-mode-icon nil)
  (doom-modeline-vcs-max-length 24)
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  (doom-modeline-mode . my-doom-modeline-hook)
  :init
  (defun my-doom-modeline-hook ()
    (doom-modeline-set-modeline 'my-modeline 'default)))

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:
