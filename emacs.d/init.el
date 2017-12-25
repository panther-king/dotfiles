;; パッケージ関連の初期設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; 利用するパッケージ
(package-install 'all-the-icons)
(package-install 'all-the-icons-dired)
(package-install 'cargo)
(package-install 'color-theme-sanityinc-tomorrow)
(package-install 'company)
(package-install 'company-jedi)
(package-install 'dockerfile-mode)
(package-install 'dumb-jump)
(package-install 'elscreen)
(package-install 'flycheck)
(package-install 'flycheck-pos-tip)
(package-install 'foreign-regexp)
(package-install 'git-gutter-fringe+)
(package-install 'gitignore-mode)
(package-install 'haskell-mode)
(package-install 'helm)
(package-install 'helm-ag)
(package-install 'helm-core)
(package-install 'helm-projectile)
(package-install 'init-loader)
(package-install 'jedi-core)
(package-install 'js2-mode)
(package-install 'json-mode)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'migemo)
(package-install 'mozc-popup)
(package-install 'neotree)
(package-install 'omni-scratch)
(package-install 'open-junk-file)
(package-install 'php-mode)
(package-install 'plantuml-mode)
(package-install 'popwin)
(package-install 'powerline)
(package-install 'projectile)
(package-install 'python-mode)
(package-install 'racer)
(package-install 'rainbow-delimiters)
(package-install 'redo+)
(package-install 'ripgrep)
(package-install 'ruby-mode)
(package-install 'rust-mode)
(package-install 'toml-mode)
(package-install 'typescript-mode)
(package-install 'undo-tree)
(package-install 'undohist)
(package-install 'web-mode)
(package-install 'whitespace)
(package-install 'yaml-mode)

;; 設定ファイルを指定した順に読み込む
(use-package init-loader
  :config
  (init-loader-load "~/.emacs.d/loader"))

;; package-selected-packagesの書き出し先を変更
(load (setq custom-file "~/.emacs.d/custom.el"))
