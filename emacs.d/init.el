;; パッケージ関連の初期設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; 利用するパッケージ
(package-install 'anzu)
(package-install 'company-jedi)
(package-install 'dumb-jump)
(package-install 'exec-path-from-shell)
(package-install 'helm-ag)
(package-install 'helm-core)
(package-install 'helm-projectile)

;; 設定ファイルを指定した順に読み込む
(use-package init-loader
  :ensure t
  :config
  (init-loader-load "~/.emacs.d/loader"))

;; package-selected-packagesの書き出し先を変更
(load (setq custom-file "~/.emacs.d/custom.el"))
