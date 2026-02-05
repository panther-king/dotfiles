;;; init.el --- My init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; My init.el.

;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize))

(setq custom-file (locate-user-emacs-file "custom.el"))  ;; カスタマイズ保存ファイルを指定するがロードはしない

;;
;; ベース設定
;;

(use-package emacs
  :ensure nil
  :config
  (fset 'yes-or-no-p 'y-or-n-p)                                     ;; プロンプトでyes/noを短縮する
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))           ;; ウインドウを透過する
  (add-to-list 'default-frame-alist '(font . "UDEV Gothic NF 14"))  ;; フォントはUDEV Gothic
  :custom
  (auto-save-default nil)                             ;; 自動保存を行わない
  (column-number-mode t)                              ;; モードラインに列番号も表示する
  (create-lockfiles nil)                              ;; ロックファイルを作成しない
  (cua-mode t)                                        ;; 矩形編集のためにcua-modeを有効にする
  (cua-enable-cua-keys nil)                           ;; cuaのデフォルトキーバインドは利用しない
  (indent-tabs-mode nil)                              ;; タブインデントは利用しない
  (inhibit-startup-screen t)                          ;; 起動画面を表示しない
  (initial-scratch-message nil)                       ;; Scratchバッファにメッセージは表示しない
  (kill-whole-line t)                                 ;; C-kで行末の改行コードごと削除する
  (make-backup-files nil)                             ;; バックアップファイルを作成しない
  (menu-bar-mode nil)                                 ;; メニューバーは利用しない
  (native-comp-async-report-warnings-errors 'silent)  ;; サブプロセスのネイティブコンパイル警告は *Warnings* に出す
  (next-line-add-newlines nil)                        ;; バッファの末尾で新しい行を追加しない
  (package-install-upgrade-built-in t)                ;; ビルトインパッケージも更新対象にする
  (package-native-compile t)                          ;; インストール時にネイティブコンパイルする
  (require-final-newline t)                           ;; ファイルの末尾は改行を必須にする
  (ring-bell-function 'ignore)                        ;; ビープ音を無効化
  (scroll-bar-mode nil)                               ;; スクロールバーは利用しない
  (tool-bar-mode nil)                                 ;; ツールバーは利用しない
  (vc-follow-symlinks t))                             ;; 常にシンボリックリンクをたどる

;;
;; パッケージ設定
;;

(use-package use-package
  :custom (use-package-always-ensure t))  ;; ensureを省略しても自動的にインストールする

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package auto-package-update
  :config (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t)                           ;; 旧バージョンのパッケージは削除
  (auto-package-update-excluded-packages '(drill-instructor smartchr))  ;; ELPAからインストールしていないパッケージは対象外
  (auto-package-update-prompt-before-update t)                          ;; アップデート実行前に確認
  (auto-package-update-show-preview t))                                 ;; アップデート対象パッケージを事前に表示する

;;
;; テーマ設定
;;

(use-package catppuccin-theme
  :custom
  (catppuccin-highlight-matches t)  ;; 検索キーワードをハイライトする
  (catppuccin-italic-comments t)    ;; コメントは斜体にする
  :init (load-theme 'catppuccin :no-confirm))

;;
;; モードライン設定
;;

;; モードラインのマイナーモード表示をシンプルにする
(use-package minions
  :config (minions-mode 1)
  :custom
  (minions-mode-line-lighter "[+]")           ;; minor-modeを展開するUIを変更する
  (minions-prominent-mode '(flymake-mode)))   ;; エラー情報を可視化するため、flymakeは常に表示する

;; モードラインの表示を分かりやすくする
(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; モードラインのファイル名にディレクトリ名も表示する
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)  ;; ディレクトリ名はファイル名の前に表示する
  (uniquify-min-dir-content 2)           ;; 同名ファイルを判別できるよう親階層も表示させる
  :ensure nil)

;;
;; バッファ設定
;;

;; 行番号を表示させる
(use-package display-line-numbers
  :custom
  (display-line-numbers-width-start t)        ;; 行番号の幅を最大行に合わせる
  :ensure nil
  :hook
  ((prog-mode . display-line-numbers-mode)    ;; プログラミングモードで行番号を表示する
   (conf-mode . display-line-numbers-mode)    ;; 設定ファイルモードでも行番号を表示する
   (text-mode . display-line-numbers-mode)))  ;; テキストモードでも行番号を表示する

;; インデントを可視化する
(use-package highlight-indent-guides
  :config
  (custom-set-faces
   `(highlight-indent-guides-top-character-face
     ((t (:foreground ,(cdr (assoc 'overlay0 catppuccin-mocha-colors))))))
   `(highlight-indent-guides-character-face
     ((t (:foreground ,(cdr (assoc 'surface0 catppuccin-mocha-colors)))))))
  :custom
  (highlight-indent-guides-auto-enabled nil)  ;; カラーはカスタム定義する
  (highlight-indent-guides-method 'bitmap)    ;; インデントガイドをbitmapで表示する
  (highlight-indent-guides-responsive 'top)   ;; 現在のインデントガイドを強調する
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (html-ts-mode . highlight-indent-guides-mode)))

;; 現在行をハイライトする
(use-package hl-line
  :custom (global-hl-line-mode t)  ;; 常にハイライトさせる
  :ensure nil)

;; 特定操作の実行をハイライトする
(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode))  ;; 全バッファで有効にする

;; スペース・タブを可視化する
(use-package whitespace
  :config (global-whitespace-mode t)
  :custom
  (whitespace-action '(auto-cleanup))          ;; 保存時に余計な空白・タブを削除
  (whitespace-global-modes '(not dired-mode))  ;; diredモードは除外する
  (whitespace-style '(face                     ;; 可視化の有効化
                      empty                    ;; バッファ前後の空行を可視化
                      tab-mark                 ;; タブを専用マークで表示
                      tabs                     ;; タブを可視化
                      trailing))               ;; 行末の空白を可視化
  :ensure nil)

;;
;; カッコ設定
;;

;; 対応するカッコを強調表示する
(use-package paren
  :config (show-paren-mode t)
  :custom
  (show-paren-style 'mixed)                ;; 対応カッコが画面外なら式全体をハイライトする
  (show-paren-when-point-in-periphery t)   ;; 論理的に最も近いカッコをハイライトする
  (show-paren-when-point-inside-paren t)   ;; カーソルがカッコの内側にあってもハイライトする
  :ensure nil)

;; カッコの対応を色づけする
(use-package rainbow-delimiters
  :hook prog-mode)

;; カッコを自動的に補完する
(use-package smartparens
  :config (require 'smartparens-config)
  :hook prog-mode)

;;
;; アイコン設定
;;

;; アイコンを利用する
(use-package nerd-icons)

;; corfuでもnerd-iconsを利用する
(use-package nerd-icons-corfu
  :after corfu nerd-icons
  :config
  (defvar corfu-margin-formatters)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :vc (:fetcher github :repo "LuigiPiucco/nerd-icons-corfu"))

;; verticoでもnerd-iconsを利用する
(use-package nerd-icons-completion
  :after nerd-icons
  :hook (after-init . nerd-icons-completion-mode))

;;
;; 便利系拡張設定
;;

;; Emacs外でファイルが変更されたら自動的に読み込み直す
(use-package autorevert
  :custom (global-auto-revert-mode t))  ;; 常に自動更新する

;; シェルの環境変数を引き継ぐ
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))  ;; すべての環境変数を引き継ぐ

;; 一時ファイルを作成する
(use-package open-junk-file
  :custom (open-junk-file-format "/tmp/junk/%Y%m%d-%H%M%S."))  ;; 一時ファイルは/tmpに保存する

;; 最近開いたファイルを保存する
(use-package recentf
  :config (recentf-mode 1)
  :custom
  (recentf-max-saved-items 2000)     ;; 保存アイテム数
  (recentf-filename-handlers nil)
  (recentf-exclude '(".recentf")))   ;; .recentfファイルは対象としない

;; 検索にripgrepを利用する
(use-package rg
  :config (rg-enable-default-bindings))  ;; magit同様のデフォルトキーバインドを利用する

;; ミニバッファの履歴を保存する
(use-package savehist
  :config (savehist-mode t)
  :ensure nil)

;; Emacsからroot権限でファイルを編集できるようにする
(use-package sudo-edit)

;; undo履歴をツリー形式で可視化する
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))  ;; 差分ツリーはユニコードも利用する

;;
;; IDE設定
;;

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               ;; paru -S vscode-langservers-extracted
               '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; paru -S dockerfile-language-server
               '(dockerfile-ts-mode . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; paru -S vscode-langservers-extracted
               '(json-ts-mode . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; paru -S nodejs-intelephense
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               ;; paru -S taplo-cli
               '(toml-ts-mode . ("taplo" "lsp" "stdio")))
  (add-to-list 'eglot-server-programs
               ;; paru -S typescript-language-server
               '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  :hook
  ((css-ts-mode . eglot-ensure)
   (dockerfile-ts-mode . eglot-ensure)
   (elm-mode . eglot-ensure)
   (fsharp-mode . eglot-ensure)
   (haskell-mode . eglot-ensure)
   (html-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (json-ts-mode . eglot-ensure)
   (php-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (terraform-mode . eglot-ensure)
   (toml-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (eglot-managed-mode . (lambda ()
                           (add-hook 'before-save-hook 'eglot-format-buffer -10 t)))))
;; eglotのlspを高速化する
;; cargo install emacs-lsp-booster
(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode)
  :vc (:fetcher github :repo "jdtsmith/eglot-booster"))

;; コードブロックの折り畳みキーバインド
(use-package hideshow
  :bind ("C-]" . hs-toggle-hiding)
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

;; EmacsでGithub Copilotを利用する
(use-package copilot
  :bind
  (:map copilot-mode-map
        ("C-c c" . copilot-complete)          ;; 補完候補を表示する
        ("M-i" . copilot-accept-completion))  ;; 補完を受け入れる
  :custom
  (copilot-idle-delay nil)                    ;; 自動補完を無効にする
  (copilot-indent-offset-warning-disable t)   ;; インデント警告を無効化する
  :hook
  ((prog-mode . copilot-mode)
   (text-mode . copilot-mode)))

;; dirvishでdiredを拡張する
(use-package dirvish
  :bind ("C-c d" . dirvish-side)
  :config (dirvish-side-follow-mode)
  :custom
  (dirvish-attributes '(vc-state                             ;; フリンジにgitの状態を表示する
                        subtree-state                        ;; ディレクトリの階層有無を表示する
                        nerd-icons                           ;; アイコンを表示する
                        collapse                             ;; 詳細を折りたたんで表示する
                        file-modes                           ;; パーミッションを表示する
                        file-size                            ;; ファイルサイズを表示する
                        file-time                            ;; タイムスタンプを表示する
                        git-msg))                            ;; 直近のgitコミットメッセージを表示する
  (dirvish-mode-line-height 30)                              ;; モードラインの高さはmoodyのデフォルトに合わせる
  (dirvish-side-attributes '(vc-state                        ;; フリンジにgitの状態を表示する
                             nerd-icons                      ;; アイコンを表示する
                             collapse))                      ;; 詳細を折りたたんで表示する
  (dirvish-project-root-function #'projectile-project-root)  ;; projectileのrootを認識させる
  (dirvish-time-format-string "%Y-%m-%d %R")                 ;; タイムスタンプは西暦4ケタで表示する
  :init (dirvish-override-dired-mode t))

;; EmacsにEditorConfigを認識させる
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Emacsでmiseを利用する
(use-package mise
  :hook (after-init . global-mise-mode))

;; Emacsでプロジェクト管理を行う
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-mode +1))

;;
;; キーバインド拡張設定
;;

;; よく利用する機能を特定のキーバインドにマッピングする
(use-package bind-key
  :bind
  (("C-h" . delete-backward-char)  ;; 1文字前を削除
   ("C-'" . set-mark-command)))    ;; リージョン選択

;; Emacsキーバインドを強制する
(use-package drill-instructor
  :config (setq drill-instructor-global t)  ;; 常にEmacsキーバインドを強制する
  :vc (:fetcher github :repo "k1LoW/emacs-drill-instructor"))

;; 同一キーの入力で入力内容を切り替える
(use-package smartchr
  :config
  (declare-function smartchr "smartchr" (&rest strings))  ;; 古い関数でautoloadコメントが無いため、ここで伝えておく
  ;; Elm
  (defun my/elm-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = " " == "))
    (local-set-key (kbd "+") (smartchr "+" " ++ " " + "))
    (local-set-key (kbd "-") (smartchr "-" " - "))
    (local-set-key (kbd ">") (smartchr ">" " > " " -> " " >> "))
    (local-set-key (kbd "<") (smartchr "<" " < " " <- " " << "))
    (local-set-key (kbd "|") (smartchr "|" "|> " "<| " " | "))
    (local-set-key (kbd ":") (smartchr ":" " : " " :: ")))
  ;; F#
  (defun my/fsharp-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = "))
    (local-set-key (kbd "+") (smartchr "+" " + "))
    (local-set-key (kbd "-") (smartchr "-" " - "))
    (local-set-key (kbd ">") (smartchr ">" " > " " -> " " >> " " >=> " " >= "))
    (local-set-key (kbd "<") (smartchr "<" "<`!!'>" " < " " <- " " << " " <= "))
    (local-set-key (kbd ":") (smartchr ":" ": " " :: "))
    (local-set-key (kbd "|") (smartchr "|" "|> " "<| " " | ")))
  ;; JavaScript
  (defun my/js2-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = " " === " " == "))
    (local-set-key (kbd "+") (smartchr "+" " += " "++"))
    (local-set-key (kbd "-") (smartchr "-" "-=" "--"))
    (local-set-key (kbd ">") (smartchr ">" "->" " => " " > " " >= "))
    (local-set-key (kbd "<") (smartchr "<" " <= " " =< " " <<< "))
    (local-set-key (kbd "!") (smartchr "!" " !== " " != ")))
  ;; PHP
  (defun my/php-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = " " === " " == "))
    (local-set-key (kbd "+") (smartchr "+" " += " "++"))
    (local-set-key (kbd "-") (smartchr "-" "-=" "--"))
    (local-set-key (kbd ">") (smartchr ">" "->" " => " " > " " >= "))
    (local-set-key (kbd "<") (smartchr "<" " <= " " =< " " <<< "))
    (local-set-key (kbd "!") (smartchr "!" " !== " " != ")))
  ;; Python
  (defun my/python-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = " " == "))
    (local-set-key (kbd "+") (smartchr "+" " + " " += "))
    (local-set-key (kbd "-") (smartchr "-" " - " " -= "))
    (local-set-key (kbd "<") (smartchr "<" " < " " <= "))
    (local-set-key (kbd ">") (smartchr ">" " > " " >= " " -> "))
    (local-set-key (kbd "!") (smartchr "!" " != ")))
  ;; Rust
  (defun my/rust-smartchr-init ()
    (local-set-key (kbd "=") (smartchr "=" " = " " == "))
    (local-set-key (kbd "+") (smartchr "+" " + " " += "))
    (local-set-key (kbd "-") (smartchr "-" " - " " -= "))
    (local-set-key (kbd ">") (smartchr ">" " > " " -> " " => " " >= "))
    (local-set-key (kbd "<") (smartchr "<" "<`!!'>" " < " " <- " " <= "))
    (local-set-key (kbd "!") (smartchr "!" " != "))
    (local-set-key (kbd "|") (smartchr "|" "|`!!'|" " || " " | ")))
  :hook
  ((elm-mode . my/elm-smartchr-init)
   (fsharp-mode . my/fsharp-smartchr-init)
   (js2-mode . my/js2-smartchr-init)
   (php-mode . my/php-smartchr-init)
   (python-ts-mode . my/python-smartchr-init)
   (rust-ts-mode . my/rust-smartchr-init)
   (typescript-ts-mode . my/js2/smartchr-init))
  :vc (:fetcher github :repo "imakado/emacs-smartchr"))

;; prefixキーの次の操作をナビゲーションする
(use-package which-key
  :config (which-key-mode 1))

;;
;; 日本語入力設定
;;

;; 日本語入力にDDSKKを利用する
(use-package ddskk
  :bind ("C-x j" . skk-mode)
  :custom
  (skk-auto-insert-paren t)                                                       ;; 全角のカッコを自動補完する
  (skk-use-jisx0201-input-method t)                                               ;; 半角カナモードを利用する
  (skk-cursor-hiragana-color (cdr (assoc 'green catppuccin-mocha-colors)))        ;; ひらがなモード
  (skk-cursor-jisx0208-latin-color (cdr (assoc 'mauve catppuccin-mocha-colors)))  ;; 全角英数モード
  (skk-cursor-katakana-color (cdr (assoc 'red catppuccin-mocha-colors)))          ;; カタカナモード
  (skk-cursor-jisx0201-color (cdr (assoc 'yellow catppuccin-mocha-colors)))       ;; 半角カナモード
  (skk-egg-like-newline t)                                                        ;; Enterキーでも入力を確定する
  (skk-hiragana-mode-string "[あ]")                                               ;; ひらがなモードのモードライン表示
  (skk-jisx0208-latin-mode-string "[Ａ]")                                         ;; 全角英数モードのモードライン表示
  (skk-katakana-mode-string "[ア]")                                               ;; カタカナモードのモードライン表示
  (skk-jisx0201-mode-sring "[ｱ]")                                                 ;; 半角カナモードのモードライン表示
  (skk-latin-mode-string "[_A]")                                                  ;; ASCIIモードのモードライン表示
  (skk-show-annotation t)                                                         ;; 変換候補に注釈を表示する
  (skk-use-color-cursor t))                                                       ;; カーソルカラーでモードが判別できるようにする

;;
;; git設定
;;

;; ファイルの編集状況をフリンジに表示する
(use-package git-gutter
  :config (global-git-gutter-mode t)
  :custom
  (git-gutter:modified-sign "~")   ;; 変更
  (git-gutter:added-sign "+")      ;; 追加
  (git-gutter:deleted-sign "!"))   ;; 削除

;; gitの設定ファイルメジャーモード
(use-package git-modes)

;; gitをEmacsから操作する
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c g" . magit-dispatch)
   ("C-c f" . magit-file-dispatch))
  :custom (auto-revert-check-vc-info t))  ;; magitのブランチ変更を自動的に検知する

;; magitのdiff表示にdeltaを利用する
(use-package magit-delta
  :after magit
  :custom
  (magit-delta-default-dark-theme "Catppuccin Mocha")  ;; bat --list-themes
  (magit-delta-hide-plus-minus-markers nil)            ;; diffの行頭に+/-を表示する
  :hook magit-mode)

;;
;; 構文チェック設定
;;

;; Syntax checkにflymakeを利用する
(use-package flymake
  :bind
  (("<f12>" . flymake-goto-next-error)
   ("<f11>" . flymake-goto-prev-error))
  :ensure nil
  :hook prog-mode)

;;
;; 構文ハイライト設定
;;

;; tree-sitterでハイライトをより正確にする
(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
          (phpdoc . ("https://github.com/claytonrcarter/tree-sitter-phpdoc"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/ikatyang/tree-sitter-toml/"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))
  (defun my/treesit-install-all ()
    "install all treesit language sources"
    (interactive)
    (let ((count 0)
          (errors 0))
      (dolist (source treesit-language-source-alist)
        (let ((lang (car source)))
          (cond
           ((treesit-language-available-p lang)
            (message "Skipping %s (already installed)" lang))
           (t
            (message "Installing %s..." lang)
            (condition-case err
                (progn
                  (treesit-install-language-grammar lang)
                  (setq count (1+ count)))
              (error
               (setq errors (1+ errors))
               (message "❌ Failed to install %s: %s" lang err)))))))
          (message "✅ Tree-sitter setup completed! (Installed: %d, Errors: %d)" count errors)))
  :custom (treesit-font-lock-level 4)  ;; 最大限ハイライトする
  :ensure nil
  :init
  (setq major-mode-remap-alist
        '((css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (toml-mode . toml-ts-mode)
          (typescript-mode . typescript-ts-mode))))

;;
;; プログラミング言語設定
;;

;; Elm
(use-package elm-mode)

;; F#
(use-package fsharp-mode)
(use-package eglot-fsharp
  :after fsharp-mode)

;; Haskell
(use-package haskell-mode)

;; PHP
(use-package php-mode
  :custom (php-mode-coding-style 'psr2)
  :mode "\\.php\\'")

;; Shell script
(use-package sh-mode
  :ensure nil
  :mode ("\\.z?sh\\'" "\\.env\\'" "\\.sample\\'" "rc\\'"))

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode))

;; Typescript
(use-package typescript-ts-mode
  :custom (typescript-ts-mode-indent-level 2)  ;; TypeScriptのインデントは2スペース
  :ensure nil
  :mode
  (("\\.ts\\'" . typescript-ts-mode)
   ("\\.tsx\\'" . typescript-ts-mode)))

;;
;; 構造化言語設定
;;

;; JSON
(use-package json-ts-mode
  :custom (json-ts-mode-indent-offset 2)  ;; JSONのインデントは2スペース
  :ensure nil)

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom (markdown-command "pandoc")
  :hook (markdown-mode . (lambda () (setq-local whitespace-action nil))))  ;; Markdown編集時に行末の空白を削除しない
(use-package markdown-preview-mode)

;; PlantUML
(use-package plantuml-mode
  :if (file-exists-p "/usr/share/java/plantuml/plantuml.jar")
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")  ;; pacmanで入るPlantUMLを利用する
  (plantuml-default-exec-mode 'jar)                            ;; jarを利用してレンダリングする
  (plantuml-indent-level 4)                                    ;; 4スペースインデント
  :mode "\\.p?uml\\'")

;; Terraform
(use-package terraform-mode
  :custom (terraform-command "tofu"))

;; TOML
(use-package toml-ts-mode
  :ensure nil
  :mode
  (("\\.toml\\'" . toml-ts-mode)
   ("^Pipfile\\'" . toml-ts-mode)))

;; HTML
(use-package html-ts-mode
  :ensure nil
  :mode "\\.html\\'")
(use-package emmet-mode
  :bind
  (:map emmet-mode-keymap
        ("C-c e" . emmet-expand-line)  ;; div.container>ul>li*3 の記法を展開する
        ("C-j" . nil))
  :hook (html-ts-mode . emmet-mode))
(use-package rainbow-mode
  :hook (html-ts-mode . rainbow-mode))
(use-package auto-rename-tag
  :hook (html-ts-mode . auto-rename-tag-mode)  ;; タグ名を変更すると閉じタグも追随させる
  :vc (:fetcher github :repo "jcs-elpa/auto-rename-tag"))

(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)    ;; JavaScriptは2スペースインデント
  (web-mode-css-indent-offset 2)     ;; CSSは2スペースインデント
  (web-mode-markup-indent-offset 2)  ;; HTMLは2スペースインデント
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  :mode "\\.blade\\.php\\'")

;; YAML
(use-package yaml-mode)
(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-mode))

;;
;; 絞り込み・補完設定
;;

;; 補完コマンドを利用する
(use-package consult
  :bind
  (("C-;" . consult-buffer)
   ("C-o" . consult-outline)
   ("C-s" . consult-line)
   ("C-r" . consult-line)
   ([remap goto-line] . consult-goto-line)))

;; コードの補完候補をポップアップする
(use-package corfu
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init (global-corfu-mode))

;; 補完候補の情報を表示する
(use-package marginalia
  :hook after-init)

;; 補完候補に対するアクションを行えるようにする
(use-package embark)

;; consultとembarkを組み合わせて利用する
(use-package embark-consult
  :after (consult embark))

;; 順不同の複数キーワードで補完候補を絞り込む
(use-package orderless
  :custom (completion-styles '(orderless basic)))

;; 補完候補にアイコンを利用する
(use-package kind-icon
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; 補完方法をカスタマイズする
(use-package cape)

;; ミニバッファで補完を利用する
(use-package vertico
  :bind
  (:map vertico-map
        ("C-v" . vertico-scroll-up)
        ("M-v" . vertico-scroll-down))
  :custom
  (vertico-count 20)                            ;; 候補表示は20個まで
  (vertico-cycle t)                             ;; 候補の先頭・末尾を移動できるようにする
  (vertico-sort-function 'vertico-sort-alpha)   ;; 候補はアルファベット順で表示する
  :init (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        ("C-l" . vertico-directory-up))  ;; C-lでディレクトリ階層を上がれるようにする
  :ensure nil)

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not cl-function obsolete)
;; End:

;;; init.el ends here.
