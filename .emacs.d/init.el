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

(fset 'yes-or-no-p 'y-or-n-p)                                     ;; プロンプトでyes/noを短縮する
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))           ;; ウインドウを透過する
(add-to-list 'default-frame-alist '(font . "UDEV Gothic NF 14"))  ;; フォントはUDEV Gothic

(custom-set-variables
 '(auto-save-default nil)                             ;; 自動保存を行わない
 '(column-number-mode t)                              ;; モードラインに列番号も表示する
 '(create-lockfiles nil)                              ;; ロックファイルを作成しない
 '(cua-mode t)                                        ;; 矩形編集のためにcua-modeを有効にする
 '(cua-enable-cua-keys nil)                           ;; cuaのデフォルトキーバインドは利用しない
 '(indent-tabs-mode nil)                              ;; タブインデントは利用しない
 '(inhibit-startup-screen t)                          ;; 起動画面を表示しない
 '(initial-scratch-message nil)                       ;; Scratchバッファにメッセージは表示しない
 '(kill-whole-line t)                                 ;; C-kで行末の改行コードごと削除する
 '(make-backup-files nil)                             ;; バックアップファイルを作成しない
 '(menu-bar-mode nil)                                 ;; メニューバーは利用しない
 '(native-comp-async-report-warnings-errors 'silent)  ;; サブプロセスのネイティブコンパイル警告は *Warnings* に出す
 '(next-line-add-newlines nil)                        ;; バッファの末尾で新しい行を追加しない
 '(package-install-upgrade-built-in t)                ;; ビルトインパッケージも更新対象にする
 '(package-native-compile t)                          ;; インストール時にネイティブコンパイルする
 '(require-final-newline t)                           ;; ファイルの末尾は改行を必須にする
 '(ring-bell-function 'ignore)                        ;; ビープ音を無効化
 '(scroll-bar-mode nil)                               ;; スクロールバーは利用しない
 '(tool-bar-mode nil)                                 ;; ツールバーは利用しない
 '(vc-follow-symlinks t))                             ;; 常にシンボリックリンクをたどる

(global-set-key (kbd "C-]") 'hs-toggle-hiding)
(add-hook 'prog-mode-hook
          #'(lambda () (hs-minor-mode 1)))

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
  ((auto-package-update-delete-old-versions t)                           ;; 旧バージョンのパッケージは削除
   (auto-package-update-excluded-packages '(drill-instructor smartchr))  ;; ELPAからインストールしていないパッケージは対象外
   (auto-package-update-prompt-before-update t)                          ;; アップデート実行前に確認
   (auto-package-update-show-preview t)))                                ;; アップデート対象パッケージを事前に表示する



;;
;; テーマ設定
;;

(use-package catppuccin-theme
  :custom
  ((catppuccin-highlight-matches t)  ;; 検索キーワードをハイライトする
   (catppuccin-italic-comments t))   ;; コメントは斜体にする
  :init (load-theme 'catppuccin :no-confirm))

;;
;; モードライン設定
;;

;; モードラインのマイナーモード表示をシンプルにする
(use-package minions
  :config (minions-mode 1)
  :custom
  ((minions-mode-line-lighter "[+]")           ;; minor-modeを展開するUIを変更する
   (minions-prominent-mode '(flymake-mode))))  ;; エラー情報を可視化するため、flymakeは常に表示する

;; モードラインの表示を分かりやすくする
(use-package moody
  :config
  (progn
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)))

;; モードラインのファイル名にディレクトリ名も表示する
(use-package uniquify
  :custom
  ((uniquify-buffer-name-style 'forward)  ;; ディレクトリ名はファイル名の前に表示する
   (uniquify-min-dir-content 1))          ;; centaur-tabs と競合するため1階層だけ表示する
  :ensure nil)

;;
;; バッファ設定
;;

;; 行番号を表示させる
(use-package display-line-numbers
  :custom
  ((display-line-numbers-width-start t)   ;; 行番号の幅を最大行に合わせる
   (global-display-line-numbers-mode t))  ;; 常に表示させる
  :ensure nil)

;; インデントを可視化する
(use-package highlight-indent-guides
  :config
  (custom-set-faces
   `(highlight-indent-guides-top-character-face
     ((t (:foreground ,(cdr (assoc 'overlay0 catppuccin-mocha-colors))))))
   `(highlight-indent-guides-character-face
     ((t (:foreground ,(cdr (assoc 'surface0 catppuccin-mocha-colors)))))))
  :custom
  ((highlight-indent-guides-auto-enabled nil)  ;; カラーはカスタム定義する
   (highlight-indent-guides-method 'bitmap)    ;; インデントガイドをbitmapで表示する
   (highlight-indent-guides-responsive 'top))  ;; 現在のインデントガイドを強調する
  :hook prog-mode)

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
  ((whitespace-action '(auto-cleanup))          ;; 保存時に余計な空白・タブを削除
   (whitespace-global-modes '(not dired-mode))  ;; diredモードは除外する
   (whitespace-style '(face                     ;; 可視化の有効化
                       empty                    ;; バッファ前後の空行を可視化
                       tab-mark                 ;; タブを専用マークで表示
                       tabs                     ;; タブを可視化
                       trailing)))              ;; 行末の空白を可視化
  :ensure nil)

;;
;; カッコ設定
;;

;; 対応するカッコを強調表示する
(use-package paren
  :config (show-paren-mode t)
  :custom
  ((show-paren-style 'mixed)                ;; 対応カッコが画面外なら式全体をハイライトする
   (show-paren-when-point-in-periphery t)   ;; 論理的に最も近いカッコをハイライトする
   (show-paren-when-point-inside-paren t))  ;; カーソルがカッコの内側にあってもハイライトする
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
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :vc (:fetcher github :repo LuigiPiucco/nerd-icons-corfu))

;; verticoでもnerd-iconsを利用する
(use-package nerd-icons-completion
  :after nerd-icons
  :hook (after-init . nerd-icons-completion-mode))

;; diredでもnerd-iconsのアイコンを利用する
(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

;;
;; 便利系拡張設定
;;

;; Emacs外でファイルが変更されたら自動的に読み込み直す
(use-package autorevert
  :commands global-auto-revert-mode)

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
  ((recentf-max-saved-items 2000)     ;; 保存アイテム数
   (recentf-filename-handlers nil)
   (recentf-exclude '(".recentf"))))  ;; .recentfファイルは対象としない

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
(use-package undo-tree
  :custom
  ((global-undo-tree-mode t)            ;; 常に有効にする
   (undo-tree-auto-save-history nil)))  ;; 履歴をファイルに保存しない

;;
;; IDE設定
;;

;; バッファをタブで管理する
(use-package centaur-tabs
  :config (centaur-tabs-mode t)
  :custom
  ((centaur-tabs-gray-out-icons 'buffer)  ;; アクティブでないバッファのアイコンはグレーアウト
   (centaur-tabs-height 28)               ;; タブの高さは標準より少し高め
   (centaur-tabs-icon-type 'nerd-icons)   ;; アイコンはNerd Iconで統一
   (centaur-tabs-set-bar 'left )          ;; アクティブタブの左にバーを表示
   (centaur-tabs-set-icons t)             ;; タブにアイコンを表示する
   (centaur-tabs-style "box"))            ;; タブスタイルはシンプルなスクエア
  :demand t)

;; EmacsでGithub Copilotを利用する
(use-package copilot
  :bind
  (:map copilot-mode-map
        ("M-i" . copilot-accept-completion))
  :custom (copilot-indent-offset-warning-disable t)  ;; インデント警告を無効化する
  :hook prog-mode)

;; EmacsにEditorConfigを認識させる
(use-package editorconfig
  :config (editorconfig-mode 1))

;; eglotのlspを高速化する
;; cargo install emacs-lsp-booster
(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode)
  :vc (:fetcher github :repo jdtsmith/eglot-booster))

;; Emacsでmiseを利用する
(use-package mise
  :hook (after-init . global-mise-mode))

;; Emacsでプロジェクト管理を行う
(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-mode +1))

;; ディレクトリ・ファイルツリーを表示する
(use-package treemacs
  :bind ([f8] . treemacs)
  :custom (treemacs-position 'right)  ;; フレーム右側に表示する
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1))))

;; treemacsとmagitを統合する
(use-package treemacs-magit
  :after (treemacs magit))

;; treemacsでnerd-iconsを利用する
(use-package treemacs-nerd-icons
  :after (nerd-icons treemacs)
  :custom (treemacs-load-theme "nerd-icons"))

;; treemacsとprojectileを統合する
(use-package treemacs-projectile
  :after (projectile treemacs))

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
  :vc (:fetcher github :repo k1LoW/emacs-drill-instructor))

;; 同一キーの入力で入力内容を切り替える
(use-package smartchr
  :vc (:fetcher github :repo imakado/emacs-smartchr))

;; prefixキーの次の操作をナビゲーションする
(use-package which-key
  :config (which-key-mode 1))

;;
;; 日本語入力設定
;;

;; 日本語入力にDDSKKを利用する
(use-package ddskk
  :bind ("C-SPC" . skk-mode)
  :custom
  ((skk-auto-insert-paren t)                                                       ;; 全角のカッコを自動補完する
   (skk-cursor-hiragana-color (cdr (assoc 'green catppuccin-mocha-colors)))        ;; ひらがなモード
   (skk-cursor-jisx0208-latin-color (cdr (assoc 'mauve catppuccin-mocha-colors)))  ;; 全角英数モード
   (skk-cursor-katakana-color (cdr (assoc 'red catppuccin-mocha-colors)))          ;; カタカナモード
   (skk-egg-like-newline t)                                                        ;; Enterキーでも入力を確定する
   (skk-hiragana-mode-string "[あ]")                                               ;; ひらがなモードのモードライン表示
   (skk-jisx0208-latin-mode-string "[Ａ]")                                         ;; 全角英数モードのモードライン表示
   (skk-katakana-mode-string "[ア]")                                               ;; カタカナモードのモードライン表示
   (skk-latin-mode-string "[_A]")                                                  ;; ASCIIモードのモードライン表示
   (skk-show-annotation t)                                                         ;; 変換候補に注釈を表示する
   (skk-use-color-cursor t)))                                                      ;; カーソルカラーでモードが判別できるようにする

;;
;; git設定
;;

;; ファイルの編集状況をフリンジに表示する
(use-package git-gutter
  :config (global-git-gutter-mode t)
  :custom
  ((git-gutter:modified-sign "~")   ;; 変更
   (git-gutter:added-sign "+")      ;; 追加
   (git-gutter:deleted-sign "!")))  ;; 削除

;; gitの設定ファイルメジャーモード
(use-package git-modes)

;; gitをEmacsから操作する
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c g" . magit-dispatch)
   ("C-c f" . magit-file-dispatch)))

;; magitのdiff表示にdeltaを利用する
(use-package magit-delta
  :after magit
  :custom ((magit-delta-default-dark-theme "Catppuccin Mocha")  ;; bat --list-themes
           (magit-delta-hide-plus-minus-markers nil))           ;; diffの行頭に+/-を表示する
  :hook magit-mode)

;;
;; 構文チェック設定
;;

;; Syntax checkにflymakeを利用する
(use-package flymake
  :hook prog-mode
  :bind
  (("<f12>" . flymake-goto-next-error)
   ("<f11>" . flymake-goto-prev-error))
  :ensure nil)

;; flymakeのエラーメッセージをポップアップで表示する
(use-package flymake-diagnostic-at-point
  :after flymake
  :custom (flymake-diagnostic-at-point-error-prefix "❯ ")
  :hook flymake-mode)

;;
;; 構文ハイライト設定
;;

;; tree-sitterでハイライトをより正確にする
(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (toml . ("https://github.com/ikatyang/tree-sitter-toml/"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
          (message "treesit: %s is already installed" lang)
        (message "treesit: %s is not installed" lang)
        (treesit-install-language-grammar lang))))
  :custom (treesit-font-lock-level 4)
  :ensure nil)

;;
;; プログラミング言語設定
;;

;; Elm
(use-package elm-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = " " == " ) elm-mode-map)
    (bind-key "+" (smartchr "+" " ++ " " + ") elm-mode-map)
    (bind-key "-" (smartchr "-" " - ") elm-mode-map)
    (bind-key ">" (smartchr ">" " > " " -> " " >> ") elm-mode-map)
    (bind-key "<" (smartchr "<" " < " " <- " " << ") elm-mode-map)
    (bind-key "|" (smartchr "|" "|> " "<| " " | ") elm-mode-map)
    (bind-key ":" (smartchr ":" " : " " :: ") elm-mode-map))
  :custom (elm-format-on-save t)
  :hook ((elm-mode . eglot-ensure)))

;; F#
(use-package fsharp-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = ") fsharp-mode-map)
    (bind-key "+" (smartchr "+" " + ") fsharp-mode-map)
    (bind-key "-" (smartchr "-" " - ") fsharp-mode-map)
    (bind-key ">" (smartchr ">" " > " " -> " " >> " " >=> " " >= ") fsharp-mode-map)
    (bind-key "<" (smartchr "<" "<`!!'>" " < " " <- " " << " " <= ") fsharp-mode-map)
    (bind-key ":" (smartchr ":" ": " " :: ") fsharp-mode-map)
    (bind-key "|" (smartchr "|" "|> " "<| " " | ") fsharp-mode-map))
  :hook
  ((fsharp-mode . eglot-ensure)
   (fsharp-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil 'local)))))

(use-package eglot-fsharp
  :after fsharp-mode)

;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

;; JavaScript
(use-package js2-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = " " === " " == ") js2-mode-map)
    (bind-key "+" (smartchr "+" " + " " += " "++") js2-mode-map)
    (bind-key "-" (smartchr "-" " - " " -= " "--") js2-mode-map)
    (bind-key ">" (smartchr ">" " > " " => " " >= ") js2-mode-map)
    (bind-key "<" (smartchr "<" " < " " <= ") js2-mode-map)
    (bind-key "!" (smartchr "!" " !== " " != ") js2-mode-map))
  :custom (js-indent-level 2)
  :mode "\\.js\\'")

;; PHP
(use-package php-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = " " === " " == ") php-mode-map)
    (bind-key "+" (smartchr "+" " += " "++") php-mode-map)
    (bind-key "-" (smartchr "-" " -= " "--") php-mode-map)
    (bind-key ">" (smartchr ">" "->" " => " " > " " >= ") php-mode-map)
    (bind-key "<" (smartchr "<" " <= " " =< " " <<< ") php-mode-map)
    (bind-key "!" (smartchr "!" " !== " " != ") php-mode-map))
  :hook (php-mode . php-enable-psr2-coding-style)
  :mode "\\.php\\'")

;; Python
(use-package python-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = " " == ") python-mode-map)
    (bind-key "+" (smartchr "+" " + " " += ") python-mode-map)
    (bind-key "-" (smartchr "-" " - " " -= ") python-mode-map)
    (bind-key "<" (smartchr "<" " < " " <= ") python-mode-map)
    (bind-key ">" (smartchr ">" " > " " >= " " -> ") python-mode-map)
    (bind-key "!" (smartchr "!" " != ") python-mode-map))
  :hook (python-mode . eglot-ensure)
  :mode "\\.py\\'")

;; Shell script
(use-package sh-mode
  :ensure nil
  :mode "\\.z?sh\\'" "\\.env\\'" "\\.sample\\'" "rc\\'")

;; Rust
(use-package rust-mode
  :after smartchr
  :config
  (progn
    (bind-key "=" (smartchr "=" " = " " == ") rust-mode-map)
    (bind-key "+" (smartchr "+" " + " " += ") rust-mode-map)
    (bind-key "-" (smartchr "-" " - " " -= ") rust-mode-map)
    (bind-key ">" (smartchr ">" " > " " -> " " => " " >= ") rust-mode-map)
    (bind-key "<" (smartchr "<" "<`!!'>" " < " " <- " " <= ") rust-mode-map)
    (bind-key "!" (smartchr "!" " != ") rust-mode-map)
    (bind-key "|" (smartchr "|" "|`!!'|" " || " " | ") rust-mode-map))
  :custom (rust-format-on-save t)
  :hook (rust-mode . eglot-ensure))

(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;;
;; 構造化言語設定
;;

;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :custom (js-indent-level 2))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom (markdown-command "pandoc"))

(use-package markdown-preview-mode)

;; PlantUML
(use-package plantuml-mode
  :if (file-exists-p "/usr/share/java/plantuml/plantuml.jar")
  :custom
  ((plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")  ;; pacmanで入るPlantUMLを利用する
   (plantuml-default-exec-mode 'jar)                            ;; jarを利用してレンダリングする
   (plantuml-indent-level 4))                                   ;; 4スペースインデント
  :mode "\\.p?uml\\'")

;; Terraform
(use-package terraform-mode
  :custom (terraform-format-on-save t))

;; TOML
(use-package toml-mode
  :mode ("\\.toml\\'" "^Pipfile\\'"))

;; HTML
(use-package web-mode
  :custom
  ((web-mode-code-indent-offset 2)    ;; JavaScriptは2スペースインデント
   (web-mode-css-indent-offset 2)     ;; CSSは2スペースインデント
   (web-mode-markup-indent-offset 2)  ;; HTMLは2スペースインデント
   (web-mode-script-padding 2)
   (web-mode-style-padding 2))
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'"))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;
;; 絞り込み・補完設定
;;

;; 補完コマンドを利用する
(use-package consult
  :after recentf
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
  ((corfu-cycle t)
   (corfu-auto t))
  :hook prog-mode
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
  :custom
  ((vertico-count 20)                            ;; 候補表示は20個まで
   (vertico-cycle t)                             ;; 候補の先頭・末尾を移動できるようにする
   (vertico-sort-function 'vertico-sort-alpha))  ;; 候補はアルファベット順で表示する
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
