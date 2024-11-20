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

(custom-set-variables '(inhibit-startup-screen t)     ;; 起動画面を表示しない
                      '(initial-scratch-message nil)  ;; Scratchバッファにメッセージは表示しない
                      '(make-backup-files nil)        ;; バックアップファイルを作成しない
                      '(auto-save-default nil)        ;; 自動保存を行わない
                      '(create-lockfiles nil)         ;; ロックファイルを作成しない
                      '(ring-bell-function 'ignore)   ;; ビープ音を無効化
                      '(scroll-bar-mode nil)          ;; スクロールバーは利用しない
                      '(menu-bar-mode nil)            ;; メニューバーは利用しない
                      '(tool-bar-mode nil)            ;; ツールバーは利用しない
                      '(column-number-mode t)         ;; モードラインに列番号も表示する
                      '(indent-tabs-mode nil)         ;; タブインデントは利用しない
                      '(kill-whole-line t)            ;; C-kで行末の改行コードごと削除する
                      '(next-line-add-newlines nil)   ;; バッファの末尾で新しい行を追加しない
                      '(require-final-newline t)      ;; ファイルの末尾は改行を必須にする
                      '(vc-follow-symlinks t))        ;; 常にシンボリックリンクをたどる

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(global-set-key (kbd "C-]") 'hs-toggle-hiding)
(add-hook 'prog-mode-hook
          #'(lambda () (hs-minor-mode 1)))

;;
;; テーマ設定
;;
(require-theme 'modus-themes)
(setopt modus-themes-italic-constructs t                      ;; 斜体を強調する
        modus-themes-bold-constructs t                        ;; 特定のキーワードを太字にする
        modus-themes-mode-line '(moody borderless)            ;; モードラインをmoodyに合わせる
        modus-themes-syntax '(yellow-comments green-strings)  ;; コメントと文字列リテラルをカラーリングする
        modus-themes-paren-match '(bold intense)              ;; マッチするカッコを強調する
        modus-themes-region '(bg-only no-extend))             ;; regionを適度に強調する
(load-theme 'modus-vivendi)

;;
;; モードライン設定
;;

;; モードラインのファイル名にディレクトリ名も表示する
(use-package uniquify
  :defer t
  :custom ((uniquify-buffer-name-style 'forward)  ;; ディレクトリ名はファイル名の前に表示する
           (uniquify-min-dir-content 5)))         ;; 5階層まで表示する

;; モードラインの表示を分かりやすくする
(use-package moody
  :ensure t
  :config (progn
            (moody-replace-mode-line-buffer-identification)
            (moody-replace-vc-mode)))

;; モードラインのマイナーモード表示をシンプルにする
(use-package minions
  :ensure t
  :config (minions-mode 1)
  :custom ((minions-prominent-mode '(flymake-mode))  ;; エラー情報を可視化するため、flymakeは常に表示する
           (minions-mode-line-lighter "[+]")))       ;; minor-modeを展開するUIを変更する

;;
;; バッファ設定
;;

;; 現在行をハイライトする
(use-package hl-line
  :defer t
  :custom (global-hl-line-mode t))  ;; 常にハイライトさせる

;; 行番号を表示させる
(use-package display-line-numbers
  :defer t
  :custom ((global-display-line-numbers-mode t)    ;; 常に表示させる
           (display-line-numbers-width-start t)))  ;; 行番号の幅を最大行に合わせる

;; 特定操作の実行をハイライトする
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode))  ;; 全バッファで有効にする

;; インデントを可視化する
(use-package highlight-indent-guides
  :ensure t
  :custom ((highlight-indent-guides-auto-enabled nil)  ;; カラーはカスタム定義する
           (highlight-indent-guides-responsive 'top)   ;; 現在のインデントガイドを強調する
           (highlight-indent-guides-method 'bitmap))   ;; インデントガイドをbitmapで表示する
  :custom-face
  (highlight-indent-guides-top-character-face ((t (:foreground "#505050"))))  ;; 基本カラー(bg-mode-line-inactive)
  (highlight-indent-guides-character-face ((t (:foreground "#2d2d2d"))))      ;; 現在のインデントカラー(bg-mode-line-active)
  :hook prog-mode)

;; スペース・タブを可視化する
(use-package whitespace
  :ensure t
  :config (global-whitespace-mode t)
  :custom ((whitespace-style '(face               ;; 可視化の有効化
                               empty              ;; バッファ前後の空行を可視化
                               tab-mark           ;; タブを専用マークで表示
                               tabs               ;; タブを可視化
                               trailing))         ;; 行末の空白を可視化
           (whitespace-action '(auto-cleanup))))  ;; 保存時に余計な空白・タブを削除

;;
;; カッコ設定
;;

;; 対応するカッコを強調表示する
(use-package paren
  :ensure t
  :config (show-paren-mode t)
  :custom ((show-paren-style 'mixed)                 ;; 対応カッコが画面外なら式全体をハイライトする
           (show-paren-when-point-inside-paren t)    ;; カーソルがカッコの内側にあってもハイライトする
           (show-paren-when-point-in-periphery t)))  ;; 論理的に最も近いカッコをハイライトする

;; カッコの対応を色づけする
(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

;; カッコを自動的に補完する
(use-package smartparens
  :ensure t
  :config (require 'smartparens-config)
  :hook prog-mode)

;;
;; アイコン設定
;;

;; アイコンを利用する
(use-package nerd-icons
  :ensure t)

;; diredでもnerd-iconsのアイコンを利用する
(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

;; 便利系拡張設定
;;

;; Emacs外でファイルが変更されたら自動的に読み込み直す
(use-package autorevert
  :defer t
  :commands global-auto-revert-mode)

;; undo履歴をツリー形式で可視化する
(use-package undo-tree
  :ensure t
  :custom ((global-undo-tree-mode t)            ;; 常に有効にする
           (undo-tree-auto-save-history nil)))  ;; 履歴をファイルに保存しない

;; シェルの環境変数を引き継ぐ
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))  ;; すべての環境変数を引き継ぐ

;; 一時ファイルを作成する
(use-package open-junk-file
  :ensure t
  :custom (open-junk-file-format "/tmp/junk/%Y%m%d-%H%M%S."))  ;; 一時ファイルは/tmpに保存する

;; Emacsからroot権限でファイルを編集できるようにする
(use-package sudo-edit
  :ensure t)

;; 検索にripgrepを利用する
(use-package ripgrep
  :ensure t
  :if (file-exists-p "~/.cargo/bin/rg")
  :custom ((ripgrep-executable "~/.cargo/bin/rg")  ;; cargo経由でインストールしたripgrepを利用する
           (ripgrep-arguments '("-S"))))           ;; case-sensitiveをよしなに判断させる

;; 最近開いたファイルを保存する
(use-package recentf
  :config (recentf-mode 1)
  :custom ((recentf-max-saved-items 2000)             ;; 保存アイテム数
           (recentf-filename-handlers nil)
           (recentf-exclude '(".recentf"))))          ;; .recentfファイルは対象としない

;; ミニバッファの履歴を保存する
(use-package savehist
  :defer t
  :config (savehist-mode t))

;;
;; IDE設定
;;

;; Emacsでプロジェクト管理を行う
(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :custom (projectile-mode +1))

;; ディレクトリ・ファイルツリーを表示する
(use-package treemacs
  :ensure t
  :bind ([f8] . treemacs)
  :custom (treemacs-position 'right))  ;; フレーム右側に表示する

;; treemacsでnerd-iconsを利用する
(use-package treemacs-nerd-icons
  :ensure t
  :after (nerd-icons treemacs)
  :custom (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :ensure t
  :after (projectile treemacs))

;; EmacsにEditorConfigを認識させる
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; Emacsでmiseを利用する
(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

;;
;; キーバインド拡張設定
;;

;; 同一キーの入力で入力内容を切り替える
(use-package smartchr
  :vc (:fetcher github :repo imakado/emacs-smartchr)
  :ensure t)

;; Emacsキーバインドを強制する
(use-package drill-instructor
  :vc (:fetcher github :repo k1LoW/emacs-drill-instructor)
  :ensure t
  :config (setq drill-instructor-global t))  ;; 常にEmacsキーバインドを強制する

;; よく利用する機能を特定のキーバインドにマッピングする
(use-package bind-key
  :ensure t
  :bind (("C-h" . delete-backward-char)  ;; 1文字前を削除
         ("C-'" . set-mark-command)))    ;; リージョン選択

;; prefixキーの次の操作をナビゲーションする
(use-package which-key
  :ensure t
  :config (which-key-mode 1))

;;
;; 日本語入力設定
;;

;; 日本語入力にDDSKKを利用する
(use-package ddskk
  :ensure t
  :bind ("C-SPC" . skk-mode)
  :custom ((skk-egg-like-newline t)                       ;; Enterキーでも入力を確定する
           (skk-show-annotation t)                        ;; 変換候補に注釈を表示する
           (skk-auto-insert-paren t)                      ;; 全角のカッコを自動補完する
           (skk-latin-mode-string "[_A]")                 ;; ASCIIモードのモードライン表示
           (skk-hiragana-mode-string "[あ]")              ;; ひらがなモードのモードライン表示
           (skk-katakana-mode-string "[ア]")              ;; カタカナモードのモードライン表示
           (skk-jisx0208-latin-mode-string "[Ａ]")        ;; 全角英数モードのモードライン表示
           (skk-use-color-cursor t)                       ;; カーソルカラーでモードが判別できるようにする
           (skk-cursor-hiragana-color "#70d73f")          ;; ひらがなモード
           (skk-cursor-katakana-color "#dbbe5f")          ;; カタカナモード
           (skk-cursor-jisx0208-latin-color "#d5b1ff")))  ;; 全角英数モード

;;
;; git設定
;;

;; gitをEmacsから操作する
(use-package magit
  :ensure t
  :bind (("C-c m s" . magit-status)
         ("C-c m b" . magit-blame-addition)))

;; magitのdiff表示にdeltaを利用する
(use-package magit-delta
  :ensure t
  :after magit
  :hook magit-mode
  :custom ((magit-delta-default-dark-theme "TwoDark")   ;; bat --list-themes
           (magit-delta-hide-plus-minus-markers nil)))  ;; diffの行頭に+/-を表示する

;; ファイルの編集状況をフリンジに表示する
(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode t)
  :custom ((git-gutter:modified-sign " ")   ;; 変更
           (git-gutter-added-sign " ")      ;; 追加
           (git-gutter-deleted-sign " ")))  ;; 削除

;; gitの設定ファイルメジャーモード
(use-package git-modes
  :ensure t)

;;
;; 構文チェック設定
;;

;; Syntax checkにflymakeを利用する
(use-package flymake
  :defer t
  :hook prog-mode
  :bind (("<f12>" . flymake-goto-next-error)
         ("<f11>" . flymake-goto-prev-error)))

;; flymakeのエラーメッセージをポップアップで表示する
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook flymake-mode
  :custom (flymake-diagnostic-at-point-error-prefix "❯ "))

;;
;; 構文ハイライト設定
;;

;; tree-sitterでハイライトをより正確にする
(use-package treesit-auto
  :ensure t
  :custom ((global-treesit-auto-modes t)
           (treesit-auto-install t)
           (treesit-font-lock-level 4)))

;;
;; プログラミング言語設定
;;

;; Elm
(use-package elm-mode
  :ensure t
  :after smartchr
  :custom (elm-format-on-save t)
  :hook ((elm-mode . eglot-ensure))
         ;; (elm-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil 'local))))
  :config (progn
            (bind-key "=" (smartchr "=" " = " " == " ) elm-mode-map)
            (bind-key "+" (smartchr "+" " ++ " " + ") elm-mode-map)
            (bind-key "-" (smartchr "-" " - ") elm-mode-map)
            (bind-key ">" (smartchr ">" " > " " -> " " >> ") elm-mode-map)
            (bind-key "<" (smartchr "<" " < " " <- " " << ") elm-mode-map)
            (bind-key "|" (smartchr "|" "|> " "<| " " | ") elm-mode-map)
            (bind-key ":" (smartchr ":" " : " " :: ") elm-mode-map)))

;; JavaScript
(use-package js2-mode
  :ensure t
  :after smartchr
  :mode "\\.js\\'"
  :config (progn
            (bind-key "=" (smartchr "=" " = " " === " " == ") js2-mode-map)
            (bind-key "+" (smartchr "+" " + " " += " "++") js2-mode-map)
            (bind-key "-" (smartchr "-" " - " " -= " "--") js2-mode-map)
            (bind-key ">" (smartchr ">" " > " " => " " >= ") js2-mode-map)
            (bind-key "<" (smartchr "<" " < " " <= ") js2-mode-map)
            (bind-key "!" (smartchr "!" " !== " " != ") js2-mode-map))
  :custom (js-indent-level 2))  ;; インデントは2スペース

;; PHP
(use-package php-mode
  :ensure t
  :after smartchr
  :mode "\\.php\\'"
  :config (progn
            (bind-key "=" (smartchr "=" " = " " === " " == ") php-mode-map)
            (bind-key "+" (smartchr "+" " += " "++") php-mode-map)
            (bind-key "-" (smartchr "-" " -= " "--") php-mode-map)
            (bind-key ">" (smartchr ">" "->" " => " " > " " >= ") php-mode-map)
            (bind-key "<" (smartchr "<" " <= " " =< " " <<< ") php-mode-map)
            (bind-key "!" (smartchr "!" " !== " " != ") php-mode-map))
  :hook (php-mode . php-enable-psr2-coding-style))

;; Python
(use-package python-mode
  :ensure t
  :after smartchr
  :mode "\\.py\\'"
  :config (progn
            (bind-key "=" (smartchr "=" " = " " == ") python-mode-map)
            (bind-key "+" (smartchr "+" " + " " += ") python-mode-map)
            (bind-key "-" (smartchr "-" " - " " -= ") python-mode-map)
            (bind-key "<" (smartchr "<" " < " " <= ") python-mode-map)
            (bind-key ">" (smartchr ">" " > " " >= " " -> ") python-mode-map)
            (bind-key "!" (smartchr "!" " != ") python-mode-map))
  :hook (python-mode . eglot-ensure))

;; Shell script
(use-package sh-mode
  :defer t
  :mode "\\.z?sh\\'" "\\.env\\'" "\\.sample\\'" "rc\\'")

;; F#
(use-package fsharp-mode
  :ensure t
  :after smartchr
  :config (progn
            (bind-key "=" (smartchr "=" " = ") fsharp-mode-map)
            (bind-key "+" (smartchr "+" " + ") fsharp-mode-map)
            (bind-key "-" (smartchr "-" " - ") fsharp-mode-map)
            (bind-key ">" (smartchr ">" " > " " -> " " >> " " >=> " " >= ") fsharp-mode-map)
            (bind-key "<" (smartchr "<" "<`!!'>" " < " " <- " " << " " <= ") fsharp-mode-map)
            (bind-key ":" (smartchr ":" ": " " :: ") fsharp-mode-map)
            (bind-key "|" (smartchr "|" "|> " "<| " " | ") fsharp-mode-map))
  :hook ((fsharp-mode . eglot-ensure)
         (fsharp-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil 'local)))))

(use-package eglot-fsharp
  :ensure t
  :after fsharp-mode)

;; Rust
(use-package rust-mode
  :ensure t
  :after smartchr
  :hook (rust-mode . eglot-ensure)
  :config (progn
            (bind-key "=" (smartchr "=" " = " " == ") rust-mode-map)
            (bind-key "+" (smartchr "+" " + " " += ") rust-mode-map)
            (bind-key "-" (smartchr "-" " - " " -= ") rust-mode-map)
            (bind-key ">" (smartchr ">" " > " " -> " " => " " >= ") rust-mode-map)
            (bind-key "<" (smartchr "<" "<`!!'>" " < " " <- " " <= ") rust-mode-map)
            (bind-key "!" (smartchr "!" " != ") rust-mode-map)
            (bind-key "|" (smartchr "|" "|`!!'|" " || " " | ") rust-mode-map))
  :custom (rust-format-on-save t))  ;; 保存時にコーディングスタイルを整形する

(use-package cargo
  :ensure t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

;; Gleam
(use-package gleam-mode
  ;; treesitter/treesitter-indentをMELPAからインストールしている
  :load-path "~/src/github.com/gleam-lang/gleam-mode"
  :hook (gleam-mode . (lambda () (add-hook 'before-save-hook 'gleam-format nil 'local)))
  :config (progn
            (bind-key "=" (smartchr "=" " = " " == ") gleam-mode-map)
            (bind-key "+" (smartchr "+" " + " " += ") gleam-mode-map)
            (bind-key "-" (smartchr "-" " - " " -= ") gleam-mode-map)
            (bind-key ">" (smartchr ">" " > " " -> " " >= ") gleam-mode-map)
            (bind-key "<" (smartchr "<" "<`!!'>" " < " " <- " " <= ") gleam-mode-map)
            (bind-key "!" (smartchr "!" " != ") gleam-mode-map)
            (bind-key "|" (smartchr "|" "|> " " || ") gleam-mode-map)))

;;
;; 構造化言語設定
;;

;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; JSON
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :custom (js-indent-level 2))

;; TOML
(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" "^Pipfile\\'"))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom (markdown-command "pandoc"))

(use-package markdown-preview-mode
  :ensure t)

;; PlantUML
(use-package plantuml-mode
  :ensure t
  :if (file-exists-p "/usr/share/java/plantuml/plantuml.jar")
  :mode "\\.p?uml\\'"
  :custom ((plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")  ;; pacmanで入るPlantUMLを利用する
           (plantuml-default-exec-mode 'jar)                            ;; jarを利用してレンダリングする
           (plantuml-indent-level 4)))                                  ;; 4スペースインデント

;; HTML
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'" "\\.tsx\\'")
  :custom ((web-mode-markup-indent-offset 2)  ;; HTMLは2スペースインデント
           (web-mode-code-indent-offset 2)    ;; JavaScriptは2スペースインデント
           (web-mode-script-padding 2)
           (web-mode-css-indent-offset 2)     ;; CSSは2スペースインデント
           (web-mode-style-padding 2)))

;; Terraform
(use-package terraform-mode
  :ensure t
  :custom (terraform-format-on-save t))

;;
;; 絞り込み・補完設定
;;

;; ミニバッファで補完を利用する
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom ((vertico-cycle t)                            ;; 候補の先頭・末尾を移動できるようにする
           (vertico-sort-function 'vertico-sort-alpha)  ;; 候補はアルファベット順で表示する
           (vertico-count 20)))                         ;; 候補表示は20個まで

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("C-l" . vertico-directory-up)))  ;; C-lでディレクトリ階層を上がれるようにする

;; 補完コマンドを利用する
(use-package consult
  :after recentf
  :ensure t
  :bind (("C-;" . consult-buffer)
         ("C-o" . consult-outline)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ([remap goto-line] . consult-goto-line)))

;; 順不同の複数キーワードで補完候補を絞り込む
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

;; 補完候補の情報を表示する
(use-package marginalia
  :ensure t
  :hook after-init)

;; 補完候補に対するアクションを行えるようにする
(use-package embark
  :ensure t)

;; consultとembarkを組み合わせて利用する
(use-package embark-consult
  :ensure t
  :after (consult embark))

;; コードの補完候補をポップアップする
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :hook prog-mode
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :custom ((corfu-cycle t)
           (corfu-auto t)))

;; 補完候補にアイコンを利用する
(use-package kind-icon
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; 補完方法をカスタマイズする
(use-package cape
  :ensure t)

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not cl-function obsolete)
;; End:

;;; init.el ends here.
