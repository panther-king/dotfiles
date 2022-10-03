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
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :doc "leafで必要なキーワードが利用できるようにする"
    :ensure t
    :init (leaf el-get :ensure t)
    :config (leaf-keywords-init)))

(leaf ignore-custom-el
  :doc "カスタマイズ内容をinit.el以外に記録する"
  :tag "settings"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))  ;; カスタマイズ保存ファイルを指定するがロードはしない

(leaf customize-deafult
  :doc "デフォルト挙動のカスタマイズ"
  :tag "settings"
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'default-frame-alist '(font . "HackGen 14"))
  :custom ((inhibit-startup-screen . t)    ;; 起動画面を表示しない
           (initial-scratch-message . "")  ;; scratchバッファのメッセージを表示しない
           (make-backup-files . nil)       ;; バックアップファイルを作成しない
           (auto-save-default . nil)       ;; 自動保存を行わない
           (create-lockfiles . nil)        ;; ロックファイルを作成しない
           (ring-bell-function . 'ignore)  ;; ビープ音を無効化
           (scroll-bar-mode . nil)         ;; スクロールバーは利用しない
           (menu-bar-mode . nil)           ;; メニューバーは利用しない
           (tool-bar-mode  . nil)          ;; ツールバーは利用しない
           (column-number-mode . t)        ;; モードラインに列番号も表示する
           (indent-tabs-mode . nil)        ;; タブインデントは利用しない
           (kill-whole-line .  t)          ;; C-kで行末の改行コードごと削除する
           (next-line-add-newlines . nil)  ;; バッファの末尾で新しい行を追加しない
           (require-final-newline . t)     ;; ファイルの末尾は改行を必須にする
           (vc-follow-symlinks . t)))      ;; 常にシンボリックリンクをたどる

(leaf nord-theme
  :doc "UIテーマにNordを利用する"
  :tag "theme"
  :ensure t
  :require t
  :config (load-theme 'nord t))

(leaf customize-mode-line
  :doc "モードライン設定"
  :tag "mode-ilne"
  :config
  (leaf uniquify
    :doc "モードラインのファイル名にディレクトリも表示する"
    :tag "mode-line" "built-in"
    :custom ((uniquify-buffer-name-style . 'forward)  ;; ディレクトリ名はファイル名の前に表示する
             (uniquify-min-dir-content . 3)))         ;; 3階層まで表示する
  (leaf moody
    :doc "モードラインの表示を分かりやすくする"
    :tag "mode-line"
    :ensure t
    :require t
    :config
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    :custom ((x-underline-at-descent-line . t)))  ;; フォントの描画が揃う位置にアンダーラインを引く
  (leaf minions
    :doc "モードラインのマイナーモード表示をシンプルにする"
    :tag "mode-line"
    :ensure t
    :require t
    :config (minions-mode 1)
    :custom ((minions-prominent-modes . '(flymake-mode))  ;; エラー情報を可視化するため、flymakeは常に表示する
             (minions-mode-line-lighter . "[+]"))))       ;; minor-modeを展開するUIを変更する

(leaf customize-buffer
  :doc "バッファ関連設定"
  :tag "buffer"
  :config
  (leaf hl-line
    :doc "現在行をハイライトする"
    :tag "buffer" "built-in"
    :custom ((global-hl-line-mode . t)))  ;; 常にハイライトさせる
  (leaf display-line-numbers-mode
    :doc "行番号を表示させる"
    :tag "buffer" "built-in"
    :custom ((global-display-line-numbers-mode . t)))  ;; 常に表示させる
  (leaf volatile-highlights
    :doc "特定操作の実行をハイライトする"
    :ensure t
    :custom-face (vhl/default-face '((t (:foreground "#81a1c1" :background "#5e81ac"))))
    :hook (after-init-hook . volatile-highlights-mode))
  (leaf highlight-indent-guides
    :doc "インデントを可視化する"
    :tag "buffer"
    :ensure t
    :custom ((highlight-indent-guides-method . 'bitmap))  ;; インデントガイドをbitmapで表示する
    :custom-face (hilight-indent-guides-character-face . '((t (:foreground "#3b4252"))))
    :hook prog-mode-hook)
  (leaf whitespace
    :doc "スペース・タブを可視化する"
    :tag "buffer"
    :ensure t
    :require t
    :config (global-whitespace-mode t)
    :custom ((whitespace-style . '(face                                         ;; 可視化の有効化
                                   empty                                        ;; バッファ前後の空行を可視化
                                   spaces                                       ;; 空白を可視化
                                   space-mark                                   ;; 空白文字は別の文字に置き換える
                                   tabs                                         ;; タブ文字を可視化
                                   tab-mark                                     ;; タブ文字は別の文字に置き換える
                                   trailing))                                   ;; 行末の空白を可視化
             (whitespace-space-regexp . "\\(\u0020+\\|\u3000+\\)")              ;; no-break spaceと全角スペースも対象にする
             (whitespace-tab-regexp . "\\(\u0009+\\)")                          ;; タブも対象にする
             (whitespace-display-mappings . '((space-mark ?\u0020 [?.])  ;;     ;; 半角スペースは.で可視化する
                                              (space-mark ?\u3000 [?\u25a1])    ;; 全角スペースは□で可視化する
                                              (tab-mark ?\u0009 [?\xBB ?\t])))  ;; タブはタブ記号で可視化する
             (whitespace-global-modes . '(not dired-mode))                      ;; 特定のモードでは可視化しない
             (whitespace-action . '(auto-cleanup)))                             ;; 保存時に余計な空白・タブを削除
    :custom-face ((whitespace-space . '((t (:background nil :foreground "#3b4252"))))
                  (whitespace-tab . '((t (:background nil :foreground "#d08770" :underline t)))))))

(leaf customize-paren
  :doc "カッコ関連の設定"
  :tag "paren"
  :config
  (leaf paren
    :doc "対応するカッコを強調表示する"
    :tag "paren" "built-in"
    :config (show-paren-mode t)
    :custom ((show-paren-style . 'mixed)                 ;; 対応カッコが画面外なら式全体をハイライトする
             (show-paren-when-point-inside-paren . t)    ;; カーソルがカッコの内側にあってもハイライトする
             (show-paren-when-point-in-periphery . t)))  ;; 論理的に最も近いカッコをハイライトする
  (leaf rainbow-delimiters
    :doc "カッコの対応を色づけする"
    :tag "paren"
    :ensure t
    :require cl-lib color
    :hook (prog-mode-hook . rainbow-delimiters-mode))
  (leaf smartparens
    :doc "カッコを自動的に補完する"
    :tag "paren"
    :ensure t
    :require smartparens-config
    :commands turn-on-smartparens-strict-mode
    :custom ((show-smartparens-global-mode . t))
    :hook ((prog-mode-hook . turn-on-smartparens-strict-mode))))

(leaf icon
  :doc "アイコン設定"
  :tag "icon"
  :config
  (leaf all-the-icons
    :doc "アイコンを利用する"
    :tag "icon"
    :ensure t
    :require t
    :custom ((all-the-icons-scale-factor . 1.0)))  ;; アイコンフォントは等倍で表示する
  (leaf all-the-icons-dired
    :doc "diredでもall-the-iconのアイコンを利用する"
    :ensure t
    :tag "icon"
    :init
    (defun dired-open-in-accordance-with-situation ()
      (interactive)
      (cond ((string-match "\\.\\.?$"
                           (format "%s" (thing-at-point 'filename)))
             (dired-find-alternate-file))
            ((file-directory-p (dired-get-filename))
             (dired-find-alternate-file))
            (t
             (dired-find-file))))
    :bind ((:dired-mode-map
            ("C-m" . dired-open-in-accordance-with-situation)
            ("a" . dired-find-file)))
    :config (put 'dired-find-alternate-file 'disabled nil)
    :hook (dired-mode-hook . all-the-icons-dired-mode)))

(leaf utility
  :doc "便利系拡張"
  :tag "utility"
  :config
  (leaf autorevert
    :doc "Emacs外でファイルが変更されたら自動的に読み直す"
    :tag "utility" "built-in"
    :global-minor-mode global-auto-revert-mode)
  (leaf undo-tree
    :doc "undo履歴をツリー形式で可視化する"
    :tag "utility"
    :ensure t
    :require t
    :custom ((global-undo-tree-mode . t)
             (undo-tree-auto-save-history . nil)))  ;; 履歴をファイルに保存しない
  (leaf exec-path-from-shell
    :doc "シェルの環境変数を引き継ぐ"
    :tag "utility"
    :ensure t
    :require t
    :config (exec-path-from-shell-initialize))
  (leaf open-junk-file
    :doc "一時ファイルを作成する"
    :tag "utility"
    :ensure t
    :custom ((open-junk-file-format . "/tmp/junk/%Y%m%d%H%M%S.")))  ;; 一時ファイルは/tmpに保存する
  (leaf sudo-edit
    :doc "Emacsからroot権限でファイルを編集できるようにする"
    :tag "utility"
    :ensure t
    :require t)
  (leaf ripgrep
    :doc "検索にRipgrepを利用する"
    :ensure t
    :require t
    :custom ((ripgrep-executable . "~/.cargo/bin/rg")  ;; cargo経由でインストールしたripgrepを利用する
             (ripgrep-arguments . '("-S"))))           ;; case-sensitiveをよしなに判断させる
  (leaf savehist
    :init (savehist-mode)))

(leaf ide
  :doc "EmacsをIDEライクに利用する"
  :tag "ide"
  :config
  (leaf tab-bar-mode
    :doc "フレームをタブで管理する"
    :tag "ide"
    :config (tab-bar-mode +1)
    :custom ((tab-bar-new-button-show . nil)     ;; タブに追加ボタンを表示させない
             (tab-bar-close-button-show . nil))  ;; タブに閉じるボタンを表示させない
    :custom-face ((tab-bar . '((t (:background "#4c566a" :foreground "#5e81ac"))))
                  (tab-bar-tab . '((t (:background "#81a1c1" :foreground "#d8dee9"))))
                  (tab-bar-tab-inactive . '((t (:background "#5e81ac" :foreground "#88c0d0"))))))
  (leaf projectile
    :doc "Emacsでプロジェクト管理を行う"
    :ensure t
    :require t)
  (leaf treemacs
    :doc "ディレクトリ・ファイルツリーを表示する"
    :tag "ide"
    :ensure t
    :require t
    :bind* ([f8] . 'treemacs)
    :custom ((treemacs-position . 'right)))  ;; フレーム右側に表示する
  (leaf treemacs-all-the-icons
    :doc "treemacsでall-the-iconを利用する"
    :tag "ide"
    :ensure t
    :require t
    :defun treemacs-load-theme
    :config (treemacs-load-theme "all-the-icons"))
  (leaf editorconfig
    :doc "EmacsにEditorConfigを認識させる"
    :tag "ide"
    :ensure t
    :require t
    :config (editorconfig-mode 1)))

(leaf keybind
  :doc "キーバインド関連拡張"
  :tag "key-bind"
  :config
  (leaf key-combo
    :doc ":comboの挙動が期待どおりではないので直接設定する"
    :ensure t
    :config
    (global-key-combo-mode t)
    (key-combo-define-global (kbd "C-a") '(back-to-indentation
                                           move-beginning-of-line
                                           beginning-of-buffer
                                           key-combo-return))
    (key-combo-define-global (kbd "C-e") '(move-end-of-line
                                           end-of-buffer
                                           key-combo-return)))
  (leaf drill-instructor
    :doc "Emacsキーバインドを強制する"
    :el-get k1LoW/emacs-drill-instructor
    :require t
    :custom ((drill-instructor-global . t)))  ;; 常にEmacsキーバインドを強制する
  (leaf bind-key
    :doc "よく利用する機能を特定のキーバインドにマッピングする"
    :ensure t
    :require t
    :bind (("C-h" . delete-backward-char)
           ("C-'" . set-mark-command)))
  (leaf which-key
    :doc "prefixキーの次のキー操作をナビゲーションする"
    :ensure t
    :require t
    :config (which-key-mode 1)))

(leaf japanese-input
  :doc "日本語入力"
  :tag "japanese-input"
  :config
  (leaf ddskk
    :doc "日本語入力にDDSKKを利用する"
    :ensure t
    :bind (("C-SPC" . skk-mode))
    :custom ((skk-egg-like-newline . t)                     ;; Enterキーでも入力を確定する
             (skk-use-color-cursor . t)                     ;; カーソル色でモードを判断できるようにする
             (skk-cursor-hiragana-color . "#a3be8c")        ;; ひらがなモード
             (skk-cursor-katakana-color . "#d08770")        ;; カタカナモード
             (skk-cursor-jisx0208-latin-color . "#b48ead")  ;; 全角英数モード
             (skk-cursor-latin-color . "#d8dee9"))))        ;; ASCIIモード

(leaf vcs
  :doc "git"
  :tag "vcs"
  :config
  (leaf magit
    :doc "gitをEmacsから操作する"
    :ensure t
    :init
    (defun my/magit-quit-session ()
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    :advice ((:after git-commit-commit move-to-magit-buffer)
             (:around magit-status (lambda (f &rest args)
                                     (prog2
                                         (window-configuration-to-register :magit-fullscreen)
                                         (apply f args)
                                       (delete-other-windows)))))
    :bind (("C-c m s" . magit-status)
           ("C-c m b" . magit-blame-addition)
           (:magit-status-mode-map
            ("q" . my/magit-quit-session))))
  (leaf magit-delta
    :doc "magitのdiff表示にdeltaを利用する"
    :ensure t
    :custom ((magit-delta-default-dark-theme . "Nord")     ;; Nordベースのダークテーマを利用する
             (magit-delta-hide-plus-minus-markers . nil))  ;; diffの行頭に+/-を表示する
    :hook (magit-mode-hook . magit-delta-mode))
  (leaf git-gutter
    :doc "ファイルの編集状況をフリンジに表示させる"
    :ensure t
    :require t
    :config (global-git-gutter-mode t)
    :custom ((git-gutter:modified-sign . "~")    ;; 変更あり
             (git-gutter:added-sign . "+")       ;; 追加
             (git-gutter:deleted-sign . "-"))))  ;; 削除

(leaf syntax-check
  :doc "構文チェック"
  :tag "syntax-check"
  :config
  (leaf flymake
    :doc "Synatx checkにflymakeを利用する"
    :tag "built-in"
    :bind (("<f12>" . flymake-goto-next-error)
           ("<f11>" . flymake-goto-prev-error))
    :hook (prog-mode-hook . flymake-mode))
  (leaf flymake-diagnostic-at-point
    :doc "Flymakeのエラーメッセージをポップアップで表示させる"
    :ensure t
    :after flymake
    :custom ((flymake-diagnostic-at-point-error-prefix . "❯ "))
    :hook (flymake-mode-hook . flymake-diagnostic-at-point-mode)))

(leaf lsp
  :doc "LSP設定"
  :tag "lsp"
  :config
  (leaf eglot
    :doc "EmacsからLSPを利用する"
    :tag "lsp"
    :ensure t
    :require t))

(leaf elm-mode
  :doc "Elm"
  :tag "programming" "elm"
  :ensure t
  :defvar elm-mode-map
  :combo (elm-mode-map
          ("=" . (" = " " == " "="))
          ("+" . (" ++ " " + " "+"))
          ("-" . (" - " "-"))
          (">" . (" > " " -> " " >> " ">"))
          ("<" . (" < " " <- " " << " "<"))
          (":" . (" : " " :: " ":"))
          ("|" . (" | " "|> " " <|" "|")))
  :hook ((elm-mode-hook . elm-format-on-save-mode)
         (elm-mode-hook . eglot-ensure)))

(leaf js2-mode
  :doc "EmacsでJavaScriptを編集する"
  :tag "programming" "javascript"
  :ensure t
  :mode "\\.js$"
  :defvar js2-mode-map
  :combo (js2-mode-map
          ("=" . (" = " " === " " == " "="))
          ("+" . (" + " " += " "++" "+"))
          ("-" . (" - " " -= " "--" "-"))
          ("<" . (" < " " <= " "<"))
          (">" . (" > " " => " " >= " ">"))
          ("!" . (" !== " " != " "!")))
  :custom ((js-indent-level . 2)))

(leaf php-mode
  :doc "EmacsでPHPを編集する"
  :tag "programming" "php"
  :ensure t
  :mode "\\.php$"
  :defvar php-mode-map
  :combo (php-mode-map
          ("=" . (" = " " === " " == " "="))
          ("+" . (" + " " += " "++" "+"))
          ("-" . (" - " " -= " "--" "-"))
          ("<" . (" < " " <= " " <<< " "<"))
          (">" . ("->" " => " " > " " >= " ">"))
          ("!" . (" !== " " != " "!")))
  :hook (php-mode-hook . php-enable-psr2-coding-style))

(leaf python-mode
  :doc "EmacsでPythonを編集する"
  :tag "programming" "python"
  :ensure t
  :mode "\\.py$"
  :defvar python-mode-map
  :combo (python-mode-map
          ("=" . (" = " " == " "="))
          ("+" . (" + " " += " "+"))
          ("-" . (" - " " -= " "-"))
          ("<" . (" < " " <= " "<"))
          (">" . (" -> " " > " " >= " ">")))
  :hook (python-mode-hook . eglot-ensure))

(leaf sh-mode
  :doc "EmacsからShellファイルを編集する"
  :tag "programming" "shell"
  :mode "\\.z?sh$" "\\.env$" "\\.sample$" "rc$")

(leaf fsharp-mode
  :doc "EmacsからF#を編集する"
  :tag "programming" "fsharp"
  :ensure t
  :defvar fsharp-mode-map
  :combo (fsharp-mode-map
          ("=" . (" = " "="))
          (">" . (" > " " -> " " >> " " >=> " " >= " ">"))
          ("<" . ("<`!!'>" " < " " <- " " << " " <= " "<"))
          (":" . (": " " :: " ":"))
          ("|" . ("|" "|> " "<| " " | "))
          ("`" . ("\``!!'\`" "\`\``!!'\`\`" "\`")))
  :hook (fsharp-mode-hook . eglot-ensure))

(leaf rust-mode
  :doc "EmacsでRustを編集する"
  :tag "programming" "rust"
  :ensure t
  :defvar rust-mode-map
  :combo (rust-mode-map
          ("=" . (" = " " == " "="))
          ("+" . (" + " " += " "+"))
          ("-" . (" - " " -= " "-"))
          (">" . (" > " " -> " " => " " >= " ">"))
          ("<" . ("<`!!'>" " < " " <- " " <= " "<"))
          ("!" . (" != " "!"))
          ("|" . ("|`!!'|" "||" " | " "|")))
  :custom ((rust-format-on-save . t))  ;; 保存時にコーディングスタイルを整形する
  :hook (rust-mode-hook . eglot-ensure)
  :config
  (leaf cargo
    :doc "EmacsからCargoを操作する"
    :ensure t
    :hook (rust-mode-hook . cargo-minor-mode)))

(leaf slime
  :doc "EmacsでLISPのREPLを利用する"
  :tag "programming" "lisp"
  :ensure t
  :require t
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (slime-setup '(slime-repl slime-fancy slime-banner))
  :custom ((inferior-lisp-program . "clisp")))  ;; CLISPを利用する

(leaf structured-data
  :doc "マークアップ・データ構造言語の設定"
  :tag "structured-data"
  :config
  (leaf dockerfile-mode
    :doc "EmacsでDockerfileを編集する"
    :ensure t
    :mode "Dockerfile$")
  (leaf json-mode
    :doc "EmacsでJSONを編集する"
    :ensure t
    :mode "\\.json$"
    :custom (js-indent-level . 2))
  (leaf markdown-mode
    :doc "EmacsでMarkdownを編集する"
    :ensure t
    :mode ("\\.md$" . gfm-mode))
  (leaf plantuml-mode
    :doc "EmacsでPlantUMLを編集する"
    :ensure t
    :mode "\\.p?uml$"
    :custom ((plantuml-jar-path . "/usr/share/java/plantuml/plantuml.jar")  ;; Pacmanで入るPlantUMLを利用する
             (plantuml-default-exec-mode . 'jar)                            ;; jarを利用してレンダリングする
             (plantuml-indent-level . 4)))                                  ;; インデントを変更する
  (leaf scss-mode
    :doc "EmacsからSCSSを編集する"
    :ensure t
    :mode "\\.scss$"
    :init
    (defun my-scss-mode-hook ()
      (set (make-local-variable 'css-indent-offset) 2))
    :custom ((scss-compile-at-save . nil))  ;; 保存時にコンパイルしない
    :hook (scss-mode-hook . my-scss-mode-hook))
  (leaf toml-mode
    :doc "EmacsからTOMLを編集する"
    :ensure t
    :mode "\\.toml$" "^Pipfile$")
  (leaf web-mode
    :doc "EmacsからHTML/CSSを編集する"
    :ensure t
    :mode "\\.html$" "\\.css$"
    :custom ((web-mode-markup-indent-offset . 2)
             (web-mode-code-indent-offset . 2)
             (web-mode-css-indent-offset . 2)
             (web-mode-style-padding . 2)
             (web-mode-script-padding . 2)))
  (leaf yaml-mode
    :doc "EmacsからYAMLを編集する"
    :ensure t
    :mode "\\.ya?ml$"))

(leaf completion
  :doc "補完系設定"
  :tag "completion"
  :config
  (leaf vertico
    :doc "ミニバッファで補完を利用する"
    :ensure t
    :init (vertico-mode)
    :config
    (leaf vertico-directory
      :require t
      :bind (:vertico-map
             ("C-l" . vertico-directory-up)))
    :custom ((vertico-cycle . t)     ;; 候補の先頭と末尾を移動できるようにする
             (vertico-count . 20)))  ;; 候補表示は20個まで
  (leaf consult
    :doc "補完コマンドを利用する"
    :ensure t
    :bind (("C-;" . consult-buffer)
           ("C-s" . consult-line)
           ("C-r" . consult-line)
           ([remap goto-line] . consult-goto-line)))
  (leaf orderless
    :doc "順不同の複数キーワードで補完候補を絞り込めるようにする"
    :ensure t
    :custom-face ((orderless-match-face-0 . '((t (:foreground "#ebcb8b" :weight bold))))
                  (orderless-match-face-1 . '((t (:foreground "#d08770" :weight bold))))
                  (orderless-match-face-2 . '((t (:foreground "#a3be8c" :weight bold))))
                  (orderless-match-face-3 . '((t (:foreground "#b48ead" :weight bold)))))
    :custom ((completion-styles . '(orderless))))
  (leaf marginalia
    :doc "補完候補の情報を表示する"
    :ensure t
    :hook after-init-hook)
  (leaf embark
    :doc "補完候補に対するアクションを行えるようにする"
    :ensure t
    :config
    (leaf embark-consult
      :doc "ConsultとEmbarkを組み合わせて利用する"
      :ensure t))
  (leaf corfu
    :doc "コードの補完候補をポップアップする"
    :ensure t
    :init (global-corfu-mode)
    :config
    (leaf kind-icon
      :doc "補完候補にアイコンを利用する"
      :tag "icon"
      :ensure t
      :defvar corfu-margin-formatters
      :defun kind-icon-margin-formatter
      :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
      :custom ((kind-icon-default-face . 'corfu-default)))
    :bind (:corfu-map
           ("C-n" . corfu-next)
           ("C-p" . corfu-previous))
    :custom ((corfu-cycle . t)
             (corfu-auto . t))
    :hook (prog-mode-hook . corfu-mode))
  (leaf cape
    :doc "補完方法をカスタマイズする"
    :ensure t))

(leaf leaf
  :doc "leaf関連設定"
  :tag "leaf"
  :config
  (leaf leaf-convert
    :doc "elispやuse-packageからleafへの変換を行えるようにする"
    :ensure t)
  (leaf leaf-tree
    :doc "leafの構成をサイドバーで表示する"
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))
  (leaf macrostep
    :doc "leafのマクロを展開する"
    :ensure t
    :bind (("C-c e" . macrostep-expand)))
  )

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not cl-function obsolete)
;; End:

;;; init.el ends here.
