;;; init.el --- My init.el -*- lexical-bindings: t; -*-

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
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf customize-deafult
  :doc "デフォルト挙動のカスタマイズ"
  :tag "settings"
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'default-frame-alist '(font . "HackGen 14"))
  :custom (;; 起動画面を表示しない
           (inhibit-startup-screen . t)
           ;; バックアップファイルを作成しない
           (make-backup-files . nil)
           ;; 自動保存を行わない
           (auto-save-default . nil)
           ;; ロックファイルを作成しない
           (create-lockfiles . nil)
           ;; ビープ音を無効化
           (ring-bell-function . 'ignore)
           ;; スクロールバーは利用しない
           (scroll-bar-mode . nil)
           ;; メニューバーは利用しない
           (menu-bar-mode . nil)
           ;; ツールバーは利用しない
           (tool-bar-mode  . nil)
           ;; モードラインに列番号も表示する
           (column-number-mode . t)
           ;; タブインデントは利用しない
           (indent-tabs-mode . nil)
           ;; C-kで行末の改行コードごと削除する
           (kill-whole-line .  t)
           ;; バッファの末尾で新しい行を追加しない
           (next-line-add-newlines . nil)
           ;; ファイルの末尾は改行を必須にする
           (require-final-newline . t)))

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
    :custom ((uniquify-buffer-name-style . 'forward)
             (uniquify-min-dir-content . 3)))
  (leaf moody
    :doc "モードラインの表示を分かりやすくする"
    :tag "mode-line"
    :ensure t
    :require t
    :config
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    :custom ((x-underline-at-descent-line . t)))
  (leaf minions
    :doc "モードラインのマイナーモード表示をシンプルにする"
    :tag "mode-line"
    :ensure t
    :require t
    :config (minions-mode 1)
    :custom ((minions-prominent-modes . '(flymake-mode))
             (minions-mode-line-lighter . "[+]"))))

(leaf customize-buffer
  :doc "バッファ関連設定"
  :tag "buffer"
  :config
  (leaf hl-line
    :doc "現在行をハイライトする"
    :tag "buffer" "built-in"
    :custom ((global-hl-line-mode . t)))
  (leaf display-line-numbers-mode
    :doc "行番号を表示させる"
    :tag "buffer" "built-in"
    :custom ((global-display-line-numbers-mode . t)
             (display-line-numbers-width-start . t)))
  (leaf volatile-highlights
    :doc "特定操作の実行をハイライトする"
    :ensure t
    :custom-face (vhl/default-face '((nil (:foreground "#81a1c1" :background "#5e81ac"))))
    :hook (after-init-hook . volatile-highlights-mode))
  (leaf highlight-indent-guides
    :doc "インデントを可視化する"
    :tag "buffer"
    :ensure t
    :custom ((highlight-indent-guides-method . 'bitmap))
    :custom-face (hilight-indent-guides-character-face . '((t (:foreground "#3b4252"))))
    :hook prog-mode-hook)
  (leaf whitespace
    :doc "スペース・タブを可視化する"
    :tag "buffer"
    :ensure t
    :require t
    :config (global-whitespace-mode t)
    :custom ((whitespace-style . '(face empty space-mark spaces tab-mark tabs trailing))
             (whitespace-space-regexp . "\\(\u0020+\\|\u3000+\\)")
             (whitespace-tab-regexp . "\\(\u0009+\\)")
             (whitespace-display-mappings . '((space-mark ?\u0020 [?.])
                                              (space-mark ?\u3000 [?\u25a1])
                                              (tab-mark ?\u0009 [?\xBB ?\t])))
             (whitespace-action . '(auto-cleanup)))
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
    :custom ((show-paren-style . 'mixed)
             (show-paren-when-point-inside-paren . t)
             (show-paren-when-point-in-periphery . t))
    :custom-face (show-paren-match '((nil (:background "#434c5e" :foreground "#d08770")))))
  (leaf rainbow-delimiters
    :doc "カッコの対応を色づけする"
    :tag "paren"
    :ensure t
    :require cl-lib color
    :defun color-saturate-name
    :defvar rainbow-delimiters-max-face-count
    :init
    (defun rainbow-delimiters-using-stronger-colors ()
      (interactive)
      (cl-loop
       for index from 1 to rainbow-delimiters-max-face-count
       do
       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
         (cl-callf color-saturate-name (face-foreground face) 30))))
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
    :custom ((all-the-icons-scale-factor . 1.0)))
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
    :custom ((auto-revert-interval . 1))
    :global-minor-mode global-auto-revert-mode)
  (leaf undo-tree
    :doc "undo履歴をツリー形式で可視化する"
    :tag "utility"
    :ensure t
    :require t
    :custom ((global-undo-tree-mode . t)
             (undo-tree-auto-save-history . nil)))
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
    :custom ((open-junk-file-format . "/tmp/junk/%Y%m%d%H%M%S.")))
  (leaf sudo-edit
    :doc "Emacsからroot権限でファイルを編集できるようにする"
    :tag "utility"
    :ensure t
    :require t)
  (leaf ripgrep
    :doc "検索にRipgrepを利用する"
    :ensure t
    :require t
    :custom ((ripgrep-executable . "~/.cargo/bin/rg")
             (ripgrep-arguments . '("-S")))))

(leaf ide
  :doc "EmacsをIDEライクに利用する"
  :tag "ide"
  :config
  (leaf tab-bar-mode
    :doc "フレームをタブで管理する"
    :tag "ide"
    :config (tab-bar-mode +1)
    :custom ((tab-bar-new-button-show . nil)
             (tab-bar-close-button-show . nil))
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
    :custom ((treemacs-position . 'right)))
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
    :custom ((drill-instructor-global . t)))
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
    :custom ((skk-egg-like-newline . t)
             (skk-use-color-cursor . t)
             (skk-cursor-hiragana-color . "#a3be8c")
             (skk-cursor-katakana-color . "#d08770")
             (skk-cursor-jisx0201-color . "#ebcb8b")
             (skk-cursor-jisx0208-latin-color . "#b48ead")
             (skk-cursor-latin-color . "#d8dee9")
             (skk-cursor-abbrev-color . "#5e81ac"))))

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
    :custom ((magit-delta-default-dark-theme . "Nord")
             (magit-delta-hide-plus-minus-markers . nil))
    :hook (magit-mode-hook . magit-delta-mode))
  (leaf git-gutter
    :doc "ファイルの編集状況をフリンジに表示させる"
    :ensure t
    :require t
    :config (global-git-gutter-mode t)
    :custom ((git-gutter:modified-sign . "~")
             (git-gutter:added-sign . "+")
             (git-gutter:deleted-sign . "-"))))

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
  :custom ((rust-format-on-save . t))
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
  :custom ((inferior-lisp-program . "clisp")))

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
    :custom ((plantuml-jar-path . "/usr/share/java/plantuml/plantuml.jar")
             (plantuml-default-exec-mode . 'jar)
             (plantuml-indent-level . 4)))
  (leaf scss-mode
    :doc "EmacsからSCSSを編集する"
    :ensure t
    :mode "\\.scss$"
    :init
    (defun my-scss-mode-hook ()
      (set (make-local-variable 'css-indent-offset) 2))
    :custom ((scss-comple-at-save . nil))
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

(leaf ivy
  :ensure t
  :require t
  :config (ivy-mode 1)
  :bind (("C-;" . ivy-switch-buffer))
  :custom ((ivy-count-format . "%d/%d ")
           (ivy-format-function . 'ivy-format-function-arrow)
           (ivy-height . 20)
           (ivy-use-virtual-buffers . t)
           (ivy-wrap . t)))

(leaf counsel
  :ensure t
  :require t
  :config (counsel-mode 1)
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  :custom ((enable-recursive-minibuffers . t)
           (swiper-include-line-number-in-search . t)))

(leaf ivy-rich
  :ensure t
  :require t
  :config (ivy-rich-mode 1)
  :custom ((ivy-rich--display-transformers-list . '(ivy-switch-buffer
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
                                                      (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))))

(leaf counsel-projectile
  :ensure t
  :require counsel projectile)

(leaf company
  :ensure t
  :bind ((:company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-s" . company-filter-candidates)
          ("C-i" . company-complete-selection))
         (:company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-eslect-previous)))
  :custom ((company-selection-wrap-around . t))
  :hook (after-init-hook . global-company-mode))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not cl-function obsolete)
;; End:

;;; init.el ends here.
