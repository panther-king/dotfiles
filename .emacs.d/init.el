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
        :ensure t
        :init
        (leaf hydra :ensure t)
        (leaf el-get :ensure t)
        (leaf blackout :ensure t)
        :config
        (leaf-keywords-init)))

(use-package smartchr
  :load-path "github/emacs-smartchr")

(leaf leaf
  :doc "leaf"
  :tag "leaf"
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :doc "leafのマクロを展開する"
  :tag "leaf"
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "カスタマイズ内容をinit.el以外に記録する"
  :tag "builtin"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "デフォルト挙動のカスタマイズ"
  :tag "builtin"
  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-to-list 'default-frame-alist '(font . "HackGen 14"))
  :custom '(;; バックアップファイルを作成しない
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

(leaf autorevert
  :doc "Emacs外でファイルが変更されたら自動的に読み直す"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf hl-line
  :doc "現在行をハイライトする"
  :tag "builtin" "interface"
  :custom ((global-hl-line-mode . t)))

(leaf display-line-numbers-mode
  :doc "行番号を表示させる"
  :tag "interface"
  :custom ((global-display-line-numbers-mode . t)
           (display-line-numbers-width-start . t)))

(leaf paren
  :doc "対応するカッコを強調表示する"
  :tag "builtin" "interface"
  :ensure nil
  :require t
  :init (show-paren-mode t)
  :custom ((show-paren-style . 'mixed)
           (show-paren-when-point-inside-paren . t)
           (show-paren-when-point-in-periphery . t))
  :custom-face (show-paren-match '((nil (:background "#434c5e" :foreground "#d08770")))))

(leaf delsel
  :doc "region選択中の入力でregionを削除して挿入する"
  :tag "builtin" "utility"
  :global-minor-mode delete-selection-mode)

(leaf flymake
  :doc "Synatx checkにflymakeを利用する"
  :tag "builtin" "programming"
  :bind (("<f12>" . flymake-goto-next-error)
         ("<f11>" . flymake-goto-prev-error))
  :hook (prog-mode-hook . flymake-mode))

(leaf exec-path-from-shell
  :doc "シェルの環境変数を引き継ぐ"
  :ensure t
  :require t
  :config (exec-path-from-shell-initialize))

(leaf dashboard
  :doc "Emacsの起動画面をカスタマイズ"
  :tag "interface"
  :ensure t
  :require t
  :config (dashboard-setup-startup-hook)
  :custom ((dashboard-items . '((recents . 5)
                                (projects . 5)
                                (bookmarks . 5)))
           (dashboard-set-file-icons . t)
           (dashboard-set-footer . nil)
           (dashboard-set-heading-icons . t)
           (dashboard-startup-banner . 'logo)))

(leaf all-the-icons
  :doc "アイコンを利用する"
  :tag "interface"
  :ensure t
  :require t
  :custom ((all-the-icons-scale-factor . 1.0)))

(leaf all-the-icons-dired
  :doc "diredでもall-the-iconのアイコンを利用する"
  :tag "interface"
  :ensure t
  :init (defun dired-open-in-accordance-with-situation ()
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
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(leaf bind-key
  :doc "よく利用する機能を特定のキーバインドにマッピング"
  :tag "utility"
  :ensure t
  :require t
  :bind (("C-h" . delete-backward-char)
         ("C-'" . set-mark-command)))

(leaf elscreen
  :doc "ウインドウをタブ管理できるようにする"
  :tag "interface"
  :ensure t
  :require t
  :config (elscreen-start)
  :bind (("C-z" . elscreen-prefix-key))
  :custom ((elscreen-tab-display-kill-screen . nil)
           (elscreen-tab-display-control . nil)))

(leaf highlight-indent-guides
  :doc "インデントを可視化する"
  :tag "interface"
  :ensure t
  :config (set-face-foreground 'highlight-indent-guides-character-face "#3b4252")
  :custom ((highlight-indent-guides-method 'bitmap))
  :hook prog-mode-hook)

(leaf treemacs
  :doc "ディレクトリ・ファイルツリーを表示する"
  :tag "interface"
  :ensure t
  :require t
  :bind* ([f8] . 'treemacs)
  :custom ((treemacs-position . 'right)))

(leaf treemacs-all-the-icons
  :doc "treemacsでall-the-iconを利用する"
  :tag "interface"
  :ensure t
  :require t
  :defun treemacs-load-theme
  :config (treemacs-load-theme "all-the-icons"))

(leaf open-junk-file
  :doc "一時ファイルを手軽に作成する"
  :ensure t
  :defun open-junk-file-format
  :config (open-junk-file-format "/tmp/junk/%Y%m%d%H%M%S."))

(leaf skk
  :doc "日本語入力にDDSKKを利用する"
  :bind (("C-SPC" . skk-mode))
  :custom ((skk-egg-like-newline . t)
           (skk-use-color-cursor . t)
           (skk-cursor-hiragana-color . "#d08770")
           (skk-cursor-katakana-color . "#a3be8c")
           (skk-cursor-jisx0201-color . "#b48ead")
           (skk-cursor-jisx0208-latin-color . "#ebcb8b")
           (skk-cursor-latin-color . "#d8dee9")
           (skk-cursor-abbrev-color . "#5e81ac")))

(leaf rainbow-delimiters
  :doc "カッコの対応を色づけする"
  :tag "interface"
  :ensure t
  :require cl-lib color
  :defun color-saturate-name
  :defvar rainbow-delimiters-max-face-count
  :init (defun rainbow-delimiters-using-stronger-colors ()
          (interactive)
          (cl-loop
           for index from 1 to rainbow-delimiters-max-face-count
           do
           (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
             (cl-callf color-saturate-name (face-foreground face) 30))))
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf ripgrep
  :doc "検索にRipgrepを利用する"
  :tag "utility"
  :ensure t
  :require t
  :custom ((ripgrep-executable . "~/.cargo/bin/rg")
           (ripgrep-arguments . '("-S"))))

(leaf sequential-command
  :doc "キーバインドの連続入力で特定のコマンドを実行する"
  :tag "utility"
  :ensure t
  :config
  (leaf sequential-command-config
    :bind (("C-a" . seq-home)
           ("C-e" . seq-end))))


(leaf smartparens
  :doc "カッコを自動的に補完する"
  :tag "utility"
  :ensure t
  :require smartparens-config
  :commands turn-on-smartparens-strict-mode
  :custom ((show-smartparens-global-mode . t))
  :hook ((prog-mode-hook . turn-on-smartparens-strict-mode)))

(leaf sudo-edit
  :doc "Emacsからroot権限でファイルを編集できるようにする"
  :tag "programming"
  :ensure t
  :require t)

(leaf undo-tree
  :doc "undo履歴をツリー形式で可視化する"
  :tag "utility"
  :ensure t
  :require t
  :config global-undo-tree-mode
  :custom ((undo-tree-auto-save-history . nil)))

(leaf volatile-highlights-mode
  :doc "特定操作の実行をハイライトする"
  :tag "interface"
  :ensure t
  :custom-face (vhl/default-face '((nil (:foreground "#81a1c1" :background "#5e81ac"))))
  :hook (after-init-hook . volatile-highlights-mode))

(leaf which-key
  :doc "prefixキーの次のキー操作をナビゲーションする"
  :tag "utility"
  :ensure t
  :require t
  :config (which-key-mode 1))

(leaf whitespace
  :doc "スペース・タブを可視化する"
  :tag "interface"
  :ensure t
  :require t
  :config (global-whitespace-mode t)
  :custom ((whitespace-style . '(face
                                 empty
                                 space-mark
                                 spaces
                                 tab-mark
                                 tabs
                                 trailing))
           (whitespace-space-regexp . "\\(\u0020+\\|\u3000+\\)")
           (whitespace-tab-regexp . "\\(\u0009+\\)")
           (whitespace-display-mappings . '((space-mark ?\u0020 [?.])
                                            (space-mark ?\u3000 [?\u25a1])
                                            (tab-mark ?\u0009 [?\xBB ?\t])))
           (whitespace-action . '(auto-cleanup)))
  :custom-face ((whitespace-space . '((t (:background nil :foreground "#3b4252"))))
                (whitespace-tab . '((t (:background nil :foreground "#d08770" :underline t))))))

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

(leaf projectile
  :doc "Emacsでプロジェクト管理を行う"
  :tag "programming" "utility"
  :ensure t
  :require t)

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

(leaf flymake-diagnostic-at-point
  :doc "Flymakeのエラーメッセージをポップアップで表示させる"
  :tag "interface" "programming"
  :ensure t
  :after flymake
  :hook (flymake-mode-hook . flymake-diagnostic-at-point-mode))

(leaf magit
  :doc "gitをEmacsから操作する"
  :tag "git" "utility"
  :ensure t
  :init (defun my/magit-quit-session ()
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
  :tag "git" "utility"
  :ensure t
  :require magit
  :custom ((magit-delta-default-dark-theme . "Nord")
           (magit-delta-hide-plus-minus-markers . nil))
  :hook (magit-mode-hook . magit-delta-mode))

(leaf git-gutter
  :doc "ファイルの編集状況をフリンジに表示させる"
  :tag "git" "interface" "utility"
  :ensure t
  :require t
  :config (global-git-gutter-mode t)
  :custom ((git-gutter:modified-sign . "~")
           (git-gutter:added-sign . "+")
           (git-gutter:deleted-sign . "-")))

(leaf eglot
  :doc "EmacsからLSPを利用する"
  :tag "programming" "utility"
  :ensure t
  :require t)

(leaf editorconfig
  :doc "EmacsにEditorConfigを認識させる"
  :tag "programming" "utility"
  :ensure t
  :require t
  :config (editorconfig-mode 1))

(leaf dockerfile-mode
  :doc "EmacsでDockerfileを編集する"
  :tag "docker" "programming"
  :ensure t
  :mode "Dockerfile$")

(leaf elm-mode
  :doc "EmacsでElmを編集する"
  :tag "Elm" "programming"
  :ensure t
  :after company
  :defvar company-backends
  :config (add-to-list 'company-backends 'company-elm)
  :bind ((:elm-mode-map
          ("=" . (smartchr '(" = " " == " "=")))
          (">" . (smartchr '(" > " " -> " " >> " ">")))
          ("<" . (smartchr '(" < " " <- " " << " "<")))
          ("+" . (smartchr '(" ++ " " + " "+")))
          (":" . (smartchr '(" : " " :: " ":")))
          ("|" . (smartchr '(" | " "|> " " <|" "|")))))
  :hook ((elm-mode-hook . elm-format-on-save-mode)
         (elm-mode-hook . eglot-ensure)))

(leaf js2-mode
  :doc "EmacsでJavaScriptを編集する"
  :tag "javascript" "programming"
  :ensure t
  :mode "\\.js$"
  :bind ((:js2-mode-map
          ("=" . (smartchr '(" = " " === " " = " "=" " == ")))
          ("!" . (smartchr '("!" " !== " " != ")))
          ("+" . (smartchr '("+" "++" " += ")))
          ("-" . (smartchr '("-" "--" " -= ")))
          ("<" . (smartchr '("<" " < " " <= ")))
          (">" . (smartchr '(">" " => " " > " " >= ")))))
  :custom ((js-indent-level . 2)))

(leaf json-mode
  :doc "EmacsでJSONを編集する"
  :tag "json" "programming"
  :ensure t
  :mode "\\.json$"
  :custom (js-indent-level . 2))

(leaf markdown-mode
  :doc "EmacsでMarkdownを編集する"
  :tag "markdown"
  :ensure t
  :mode ("\\.md$" . gfm-mode))

(leaf php-mode
  :doc "EmacsでPHPを編集する"
  :tag "php" "programming"
  :ensure t
  :mode "\\.php$"
  :bind ((:php-mode-map
          ("=" . (smartchr '(" = " " === " "=" " == ")))
          ("!" . (smartchr '("!" " !== " " != ")))
          ("<" . (smartchr '("<" " < " " <= " " <<< ")))
          (">" . (smartchr '(">" "->" " => ")))))
  :hook (php-mode-hook . php-enable-psr2-coding-style))

(leaf plantuml-mode
  :doc "EmacsでPlantUMLを編集する"
  :tag "plantuml"
  :ensure t
  :mode "\\.p?uml$"
  :custom ((plantuml-jar-path . "/usr/share/java/plantuml/plantuml.jar")
           (plantuml-default-exec-mode . 'jar)
           (plantuml-indent-level . 4)))

(leaf python-mode
  :doc "EmacsでPythonを編集する"
  :tag "programming" "python"
  :ensure t
  :mode "\\.py$"
  :bind ((:python-mode-map
          ("=" . (smartchr '(" = " " == " "=")))
          ("!" . (smartchr '("!" " != ")))
          ("-" . (smartchr '("-" " - " " -= ")))
          ("+" . (smartchr '("+" " + " " += ")))
          (">" . (smartchr '(">" " -> " " > " " >= ")))
          ("<" . (smartchr '("<" " <= ")))))
  :hook (python-mode-hook . eglot-ensure))

(leaf rust-mode
  :doc "EmacsでRustを編集する"
  :tag "programming" "rust"
  :ensure t
  :bind ((:rust-mode-map
          ("=" . (smartchr '(" = " " == " "=")))
          ("+" . (smartchr '("+" " + " " += ")))
          ("-" . (smartchr '("-" " - " " -= ")))
          ("!" . (smartchr '("!" " != ")))
          (">" . (smartchr '(">" " > " " -> " " => " " >= ")))
          ("<" . (smartchr '("<`!!'>" "<" " < " " <- " " <= ")))
          ("|" . (smartchr '("|`!!'|" "||" " | " "|")))))
  :custom ((rust-format-on-save . t))
  :hook (rust-mode-hook . eglot-ensure))

(leaf cargo
  :doc "EmacsからCargoを操作する"
  :tag "programming" "rust" "utility"
  :ensure t
  :hook (rust-mode-hook . cargo-minor-mode))

(leaf scss-mode
  :doc "EmacsからSCSSを編集する"
  :tag "css" "programming" "scss"
  :ensure t
  :mode "\\.scss$"
  :init (defun my-scss-mode-hook ()
          (set (make-local-variable 'css-indent-offset) 2))
  :custom ((scss-comple-at-save . nil))
  :hook (scss-mode-hook . my-scss-mode-hook))

(leaf sh-mode
  :doc "EmacsからShellファイルを編集する"
  :tag "builtin" "programming" "shell"
  :mode "\\.z?sh$" "\\.env$" "\\.sample$" "rc$")

(leaf toml-mode
  :doc "EmacsからTOMLを編集する"
  :tag "toml"
  :ensure t
  :mode "\\.toml$" "^Pipfile$")

(leaf web-mode
  :doc "EmacsからHTML/CSSを編集する"
  :tag "css" "html" "programming"
  :ensure t
  :mode "\\.html$" "\\.css$"
  :custom ((web-mode-markup-indent-offset . 2)
           (web-mode-code-indent-offset . 2)
           (web-mode-css-indent-offset . 2)
           (web-mode-style-padding . 2)
           (web-mode-script-padding . 2)))

(leaf yaml-mode
  :doc "EmacsからYAMLを編集する"
  :tag "yaml"
  :ensure t
  :mode "\\.ya?ml$")

(leaf fsharp-mode
  :doc "EmacsからF#を編集する"
  :tag "fsharp" "programming"
  :ensure t
  :bind ((:fsharp-mode-map
          ("=" . (smartchr '(" = " "=")))
          (">" . (smartchr '(" > " " -> " " >> " " >=> " " => " ">")))
          ("<" . (smartchr '("<`!!'>" " < " " <- " " << " " <= " "<")))
          ("+" . (smartchr '(" + " "+")))
          ("*" . (smartchr '(" * " " ** " "*")))
          (":" . (smartchr '(": " " :: " ":")))
          ("|" . (smartchr '("|" "|> " "<| " " | ")))
          ("`" . (smartchr '("\``!!'\`" "\`\``!!'\`\`" "\`")))))
  :hook (fsharp-mode-hook . eglot-ensure))

(leaf slime
  :doc "EmacsでLISPのREPLを利用する"
  :tag "lisp" "programming" "utility"
  :ensure t
  :require t
  :init (prog2
            (load (expand-file-name "~/quicklisp/slime-helper.el"))
            (slime-setup '(slime-repl slime-fancy slime-banner)))
  :custom ((inferior-lisp-program . "clisp")))

(leaf nord-theme
  :doc "UIテーマにNordを利用する"
  :tag "interface"
  :ensure t
  :require t
  :init (load-theme 'nord t))

(leaf moody
  :doc "モードラインの表示を分かりやすくする"
  :tag "interface"
  :ensure t
  :require t
  :config (prog2
              (moody-replace-mode-line-buffer-identification)
              (moody-replace-vc-mode))
  :custom ((x-underline-at-descent-line . t)))

(leaf minions
  :doc "モードラインのマイナーモード表示をシンプルにする"
  :tag "interface"
  :ensure t
  :require t
  :config (minions-mode)
  :custom ((minions-mode-line-lighter . "[+]")))

(provide 'init)

;; Local Variables:
;; End:

;;; init.el ends here.
