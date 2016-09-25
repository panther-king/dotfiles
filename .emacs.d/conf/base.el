;; 不要なUIは非表示にする
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; 行番号を表示
(global-linum-mode t)

;; モードラインに列番号も表示
(column-number-mode t)

;; カラーテーマ
(load-theme 'deeper-blue t)

;; ウインドウ透過
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; フォント
(when window-system
  (create-fontset-from-ascii-font
   "SourceCodePro-16:weight=normal:slant=normal" nil "sourcecodepro")
  (set-fontset-font "fontset-sourcecodepro"
                    'unicode
                    (font-spec :family "SourceHanSansJP" :size 16)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-sourcecodepro")))

;; フレーム設定
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))

;; yes/noではなくy/nで回答
(fset 'yes-or-no-p 'y-or-n-p)

;; バッファのデフォルトメジャーモード
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 警告音を鳴らさない
(setq ring-bell-function 'ignore)

;; 外部での変更を自動的に読み込む
(global-auto-revert-mode 1)
