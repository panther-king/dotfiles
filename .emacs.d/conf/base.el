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
(progn
  (set-default-font "RictyDiminishedDiscord-17")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208 '("RictyDiminishedDiscord" . "unicode-bmp")))

;; フレーム設定
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))

;; yes/noではなくy/nで回答
(fset 'yes-or-no-p 'y-or-n-p)

;; バッファのデフォルトメジャーモード
(setq default-major-mode 'lisp-interaction-mode)

;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 警告音を鳴らさない
(setq ring-bell-function 'ignore)

;; 外部での変更を自動的に読み込む
(global-auto-revert-mode 1)
