;; 不要なUIは非表示にする
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; モードラインに列番号も表示
(column-number-mode t)

;; カラーテーマ
(load-theme 'madhat2r t)

;; ウインドウ透過
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 80)))

;; フォント
(defvar unicode-font-size
  (cond ((eq window-system 'x) 24)
        ((eq window-system 'mac) 18)))
(when window-system
  (create-fontset-from-ascii-font
   "Cica-18:weight=normal:slant=normal" nil "cica")
  (set-fontset-font "fontset-cica"
                    'unicode
                    (font-spec :family "Cica" :size unicode-font-size)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-cica")))

;; フレーム設定
(setq frame-title-format (format"emacs@%s : %%f" (system-name)))

;; yes/noではなくy/nで回答
(fset 'yes-or-no-p 'y-or-n-p)

;; テキストモードでの行折り返し
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 警告音を鳴らさない
(setq ring-bell-function 'ignore)

;; 外部での変更を自動的に読み込む
(global-auto-revert-mode 1)
