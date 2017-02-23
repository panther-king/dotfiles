;; グローバルに利用するキーバインド
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-'" 'set-mark-command)

;; タブインデントは利用しない
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 行操作設定
(setq-default kill-whole-line t)
(setq-default next-line-add-newlines nil)
(setq-default require-final-newline t)

;; 現在行のハイライトカスタマイズ
(defface hlline-face
  '((((class color) (background dark))
     (:background "gray15" t))
    (((class color) (background light))
     (:background "ForestGreen" t))
    (t
     ()))
  "*Face used by hl-line.")
(global-hl-line-mode t)

;; 対応するカッコの強調
(setq show-paren-delay 0)
(show-paren-mode t)

;; 補完
(icomplete-mode 1)
(bind-key "C-j" 'dabbrev-expand)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
