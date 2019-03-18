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

;; 補完
(icomplete-mode 1)
(bind-key "C-j" 'dabbrev-expand)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ソースコードの折りたたみ
(define-key global-map (kbd "C-=") 'hs-toggle-hiding)

;; 定義ジャンプ
(define-key global-map (kbd "C-c d g") 'dumb-jump-go)
(define-key global-map (kbd "C-c d b") 'dumb-jump-back)

;; 検索時の語数とマッチ位置表示
(global-anzu-mode +1)
