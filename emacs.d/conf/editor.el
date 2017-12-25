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

;; 環境ごとに現在行のハイライト方法を切り替える
(cond ((eq window-system 'x)
       (global-hl-line-mode t))
      ((eq window-system 'mac)
       (require 'hl-line)
       (defvar global-hl-line-timer-exclude-modes '())
       (defun global-hl-line-timer-function ()
         (unless (memq major-mode global-hl-line-timer-exclude-modes)
           (global-hl-line-unhighlight-all)
           (let ((global-hl-line-mode t))
             (global-hl-line-highlight))))
       (setq global-hl-line-timer
             (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))))

;; 対応するカッコの強調
(setq show-paren-delay 0)
(show-paren-mode t)

;; 補完
(icomplete-mode 1)
(bind-key "C-j" 'dabbrev-expand)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; ソースコードの折りたたみ
(define-key global-map (kbd "C-=") 'hs-toggle-hiding)

;; 定義ジャンプ
(define-key global-map (kbd "C-M-j") 'dumb-jump-go)
(define-key global-map (kbd "C-M-p") 'dumb-jump-back)
