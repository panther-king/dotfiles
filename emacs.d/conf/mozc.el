;; 日本語入力
(use-package mozc
  :config
  (setq default-input-method "japanese-mozc")
  (global-set-key (kbd "C-SPC") 'toggle-input-method)
  ;; 入力方式に応じてカーソル色を変更
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color "#66cc66")))
  (add-hook 'input-method-inactivate-hook
            (lambda () (set-cursor-color "#d54e53"))))

;; ポップアップ表示
(use-package mozc-popup
  :config
  (setq mozc-candidate-style 'popup))
