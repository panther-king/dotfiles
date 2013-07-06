;; HTML settings
(defun my-html-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (define-key html-mode-map (kbd "=") (smartchr '("=")))
  (define-key html-mode-map (kbd "{") (smartchr '("{}" "{" "{{`!!'}}")))
  (add-to-list 'auto-mode-alist '("\\.\\(pt\\|mak\\)$" . html-mode)))

(add-hook 'html-mode-hook 'my-html-mode-hook)

;; (when (require 'web-mode nil t)
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;;   (setq web-mode-engines-alist
;;         '(("blade" . "\\.blade\\.")))
;;   (defun my-web-mode-hook ()
;;     (setq web-mode-markup-indent-offset 2)
;;     (setq web-mode-css-indent-offset 2)
;;     (setq web-mode-code-indent-offset 2))
;;   (add-hook 'web-mode-hook 'my-web-mode-hook))
