;; HTML settings
;; (defun my-html-mode-hook ()
;;   (setq indent-tabs-mode nil)
;;   (setq tab-width 2)
;;   (define-key html-mode-map (kbd "=") (smartchr '("=")))
;;   (define-key html-mode-map (kbd "{") (smartchr '("{}" "{" "{{`!!'}}")))
;;   (add-to-list 'auto-mode-alist '("\\.\\(pt\\|mak\\)$" . html-mode)))

;; (add-hook 'html-mode-hook 'my-html-mode-hook)

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(html\\|php\\|pt\\|mak\\)$" . web-mode))
  (setq web-mode-engines-alist
        '(("blade" . "\\.blade\\.")))
  (defun my-web-mode-hook ()
    (define-key web-mode-map (kbd "C-;") nil)
    (define-key web-mode-map (kbd "C-c C-;") 'web-mode-comment-or-uncomment)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-html-tag-face
     ((t (:foreground "#E6B422"))))
   '(web-mode-html-attr-name-face
     ((t (:foreground "#C97586"))))
   '(web-mode-html-attr-value-face
     ((t (:foreground "#82AE46"))))
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))
   ))