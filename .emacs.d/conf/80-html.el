;; HTML settings
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(\\(!scala\\.\\)html\\|pt\\|mak\\|blade\\.php\\)$" . web-mode))
  (setq web-mode-engines-alist
        '(("blade" . "\\.blade\\.")))
  (defun my-web-mode-hook ()
    (define-key web-mode-map (kbd "C-;") nil)
    (define-key web-mode-map (kbd "C-c C-;") 'web-mode-comment-or-uncomment)
    (define-key web-mode-map (kbd "{") (smartchr '("{}" "{" "{{`!!'}}")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
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

;; sass(scss) settings
(when (require 'scss-mode nil t)
  (defun my-scss-mode-hook ()
    (add-to-list 'auto-mode-alist '("\\.\\(scss\\|css\\)$" . scss-mode))
    (setq css-indent-offset 2)
    (setq scss-compile-at-save nil))
  (add-hook 'scss-mode-hook 'my-scss-mode-hook))
