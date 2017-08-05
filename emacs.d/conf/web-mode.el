;; HTML
(use-package web-mode
  :mode (("\\.html$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (custom-set-faces '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
                    '(web-mode-html-tag-face ((t (:foreground "#E6B422"))))
                    '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))
                    '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))
                    '(web-mode-comment-face ((t (:foreground "#D9333F"))))
                    '(web-mode-server-comment-face ((t (:foreground "#D9333F"))))
                    '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
                    '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
                    '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00")))))
  (add-hook 'web-mode-hook
            '(lambda ()
               (hs-minor-mode 1))))
