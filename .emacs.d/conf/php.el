;; PHP
(el-get-bundle php-mode
  :features php-mode
  (with-eval-after-load-feature 'php-mode
    (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
    (defun my-php-mode-hook ()
      (define-key php-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
      (define-key php-mode-map (kbd ">") (smartchr '(">" "->" " => " " >>> ")))
      (define-key php-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
      (define-key php-mode-map (kbd "(") (smartchr '("(`!!')" "(")))
      (define-key php-mode-map (kbd "[") (smartchr '("[`!!']" "[")))
      (define-key php-mode-map (kbd "{") (smartchr '("{`!!'}" "{")))
      (define-key php-mode-map (kbd "'") (smartchr '("'`!!''" "'")))
      (define-key php-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\""))))
    (add-hook 'php-mode-hook 'my-php-mode-hook)

    ;; 配列のインデントを補正する
    ;; @see https://www.emacswiki.org/emacs/PhpMode#toc18
    (add-hook 'php-mode-hook (lambda ()
                               (defun ywb-php-lineup-arglist-intro (langelem)
                                 (save-excursion
                                   (goto-char (cdr langelem))
                                   (vector (+ (current-column) c-basic-offset))))
                               (defun ywb-php-lineup-arglist-close (langelem)
                                 (save-excursion
                                   (goto-char (cdr langelem))
                                   (vector (current-column))))
                               (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                               (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

    ;; クロージャのインデントを補正する
    ;; @see https://www.emacswiki.org/emacs/PhpMode#toc19
    (defun unindent-closure ()
      "Fix php-mode indent for closures"
      (let ((syntax (mapcar 'car c-syntactic-context)))
        (if (and (member 'arglist-cont-nonempty syntax)
                 (or
                  (member 'statement-block-intro syntax)
                  (member 'brace-list-intro syntax)
                  (member 'brace-list-close syntax)
                  (member 'block-close syntax)))
            (save-excursion
              (beginning-of-line)
              (delete-char (* (count 'arglist-cont-nonempty syntax)
                              c-basic-offset))) )))
    (add-hook 'php-mode-hook
              (lambda ()
                (add-hook 'c-special-indent-hook 'unindent-closure)))))
