;; PHP settings
;; git clone git://github.com/ejmr/php-mode.git
(when (require 'php-mode nil t)
  (defun my-php-mode-hook ()
    ;; manual settings
    (setq php-search-url "http://jp.php.net/ja/")
    (setq php-manual-url "http://jp.php.net/manual/ja/")

    ;; indent settings
    ;; @see http://blog.be-open.net/emacs/php-mode%E9%85%8D%E5%88%97%E3%82%A4%E3%83%B3%E3%83%87%E3%83%B3%E3%83%88/
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (setq php-mode-force-pear nil)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)

    ;; keyboard settings
    (define-key php-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
    (define-key php-mode-map (kbd ">") (smartchr '(">" "->" " => " " >>> ")))
    (define-key php-mode-map (kbd "!") (smartchr '("!" " !== " " != ")))
    (define-key php-mode-map (kbd "(") (smartchr '("(`!!')" "(")))
    (define-key php-mode-map (kbd "[") (smartchr '("[`!!']" "[")))
    (define-key php-mode-map (kbd "{") (smartchr '("{`!!'}" "{")))

    ;; completion settings
    ;; git clone git://github.com/imakado/php-completion.git
    (when (require 'php-completion nil t)
      (php-completion-mode t)
      (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
      (when (require 'auto-complete nil t)
        (make-variable-buffer-local 'ac-sources)
        (add-to-list 'ac-sources 'ac-source-php-completion)
        (auto-complete-mode t))))

  (add-hook 'php-mode-hook
            '(lambda ()
              (add-hook 'c-special-indent-hook)))
  (add-hook 'php-mode-hook 'my-php-mode-hook))
