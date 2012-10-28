;; PHP settings
(when (require 'php-mode nil t)
  (setq php-mode-force-pear nil)
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/")
  (defun php-indent-hook ()
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0))
;               (c-set-offset 'case-label 4)
;               (c-set-offset 'arglist-intro 4)
;               (c-set-offset 'arglist-con-nonempty 4)
;               (c-set-offset 'arglist-close 0)
  (add-hook 'php-mode-hook 'php-indent-hook)
  (defun php-completion-hook ()
    (when (require 'php-completion nil t)
      (php-completion-mode t)
      (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
      (when (require 'auto-complete nil t)
        (make-variable-buffer-local 'ac-sources)
        (add-to-list 'ac-sources 'ac-source-php-completion)
        (auto-complete-mode t))))
  (add-hook 'php-mode-hook 'php-completion-hook)
  (add-hook 'php-mode-hook
            '(lambda ()
               (define-key php-mode-map (kbd "=") (smartchr '(" = " " === " "=" " == ")))
               (define-key php-mode-map (kbd ">") (smartchr '(">" "->" "=>" " >>> ")))
               (define-key php-mode-map (kbd "!") (smartchr '("!" " !== " " != "))))))