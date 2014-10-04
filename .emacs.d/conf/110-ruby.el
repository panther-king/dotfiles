;; Ruby settings
(autoload 'ruby-mode "ruby-mode"
  "Ruby editing mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rabl\\|jbuilder\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Capfile\\|Gemfile\\|Guardfile\\)$" . ruby-mode))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(defun my-ruby-mode-hook()
  (setq ruby-deep-indent-paren-style nil)
  (define-key ruby-mode-map (kbd "=") (smartchr '(" = " " == " "=")))
  (define-key ruby-mode-map (kbd "!") (smartchr '("!" " != ")))
  (define-key ruby-mode-map (kbd "|") (smartchr '("|`!!'|" "||" " ||= " "|")))
  (define-key ruby-mode-map (kbd ">") (smartchr '(" > " " => " " -> " ">")))
  (define-key ruby-mode-map (kbd "{") (smartchr '("{`!!'}" "{")))
  (define-key ruby-mode-map [return] 'reindent-then-newline-and-indent))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'flycheck-mode)

(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))
(when (require 'ruby-end nil t)
  (add-hook 'ruby-mode-hook
            '(lambda ()
               (abbrev-mode 1)
               (electric-pair-mode t)
               (electric-layout-mode t))))
