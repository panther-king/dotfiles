;; JavaScript settings
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (autoload 'espresso-mode "espresso")
  (defun my-js2-indent-function ()
    (interactive)
    (save-restriction
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (save-excursion (syntax-ppss (point-at-bol))))
             (offset (- (current-column) (current-indentation)))
             (indentation (espresso-proper-indentation parse-status))
             node)
        (save-excursion
          (back-to-indentation)
          (if (looking-at "case\\s-")
              (setq indentation (+ indentation (/ espresso-indent-level 2))))
          (setq node (js2-node-at-point))
          (when (and node
                     (= js2-NAME (js2-node-type node))
                     (= js2-VAR (js2-node-type (js2-node-parent node))))
            (setq indentation (+ 4 indentation))))
        (indent-line-to indentation)
        (when (> offset 0) (forward-char offset)))))
  (defun my-indent-sexp()
    (interactive)
    (save-restriction
      (save-excursion
        (widen)
        (let* ((inhibit-point-motion-hooks t)
               (parse-status (syntax-ppss (point)))
               (beg (nth 1 parse-status))
               (end-marker (make-marker))
               (end (progn (goto-char beg) (forward-list) (point)))
               (ovl (make-overlay beg end)))
          (set-marker end-marker end)
          (overlay-put ovl 'face 'highlight)
          (goto-char beg)
          (while (< (point) (marker-position end-marker))
            (beginning-of-line)
            (unless (looking-at "\\s-*$")
              (indent-according-to-mode))
            (forward-line))
          (run-with-timer 0.5 nil '(lambda (oval)
                                     (delete-overlay ovl)) ovl)))))
  (defun my-js2-mode-hook ()
    (require 'espresso)
    (setq espresso-indent-level 2
          indent-tabs-mode nil
          c-basic-offset 2)
    (c-toggle-auto-state 0)
    (c-toggle-hungry-state 1)
    (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
    (define-key js2-mode-map (kbd "C-M-\\")
      '(lambda ()
         (interactive)
         (insert "/* -----[ ")
         (save-excursion
           (insert " ]----- */"))
         ))
    (define-key js2-mode-map (kbd "C-m") 'newline-and-indent)
    (define-key js2-mode-map (kbd "C-M-q") 'my-indent-sexp)
    (define-key js2-mode-map (kbd "=") (smartchr '(" = " " == " " === ")))
    (if (featurep 'js2-highlight-vars)
        (js2-highlight-vars-mode))
    (message "My JS2 hook"))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))
