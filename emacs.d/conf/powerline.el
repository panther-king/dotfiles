;; モードラインのリッチ化
(use-package powerline
  :ensure t
  :config
  (defface mode-line-color-pink '((t (:background "#ed3161"))) "" :group 'powerline)
  (defface mode-line-color-dark-gray '((t (:background "#555555"))) "" :group 'powerline)

  (defun shorten-directory (dir max-length)
    "Show up to `max-length' characters of a directory name `dir'."
    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
          (output ""))
      (when (and path (equal "" (car path)))
        (setq path (cdr path)))
      (while (and path (< (length output) (- max-length 4)))
        (setq output (concat (car path) "/" output))
        (setq path (cdr path)))
      (when path
        (setq output (concat ".../" output)))
      output))

  (defun powerline-my-theme ()
    "Setup the my mode-line."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (face3 'mode-line-color-pink)
                            (face4 'mode-line-color-dark-gray)
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw mode-line-modified face3 'l)
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format nil 'l))
                                       (powerline-raw (shorten-directory default-directory 15) face3 'l)
                                       (powerline-buffer-id face3 'l)
                                       (powerline-raw " " face3)
                                       (funcall separator-left face3 face4)

                                       (powerline-major-mode face4 'l)
                                       (powerline-process face4)
                                       (powerline-narrow face4 'l)
                                       (powerline-raw " " face4)
                                       (funcall separator-left face4 face2)

                                       (powerline-vc face2 'r)))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (powerline-raw (flycheck-mode-line-status-text) face2 'r)
                                       (funcall separator-right face2 face4)

                                       (powerline-raw "%4l:%3c" face4 'r)
                                       (funcall separator-right face4 face1)

                                       (powerline-raw " " face1)
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face1 'r))
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face1 'r))
                                       (powerline-raw " %6p" face1 'r))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
    (powerline-my-theme))
