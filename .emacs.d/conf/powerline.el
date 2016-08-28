;; モードラインのリッチ化
(el-get-bundle powerline
  :features powerline
  (defface mode-line-color-pink '((t (:background "#ed3161"))) "" :group 'powerline)
  (defface mode-line-color-light-gray '((t (:background "#999999"))) "" :group 'powerline)
  (defface mode-line-color-dark-gray '((t (:background "#555555"))) "" :group 'powerline)
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
                            (face4 'mode-line-color-light-gray)
                            (face5 'mode-line-color-dark-gray)
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw mode-line-modified face3 'l)
                                       (powerline-buffer-id face3 'l)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face3 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face3 'l))
                                       (when (and (boundp 'which-func-mode) which-func-mode)
                                         (powerline-raw which-func-format nil 'l))
                                       (powerline-raw " " face3)
                                       (funcall separator-left face3 face4)

                                       (powerline-raw "%4l" face4 'l)
                                       (powerline-raw ":" face4 'l)
                                       (powerline-raw "%3c" face4 'r)
                                       (funcall separator-left face4 face5)

                                       (powerline-major-mode face5 'l)
                                       (powerline-process face5)
                                       (powerline-minor-modes face5 'l)
                                       (powerline-narrow face5 'l)
                                       (powerline-raw " " face5)
                                       (funcall separator-left face5 face2)

                                       (powerline-vc face2 'r)))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)

                                       ;; (unless window-system
                                       ;;   (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       ;; (funcall separator-right face1 mode-line)

                                       ;(powerline-raw " ")
                                       (powerline-raw "%6p" mode-line 'r)
                                       (when powerline-display-hud
                                         (powerline-hud face2 face1)))))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (with-eval-after-load-feature 'powerline
    (powerline-my-theme)))
