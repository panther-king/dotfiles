;; anything settings
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t
   anything-unable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))
  (global-set-key (kbd "C-;") 'anything))

;; anything settings
;; (when (require 'anything nil t)
;;   (require 'anything)
;;   (require 'anything-config)
;;   (require 'anything-match-plugin)
;;   (setq anything-sources
;;         '(anything-c-source-buffers+
;;           anything-c-source-file-name-history
;;           anything-c-source-buffer-not-found
;;           anything-c-source-imenu
;;           ))
;;   (setq imenu-auto-rescan t)
;;   (global-set-key (kbd "C-;") 'anything))
