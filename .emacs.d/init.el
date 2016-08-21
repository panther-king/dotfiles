;; 設定をロード
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-get設定の拡張
(el-get-bundle with-eval-after-load-feature-el
  :type "github"
  :pkgname "tarao/with-eval-after-load-feature-el"
  :features with-eval-after-load-feature)

;; 設定ファイルを指定した順に読み込む
(el-get-bundle emacs-jp/init-loader
  :type "github"
  :pkgname "emacs-jp/init-loader"
  :features init-loader
  (init-loader-load "~/.emacs.d/loader"))
