;; Define function to add load-path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "elpa" "public_repos")
(fset 'package-desc-vers 'package--ac-desc-version)

;; init-loader settings
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

;; Ignore display minor-mode
(setq my/hidden-minor-modes
      '(ace-isearch-mode
        eldoc-mode
        git-gutter+-mode
        global-whitespace-mode
        helm-mode
        helm-migemo-mode
        projectile-mode
        undo-tree-mode))
(mapc (lambda (mode)
        (setq minor-mode-alist
              (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
      my/hidden-minor-modes)

(require 'generic-x)
(require 'cl-lib)
