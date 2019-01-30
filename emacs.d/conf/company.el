;; コード補完
(use-package company
  :ensure t
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("C-i" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-eslect-previous))
  :custom-face (company-tooltip ((nil (:foreground "black" :background "lightgrey"))))
               (company-tooltip-common ((nil (:foreground "black" :background "lightgrey"))))
               (company-tooltip-common-selection ((nil (:foreground "white" :background "steelblue"))))
               (company-tooltip-selection ((nil (:foreground "black" :background "steelblue"))))
               (company-preview-common ((nil (:background nil :foreground "lightgrey" :underline t))))
               (company-scrollbar-fg ((nil (:background "orange"))))
               (company-scrollbar-bg ((nil (:background "grey40"))))
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t))
