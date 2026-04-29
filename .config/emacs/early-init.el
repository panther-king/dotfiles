;;; early-init.el --- My early-init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; My early-init.el.

;;; Code:

;; GC 頻度を下げて起動を高速化
(setq gc-cons-threshold most-positive-fixnum)

;; 利用しない GUI 要素を無効化
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; 起動時の画面のチラつきを抑制
(push '(background-color . "#1e1e2e") default-frame-alist)
(push '(foreground-color . "#cdd6f4") default-frame-alist)

;; タイリング WM では WM がフレームサイズを管理するため、暗黙のリサイズを抑制する
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; 起動時に package.el を有効化しない
(setq package-enable-at-startup nil)


;; Local Variables:
;; byte-compile-warnings: (not cl-function obsolete)
;; End:

;;; early-init.el ends here.
