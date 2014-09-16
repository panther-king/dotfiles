;; Scala mode settings
(when (require 'scala-mode2 nil t)
  (defun my-scala-mode-hook ()
    (setq auto-mode-alist (cons '("\\.\\(scala\\|sbt\\|scala\\.html\\)$" . scala-mode) auto-mode-alist))
    (autoload 'scala-mode "scala-mode" "Scala editing mode." t)
    (define-key scala-mode-map (kbd "=") (smartchr '(" = " " == " "=" " === ")))
    (define-key scala-mode-map (kbd "+") (smartchr '(" + " "+" " += " " ++= ")))
    (define-key scala-mode-map (kbd "-") (smartchr '(" - " "-" " -= " " --= ")))
    (define-key scala-mode-map (kbd "_") (smartchr '("_" "_=")))
    (define-key scala-mode-map (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
    (define-key scala-mode-map (kbd "!") (smartchr '("!" " != ")))
    (define-key scala-mode-map (kbd ">") (smartchr '(">" " > " " -> " " => " " >= ")))
    (define-key scala-mode-map (kbd "<") (smartchr '("<" " < " " <- " " <= "))))
  (add-hook 'scala-mode-hook 'my-scala-mode-hook))

;; ensime settings
(when (require 'ensime nil t)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))
