;; PlantUML
(use-package plantuml-mode
  :ensure t
  :mode "\\.\\(puml\\|uml\\)$"
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))
