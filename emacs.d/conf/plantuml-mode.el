;; PlantUML
(use-package plantuml-mode
  :mode (("\\.\\(puml\\|uml\\)$" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))
