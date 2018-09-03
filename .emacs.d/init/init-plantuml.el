(require 'use-package)
(require 'evil)

(use-package flycheck-plantuml
  :pin "melpa"
  :ensure t)

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (add-to-list 'evil-emacs-state-modes 'image-mode)
  :ensure t)

(provide 'init-plantuml)
