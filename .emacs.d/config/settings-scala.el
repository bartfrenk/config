(require 'use-package)
(require 'evil)
(use-package ensime
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :config
  (setq ensime-sem-high-faces
        '((implicitConversion nil)
          (implicitParams nil))))

(use-package scala-mode2
  :init
  (add-hook 'scala-mode-hook '(lambda() (setq tab-width 2
                                              evil-shift-width 2))))

(provide 'settings-scala)
