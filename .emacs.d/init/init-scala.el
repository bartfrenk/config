
(use-package scala-mode
  :init (add-hook 'scala-mode-hook (lambda ()
                                     (flycheck-mode -1)))
  :pin "melpa")

(use-package ensime
  :pin "melpa"
  :config
  (setq ensime-sem-high-faces
        '((implicitConversion nil)
          (implicitParams nil))
        ensime-implicit-gutter-icons t))

(use-package sbt-mode
  :pin "melpa")

(provide 'init-scala)
