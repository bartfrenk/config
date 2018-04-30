(require 'use-package)

(use-package groovy-mode
  :mode ("Jenkinsfile" . groovy-mode)
  :config
  (add-hook 'groovy-mode-hook (lambda ()
                                (c-set-offset 'label 2))))

(provide 'init-groovy)
