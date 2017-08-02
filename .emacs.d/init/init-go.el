(require 'use-package)
(require 'whitespace)

(use-package go-mode
  :bind (:map go-mode-map
              ("M-]" . godef-jump))
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face lines-tail trailing)))))

(use-package gorepl-mode
  :ensure t)

(use-package go-projectile
  :ensure t)

(use-package go-snippets
  :ensure t)

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(provide 'init-go)
