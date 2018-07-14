(require 'use-package)
(require 'whitespace)

(use-package go-mode
  :bind (:map go-mode-map
              ("M-]" . godef-jump)
              ("M-[" . pop-tag-mark)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-z" . go-scratch))
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (go-eldoc-setup)
              (set (make-local-variable 'whitespace-style)
                   '(face lines-tail trailing))
              (if (not (string-match "go" compile-command))
                  (set (make-local-variable 'compile-command)
                       "go build -v && go test -v && go vet"))
              (setq imenu-generic-expression
                    '(("type" "^[\t]*type *\\([^ \t\n\r\f]*[\t]*\\(struct\\|interface\\)\\)" 1)
                      ("func" "^func *\\(.*\\)" 1))))))

(use-package go-scratch
  :ensure t)

(use-package go-projectile
  :ensure t)

(use-package go-snippets
  ;; not available on melpa-stable
  :pin "melpa"
  :ensure t)

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(provide 'init-go)
