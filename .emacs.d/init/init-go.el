(require 'use-package)
(require 'whitespace)
(require 'company)

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-z" . go-scratch))
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'whitespace-style)
                   '(face lines-tail trailing))
              (if (not (string-match "go" compile-command))
                  (set (make-local-variable 'compile-command)
                       "go build -v && go test -v && go vet"))
              (setq imenu-generic-expression
                    '(("type" "^[\t]*type *\\([^ \t\n\r\f]*[\t]*\\(struct\\|interface\\)\\)" 1)
                      ("func" "^func *\\(.*\\)" 1)))
              )))

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-ui-doc-show)
              ("C-C C-q" . lsp-ui-doc-hide)
              ("M-]" . lsp-find-definition)
              ("M-[" . pop-tag-mark))
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)
  :hook (go-mode . lsp-deferred))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t
        lsp-ui-doc-enable t))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(provide 'init-go)
