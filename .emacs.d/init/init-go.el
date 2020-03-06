(require 'use-package)
(require 'whitespace)
(require 'company)

;;;; Tips for writing comments:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html

;;;; Install external tools
;; go get -u golang.org/x/lint/golint
;; go get -u github.com/stamblerre/gocode
;; go get -v github.com/rogpeppe/godef
;; go get -v golang.org/x/tools/cmd/guru

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports)
              ("C-c h" . godoc-at-point)
              ("M-]" . godef-jump)
              ("M-[" . xref-pop-marker-stack))
  :hook
  (go-mode . (lambda ()
               (set (make-local-variable 'company-backends) '(company-go))
               (set (make-local-variable 'whitespace-style)
                    '(face lines-tail trailing))
               (if (not (string-match "go" compile-command))
                   (set (make-local-variable 'compile-command)
                        "go build -v && go test -v && go vet"))
               (setq imenu-generic-expression
                     '(("type" "^[\t]*type *\\([^ \t\n\r\f]*[\t]*\\(struct\\|interface\\)\\)" 1)
                       ("func" "^func *\\(.*\\)" 1)))))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-guru)
(use-package company-go)
(use-package yasnippet
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(provide 'init-go)
