(require 'use-package)

(use-package flycheck
  :commands global-flycheck-mode
  :ensure t
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit
                flycheck-check-syntax-automatically '(save mode-enabled)
                flycheck-standard-error-navigation nil
                flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(global-flycheck-mode)

(provide 'init-flycheck)
