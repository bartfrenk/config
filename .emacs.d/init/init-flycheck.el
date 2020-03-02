(require 'use-package)

(use-package flycheck
  :commands global-flycheck-mode
  :ensure t
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit
                flycheck-check-syntax-automatically '(save mode-enabled)
                flycheck-standard-error-navigation nil
                flycheck-checker-error-threshold nil ;; no limit to errors
                flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-global-modes '(not org-mode)))

(global-flycheck-mode)

(provide 'init-flycheck)
