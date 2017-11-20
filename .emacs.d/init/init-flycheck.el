(require 'use-package)

(use-package flycheck
  :commands global-flycheck-mode
  :ensure t
  :bind
  (:map flycheck-mode-map
        ("C-c C-n" . flycheck-next-error)
        ("C-c C-p" . flycheck-previous-error))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit
                flycheck-check-syntax-automatically '(save mode-enabled)
                flycheck-standard-error-navigation nil
                flycheck-checker-error-threshold nil ;; no limit to errors
                flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             python-flake8)))

(global-flycheck-mode)

(provide 'init-flycheck)
