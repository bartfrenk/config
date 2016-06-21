(require 'flycheck-pos-tip)
(require 'use-package)

(use-package flycheck
  :commands global-flycheck-mode
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-standard-error-navigation nil)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             python-flake-8
                                             python-pycompile))
  (when (display-graphic-p (selected-frame))
    (eval-after-load 'flycheck
      (progn
        #'flycheck-haskell-setup
        '(custom-set-variables
          '(flycheck-display-errors-function
            #'flycheck-pos-tip-error-messages))))))

(provide 'settings-flycheck)
