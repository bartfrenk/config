(use-package skewer-mode
  :commands skewer-setup
  :ensure t)

(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map
              ("M-]" . tern-find-definition)
              ("M-[" . tern-pop-find-definition))
  :config
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (prettier-js-mode))))

(use-package js2-refactor
  :ensure t)

(use-package company-tern
  :commands tern-mode
  :ensure t)

(use-package prettier-js
  :ensure t)

(skewer-setup)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init-javascript)
