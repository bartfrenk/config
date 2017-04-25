(use-package skewer-mode
  :commands skewer-setup
  :ensure t)

(use-package js2-mode
  :bind (:map js2-mode-map
              ("M-]" . js2-jump-to-definition))
  :config
  (setq js2-basic-offset 2)
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package company-tern
;;  :bind (:map js2-mode-map
              ;; does not work well yet
              ;; ("M-]" . tern-find-definition)
              ;; ("M-[" . tern-pop-find-definition))
  :commands tern-mode
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  :ensure t)

(skewer-setup)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init-javascript)
