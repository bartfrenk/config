(use-package skewer-mode
  :commands skewer-setup
  :diminish skewer-mode
  :ensure t)

(require 'browse-url)
(require 'company)

(use-package js2-mode
  :ensure t
  :bind (:map js2-mode-map
              ("M-]" . tern-find-definition)
              ("M-[" . tern-pop-find-definition))
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq js2-basic-offset 2
        browse-url-generic-program "/usr/bin/chromium-browser"
        browse-url-browser-function 'browse-url-generic)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)
                             (prettier-js-mode)
                             (js2-refactor-mode))))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode)


(use-package tern
  :commands tern-mode
  :diminish tern-mode
  :ensure t)

(use-package company-tern
  :ensure t)

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode)

(skewer-setup)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init-javascript)
