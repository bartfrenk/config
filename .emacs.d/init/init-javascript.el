(use-package skewer-mode
  :commands skewer-setup
  :diminish skewer-mode
  :ensure t)

(require 'browse-url)
(require 'company)

(use-package js2-mode
  :bind (:map js2-mode-map
              ("M-]" . tern-find-definition)
              ("M-[" . tern-pop-find-definition)
              ("C-c C-l" . skewer-load-buffer)
              ("C-M-x" . skewer-eval-defun))
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

(use-package rjsx-mode
  :ensure t
  :config
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package prettier-js
  :ensure t
  :pin "melpa"
  :config
  (setq-default js-indent-level 2
                js2-indent-level 2
                jsx-indent-level 2
                sgml-basic-offset 2
                js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'prettier-js-mode-hook 'add-node-modules-path))

(skewer-setup)

(provide 'init-javascript)
