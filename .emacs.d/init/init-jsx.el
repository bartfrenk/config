(require 'use-package)

(use-package rjsx-mode
  :ensure t
  :pin melpa-stable)

(use-package prettier-js
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (setq-default js-indent-level 2)
  (setq-default js2-indent-level 2)
  (setq-default jsx-indent-level 2)
  (setq-default sgml-basic-offset 2)
  (setq-default js2-basic-offset 2))

(use-package add-node-modules-path
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'prettier-js-mode-hook 'add-node-modules-path))


(provide 'init-jsx)
