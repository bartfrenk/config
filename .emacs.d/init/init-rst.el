(require 'use-package)

(defun my-rst-mode-hook ()
  (electric-indent-local-mode -1))

(add-hook 'rst-mode-hook #'my-rst-mode-hook)

(provide 'init-rst)
