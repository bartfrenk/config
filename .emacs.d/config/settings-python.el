(require 'use-package)

(use-package python
  :init
  (require 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-mode))

(provide 'settings-python)
