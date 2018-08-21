(require 'use-package)

(use-package antlr-mode
  :ensure f
  :config
  ;; To avoid highlighting strings. This does not work too well in antlr mode.
  (add-hook 'antlr-mode-hook (lambda () (font-lock-mode -1))))


(provide 'init-antlr)

