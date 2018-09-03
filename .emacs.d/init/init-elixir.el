(require 'use-package)

(use-package elixir-mode
  :ensure t)

(use-package flycheck-elixir
  :pin "melpa"
  :ensure t)

(use-package alchemist
  :ensure t)

(provide 'init-elixir)
