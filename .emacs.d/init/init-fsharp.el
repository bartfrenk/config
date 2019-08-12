(require 'use-package)

(use-package fsharp-mode-indent
  :after fsharp-mode
  :ensure nil
  :config
  (setq fsharp-indent-offset 2))

(use-package fsharp-mode)

(provide 'init-fsharp)
