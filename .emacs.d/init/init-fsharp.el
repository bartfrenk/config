(require 'use-package)

(use-package omnisharp)

(use-package fsharp-mode-indent
  :after fsharp-mode
  :ensure nil
  :config
  (setq fsharp-indent-offset 2))

(use-package fsharp-mode
  :after company
  :config
  (add-to-list 'company-backends 'company-omnisharp))


(provide 'init-fsharp)
