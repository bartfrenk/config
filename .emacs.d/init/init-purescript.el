(require 'use-package)

(use-package purescript-mode
  :pin "melpa")

(use-package psc-ide
  :bind
  (:map purescript-mode-map
        ("M-]" . psc-ide-goto-definition)
        ("M-[" . xref-pop-marker-stack))
  :commands (psc-ide-mode turn-in-purescript-indentation)
  :pin "melpa"
  :init
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (turn-on-purescript-indentation)))
  :config
  ;(setq psc-ide-use-npm-bin t)
  )


(provide 'init-purescript)
