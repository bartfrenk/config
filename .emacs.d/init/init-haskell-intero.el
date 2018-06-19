(require 'use-package)



(use-package hindent)

(defun haskell-reformat-buffer ()
    (interactive)
    (hindent-reformat-buffer))

(use-package intero
  :bind
  (:map intero-mode-map
        ("M-]" . intero-goto-definition)
        ("M-[" . xref-pop-marker-stack)
        ("C-c C-a" . haskell-reformat-buffer))
  :commands intero-global-mode)

(use-package company-ghci
  :config
  (add-to-list 'company-backends 'company-ghci))

(intero-global-mode 1)

(provide 'init-haskell-intero)
