(require 'use-package)
(require 'stack-hoogle)

;; Useful key bindings:
;; C-u C-c C-t Insert type signature for thing at point
;; C-c TAB Prints the output of :info <thing-at-point> into a new buffer

(use-package hindent)

(defun haskell-reformat-buffer ()
    (interactive)
    (hindent-reformat-buffer))

(use-package intero
  :bind
  (:map intero-mode-map
        ("M-]" . intero-goto-definition)
        ("M-[" . xref-pop-marker-stack)
        ("C-c C-a" . haskell-reformat-buffer)
        ("C-C C-d" . stack-hoogle-info))
  :commands intero-global-mode)

(use-package company-ghci
  :config
  (add-to-list 'company-backends 'company-ghci))

(intero-global-mode 1)

(provide 'init-haskell-intero)
