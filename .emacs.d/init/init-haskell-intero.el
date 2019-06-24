(require 'use-package)
(require 'hs-hoogle)
(require 'flycheck)

;; Useful key bindings:
;; C-u C-c C-t Insert type signature for thing at point
;; C-c TAB Prints the output of :info <thing-at-point> into a new buffer

(use-package hindent)

(defun haskell-reformat-buffer ()
    (interactive)
;    (hindent-reformat-buffer)
    (haskell-mode-stylish-buffer))

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(use-package intero
  :pin melpa
  :commands (intero-global-mode)
  :bind
  (:map intero-mode-map
        ("M-]" . intero-goto-definition)
        ("M-[" . xref-pop-marker-stack)
        ("C-c C-a" . haskell-reformat-buffer)
        ("C-C C-d" . hs-hoogle/info)
        ("C-c h" . hs-hoogle/helm)))

(use-package company-ghci
  :pin "melpa"
  :config
  (add-to-list 'company-backends 'company-ghci))

(intero-global-mode)

(provide 'init-haskell-intero)
