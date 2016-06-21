;;; TODO: enable paredit and paredit-evil modes

(provide 'settings-clojure)

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode)
            (eldoc-mode)
            (clj-refactor-mode 1)))
