(require 'use-package)

;; 'C-c C-f' shows signature in echo window
;; 'M-e' nagivates to next block

(use-package python
  :init
  (require 'anaconda-mode)
  (add-hook 'python-mode-hook (lambda ()
                                (anaconda-mode)))
  :bind
  (:map python-mode-map
        ("C-c C-a" . py-autopep8-buffer)))



(provide 'settings-python)
