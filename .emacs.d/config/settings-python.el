(require 'use-package)
(require 'elpy)
(require 'flycheck)

;; 'C-c C-f' shows signature in echo window
;; 'M-e' nagivates to next block

(use-package python
  :ensure t
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)
                                (elpy-mode)
                                (eldoc-mode 0)
                                (flymake-mode -1)
                                (flycheck-select-checker 'python-pylint)))

  :bind
  (:map python-mode-map
        ("C-c C-a" . py-autopep8-buffer)
        ("C-c C-," . elpy-goto-definition)))


(provide 'settings-python)
