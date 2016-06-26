(require 'use-package)

(use-package python
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)))
  :ensure t)

(use-package jedi
  :bind
  (:map python-mode-map
        ("M-]" . jedi:goto-definition)
        ("M-[" . jedi:goto-definition-pop-marker))
  :commands jedi:setup
  :ensure t)

(use-package company-jedi
  :after (company jedi)
  :init
  (use-package company)
  (add-to-list 'company-backends 'company-jedi)
  :ensure t)

(use-package py-autopep8
  :ensure t
  :bind
  (:map python-mode-map
        ("C-c C-a" . py-autopep8-buffer)))

(provide 'init-python)
