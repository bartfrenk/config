(require 'use-package)

(use-package python
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)
                                (auto-complete-mode -1)))
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

(defvar my-python-shell-dir-setup-code
  "import os
home = os.path.expanduser('~')
while os.path.isfile('__init__.py') and (os.getcwd() != home):
    os.chdir('..')
del os")

(defun my-python-shell-dir-setup ()
  (let ((process (get-buffer-process (current-buffer))))
    (python-shell-send-string my-python-shell-dir-setup-code process)
    (message "Setup project path")))

(add-hook 'inferior-python-mode-hook 'my-python-shell-dir-setup)

(provide 'init-python)
