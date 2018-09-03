(require 'use-package)

(use-package python
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--profile=dev -i")
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)
                                (auto-complete-mode -1)
                                (python-docstring-mode)
                                (sphinx-doc-mode)))
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
  :bind
  (:map python-mode-map
        ("C-c C-a" . py-autopep8-buffer)))

(use-package sphinx-doc
  :ensure t)

(use-package python-docstring
  :pin "melpa"
  :ensure t)

;; (use-package helm-pydoc
;;   :ensure t)

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
