(require 'use-package)
(require 'projectile)

(use-package python
  :commands python-shell-send-string
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (auto-complete-mode -1)
                                (python-docstring-mode)
                                (sphinx-doc-mode)
                                (yapf-mode)))
  :ensure t)

(use-package pyenv-mode
  :ensure t
  :commands pyenv-mode-versions
  :config)


(defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location "/home/bart/.pyenv/versions/2.7.4/envs"))

(use-package yapfify
  :ensure t
  :diminish yapf-mode)

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

(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode)

(use-package python-docstring
  :ensure t
  :diminish python-docstring-mode)

(use-package helm-pydoc
  :ensure t)

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

(provide 'init-python-pyenv)
