(require 'use-package)
(require 'projectile)
(require 'helm-buffers)

; requires yapf to be installed

(defun python-format-buffer ()
  "Format a Python buffer."
  (interactive)
  (yapfify-buffer))

(use-package python
  :commands python-shell-send-string
  :bind
  (:map python-mode-map
        ("C-c C-a" . python-format-buffer)
        ("C-c M-j" . run-python))
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (auto-complete-mode -1)
                                (python-docstring-mode)
                                (sphinx-doc-mode)
                                ;(yapf-mode)
                                ))
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt"))
  :config
  (setq python-shell-completion-native-enable nil)
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
  (setq venv-location "/home/bart/.pyenv/versions"))

(use-package yapfify
  :commands yapfify-buffer
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "*yapfify.**")
  :ensure t
  :diminish yapf-mode)

(use-package jedi
  :bind
  (:map python-mode-map
        ("M-]" . jedi:goto-definition)
        ("M-[" . jedi:goto-definition-pop-marker)
        ("C-c C-d" . jedi:show-doc))
  :commands jedi:setup
  :ensure t)

(use-package company-jedi
  :after (company jedi)
  :init
  (add-to-list 'company-backends 'company-jedi)
  :ensure t)

(use-package sphinx-doc
  :ensure t
  :diminish sphinx-doc-mode)

(use-package python-docstring
  :ensure t
  :diminish python-docstring-mode)

(use-package helm-pydoc
  :ensure t)

;; (use-package flycheck-mypy
;;   :pin melpa ;; not available on melpa-stable
;;   :config
;;   (flycheck-add-next-checker 'python-pylint 'python-mypy))

(defvar set-up-python-repl
  "
import sys
import os
home = os.path.expanduser('~')
while os.path.isfile('__init__.py') and (os.getcwd() != home):
    os.chdir('..')
sys.path.append(os.getcwd())
if os.path.basename(os.getcwd()) == 'src':
    os.chdir('..')
del os
del sys
"
  "Python script to run immediately after starting the REPL.")

(defun my-python-shell-dir-setup ()
  (let ((process (get-buffer-process (current-buffer))))
    (python-shell-send-string set-up-python-repl process)
    (message "Setup project path")))

(add-hook 'inferior-python-mode-hook 'my-python-shell-dir-setup)

(provide 'init-python-pyenv)
