(require 'use-package)
(require 'projectile)
(require 'helm-buffers)

;; ==== python-mode utility functions ====

(defun python--package-name (path)
  "Derive the package name for the file specified by PATH."
  (let* ((segments (reverse (split-string path "/")))
         (py-file-name (car segments))
         (package (if (string= py-file-name "__init__.py")
                      nil
                      (list (file-name-base py-file-name))))
         (path (string-remove-suffix (concat "/" py-file-name) path))
         (segments (cdr segments)))
    (while (file-exists-p (concat path "/__init__.py") )
      (setq package (cons (car segments) package)
            path (string-remove-suffix (concat "/" (car segments)) path )
            segments (cdr segments)))
    (string-join package ".")))

(defun python-shell-import-package ()
  "Imports the symbols of the current package into the running Python shell."
  (interactive)
  (let* ((process (get-buffer-process (current-buffer)))
         (package-name  (python--package-name buffer-file-name))
         (snippet (concat "from " package-name " import *")))
    (message (concat "Importing package " package-name))
    (python-shell-send-string snippet process)))

(defun python-format-buffer ()
  "Format a Python buffer. Requires yapf to be available."
  (interactive)
  (yapfify-buffer))

(defvar python-shell-set-up-project-dirs-snippet "
import sys
import os
home = os.path.expanduser('~')
while os.path.isfile('__init__.py') and (os.getcwd() != home):
    os.chdir('..')
sys.path.append(os.getcwd())
if os.path.basename(os.getcwd()) == 'src':
    os.chdir('..')
del os
del sys"
  "*Python script to run immediately after starting the REPL.")

(defun python-shell-set-up-project-dirs ()
  "Configures the current Python shell to work well with multi-package projects.

Appends the projects source directory (in src if it exist,
otherwise the project base dir) to the system path, and moves the
working directory to the project base dir."

  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (message "Setting up project paths")
    (python-shell-send-string python-shell-set-up-project-dirs-snippet process)))

(add-hook 'inferior-python-mode-hook 'python-shell-set-up-project-dirs)

;; ==== configuration =====

(use-package python
  :commands python-shell-send-string
  :bind
  (:map python-mode-map
        ("C-c C-a" . python-format-buffer)
        ("C-c M-j" . run-python)
        ("C-c C-k" . python-shell-import-package))
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (auto-complete-mode -1)
                                (python-docstring-mode)
                                (sphinx-doc-mode)
                                ;(yapf-mode)
                                ))
  (when (executable-find "ipython") (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt"))
  :config
  (setq python-shell-completion-native-enable nil))

(use-package pyenv-mode
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
  :config
  (setq venv-location "/home/bart/.pyenv/versions"))

(use-package yapfify
  :commands yapfify-buffer
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "*yapfify.**")
  :diminish yapf-mode)

(use-package jedi
  :bind
  (:map python-mode-map
        ("M-]" . jedi:goto-definition)
        ("M-[" . jedi:goto-definition-pop-marker)
        ("C-c C-d" . jedi:show-doc))
  :commands jedi:setup)

(use-package company-jedi
  :after (company jedi)
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package sphinx-doc
  :diminish sphinx-doc-mode)

(use-package python-docstring
  :diminish python-docstring-mode)

(provide 'init-python-pyenv)
