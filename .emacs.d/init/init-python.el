(require 'use-package)
(require 'projectile)
(require 'helm-buffers)
(require 'helm-dash)
(require 'flycheck)
(require 'functions-python)

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
            path (string-remove-suffix (concat "/" (car segments)) path ) segments (cdr segments)))
    (string-join package ".")))

(defun python-shell-import-package ()
  "Imports the symbols of the current package into the running Python shell."
  (interactive)
  (let* ((process (get-buffer-process (current-buffer)))
         (package-name  (python--package-name buffer-file-name))
         (snippet (concat "from " package-name " import *")))
    (message (concat "Importing package " package-name))
    (python-shell-send-string snippet process)))

(defvar python-shell-set-up-project-dirs-snippet "
import sys
import os
home = os.path.expanduser('~')
while os.path.isfile('__init__.py') and (os.getcwd() != home):
    os.chdir('..')
sys.path.append(os.getcwd())
if os.path.basename(os.getcwd()) in ['src', 'test']:
    os.chdir('..')
del os
del sys"
  "Python script to run immediately after starting the REPL.")

(defun python-shell-set-up-project-dirs ()
  "Configures the current Python shell to work well with multi-package projects.

Appends the projects source directory (in src if it exist,
otherwise the project base dir) to the system path, and moves the
working directory to the project base dir."

  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (message "Setting up project paths")
    (python-shell-send-string python-shell-set-up-project-dirs-snippet process)))

(defun python-format-buffer ()
  "Format a Python buffer"
  (interactive)
  (blacken-buffer)
  (py-isort-buffer))

(add-hook 'inferior-python-mode-hook 'python-shell-set-up-project-dirs)

(defun jedi:set-virtualenv ()
  "Set the virtualenv for jedi."
  (setq jedi:server-args (list "--virtual-env" venv-current-dir)))


;; ==== configuration =====

(defun python/setup-dash ()
  (setq-local helm-dash-docsets '("Airflow" "Python_3" "SQLAlchemy")))

(use-package py-isort)

(defun get-pyenv-root ()
  "Returns the pyenv directory."
  ;; Need to fall back on the default location, since PYENV_ROOT may be set
  ;; after starting the Emacs daemon.
  (or (getenv "PYENV_ROOT") (concat (getenv "HOME") "/.pyenv")))

(use-package python
  :commands python-shell-send-string
  :bind
  (:map python-mode-map
        ("C-c C-a" . python-format-buffer)
        ("C-c M-j" . run-python)
        ("C-c C-k" . python-shell-import-package))
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (when (fboundp 'auto-complete-mode)
                                  (auto-complete-mode -1))
                                (python-docstring-mode)
                                (python/setup-dash)
                                (python/set-dominating-venv)
                                (jedi:setup)
                                (jedi-mode t)
                                (blacken-mode)))
  :config
  ;; IMPROVEMENT: Here we better determine dynamically which version to run, by
  ;; adding advice to run-python
  (let ((pyenv-ipython (concat (get-pyenv-root) "/shims/ipython")))
    (when (executable-find pyenv-ipython)
      ;; CHECK: Use ipython<5 since later versions are not compatible with
      ;; inferior. It would be better to distinguish based on the output of
      ;; ipython --version.  Later version of ipython require the option
      ;; --simple-prompt, but this disables autocompletion.

      ;;
      ;;    (setq python-shell-interpreter "ipython"
      ;;          python-shell-interpreter-args "--simple-prompt --profile=dev)
      (setq python-shell-interpreter pyenv-ipython
            python-shell-interpreter-args "--profile=dev")))
  (setq python-shell-completion-native-enable t))

(use-package virtualenv)

(use-package pyenv-mode
  :commands pyenv-mode-versions)

(defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

(use-package virtualenvwrapper
  :after virtualenv
  :config
  (setq venv-location "/home/bart/.pyenv/versions")
  ;; Seems not to be required for JEDI
  ; (advice-add 'venv-workon :after 'jedi:set-virtualenv)
  )


(use-package yapfify
  :commands yapfify-buffer
  :diminish yapf-mode)

;; JEDI requires `pip install epc` among others.
;; See http://tkf.github.io/emacs-jedi/released/
(use-package company-jedi
  :commands (jedi:setup)
  :after python
  :bind
  (:map python-mode-map
        ("M-]" . jedi:goto-definition)
        ("M-[" . jedi:goto-definition-pop-marker)
        ("C-c C-d" . jedi:show-doc)
        ("C-c C-p" . nil))
  :after (company)
  :init
  (add-to-list 'company-backends 'company-jedi))


(use-package python-docstring
  :pin melpa
  :diminish python-docstring-mode)

(use-package blacken
  :pin melpa)

(use-package flycheck-mypy
  :pin melpa
  :config
  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-flake8 'python-mypy t)
  (flycheck-add-next-checker 'python-mypy 'python-pylint))


(jedi:setup)

(provide 'init-python)

;; Silence warning about assignment to free variable helm-dash-docsets.
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
