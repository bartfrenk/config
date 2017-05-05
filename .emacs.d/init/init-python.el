(require 'use-package)

(use-package python
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)
                                (auto-complete-mode -1)
                                (python-docstring-mode)
                                (sphinx-doc-mode)))
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--profile=dev --simple-prompt -i")
  :ensure t)

;; work-around for Python interpreter not starting properly, fixed in Emacs 25.2.
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

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
  :ensure t)

(use-package python-docstring
  :ensure t)

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

(provide 'init-python)
