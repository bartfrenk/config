(require 'use-package)
(require 'flycheck)
(require 'org-element)
(require 'ob-ipython)

(defun org-fill-paragraph--latex-environment (&rest args)
  "Use default fill-paragraph in latex environments."
  (not (eql (org-element-type (org-element-context)) 'latex-environment)))
(advice-add 'org-fill-paragraph :before-while
            #'org-fill-paragraph--latex-environment)

(defun disable-checkers-in-org-src-block ()
  "Disable specific checkers in org source blocks."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                           python-pylint
                                           python-pycompile)))

(use-package org
  :ensure t
  :bind
  (:map org-mode-map ("C-c h" . ob-ipython-inspect))
  :init
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  :config
  (require 'ob-haskell)
  (setq org-log-done t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t
        org-edit-src-content-indentation 0
        org-startup-folded 'content)
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  (add-hook 'org-src-mode-hook
            'disable-checkers-in-org-src-block))

(provide 'settings-org)
