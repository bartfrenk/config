(require 'use-package)


(use-package org

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c o" . find-organizer-file)
   ("C-c t" . find-time-tracking-file))

  :config
  (use-package flycheck)

  (defun find-organizer-file ()
    (interactive)
    (find-file "~/documents/org/organizer.org"))

  (defun find-time-tracking-file ()
    (interactive)
    (find-file "~/documents/org/time-tracking.org"))

  (defun org-fill-paragraph--latex-environment (&rest args)
    "Use default fill-paragraph in latex environments."
    (not (eql (org-element-type (org-element-context)) 'latex-environment)))

  (defun disable-checkers-in-org-src-block ()
    "Disable specific checkers in org source blocks."
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             python-pylint
                                             python-pycompile)))

  :ensure t

  :functions
  (org-fill-paragraph--latex-environment
   org-element-type
   org-element-context)

  :init
  (advice-add 'org-fill-paragraph :before-while
            #'org-fill-paragraph--latex-environment)

  (setq org-log-done t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t
        org-edit-src-content-indentation 0
        org-startup-folded 'content
        org-completion-use-ido nil
        org-outline-path-complete-in-steps nil
        org-default-notes-file "~/documents/org/organizer.org"
        org-hide-leading-stars t
        org-todo-keywords '((sequence
                             "TODO(t)" "WAIT(w)" "STARTED(s)"
                             "|" "DONE(d)" "CANCELED(c)"))
        org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  (add-hook 'org-src-mode-hook
            'disable-checkers-in-org-src-block))

(provide 'init-org)
