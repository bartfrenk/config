(require 'use-package)


;; to add file to agenda: org-agenda-file-to-front

(use-package org

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c o" . find-organizer-file)
   ("C-c t" . find-time-tracking-file)
   ("C-c c" . org-capture)
   ("C-c j" . find-journal-file))

  :config
  (use-package flycheck)

  ;; TODO: refactor
  (defun find-organizer-file ()
    (interactive)
    (find-file "~/documents/org/organizer.org"))

  (defun find-time-tracking-file ()
    (interactive)
    (find-file "~/documents/org/time-tracking.org"))

  (defun find-journal-file ()
    (interactive)
    (find-file "~/documents/org/journal.org"))

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
  ;; allow emphasis to extend over two lines
  (setcar (nthcdr 4 org-emphasis-regexp-components) 2)
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

(use-package org-capture
  :config
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline "~/documents/org/organizer.org" "Tasks")
           "* TODO %^{Task}\nFile: %F\n%?")
          ("n" "Quick note" entry
           (file+headline "~/documents/org/organizer.org" "Notes")
           "* %^{Note}")
          ("j" "Journal entry" entry
           (file+headline "~/documents/org/journal.org" "Entries")
           "* %^{Title}\nDate: %U\n\n%?"
           :unnarrowed t))))

(provide 'init-org)
