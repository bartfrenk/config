(require 'use-package)
(require 'flycheck)
(require 'ob-python)

;; to add file to agenda: org-agenda-file-to-front

(defvar journal-dir
  "~/documents/notes/journal"
  "Directory containing journal files.")

(defvar clock-dir
  "~/documents/notes/ghg/ai-team/clock")

(defun journal-file ()
  (let ((journal-name
         (concat "journal-" (format-time-string "%Y") ".org")))
    (concat journal-dir "/" journal-name)))

(defun clock-file ()
  (let ((clock-name
         (concat "clock-" (format-time-string "%Y-%m") ".org")))
    (concat clock-dir "/" clock-name)))

(defun open-clock-file ()
  "Opens the active journal file."
  (interactive)
  (find-file (clock-file)))

(defun open-journal-file ()
  "Opens the active journal file."
  (interactive)
  (find-file (journal-file)))

(defvar inbox-file
  "~/documents/notes/inbox.org"
  "Location for the inbox file, containing unsorted notes.")

(defun open-inbox-file ()
  "Opens the inbox file, set in `inbox-file'."
  (interactive)
  (find-file inbox-file))

(use-package org

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c j" . open-journal-file))

  :config
  (defun insert-time-stamp ()
    "Inserts an inactive timestamp of the current time."
    (interactive)
    (org-time-stamp '(16) t))

  (defun org-fill-paragraph--latex-environment (&rest args)
    "Use default fill-paragraph in latex environments."
    (not (eql (org-element-type (org-element-context)) 'latex-environment)))

  (defun disable-checkers-in-org-src-block ()
    "Disable specific checkers in org source blocks."
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                             python-pylint
                                             python-pycompile)))

  :ensure t

  :pin melpa-stable

  :functions
  (org-fill-paragraph--latex-environment
   org-element-type
   org-element-context)

  :init
  (advice-add 'org-fill-paragraph :before-while
              #'org-fill-paragraph--latex-environment)
  ;; allow emphasis to extend over two lines
  (setcar (nthcdr 4 org-emphasis-regexp-components) 2)
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  (add-hook 'org-src-mode-hook
            'disable-checkers-in-org-src-block)
  (add-hook 'org-mode-hook
            (lambda ()
              (linum-mode -1)
              (flycheck-mode -1)))
  :config
  (setq org-log-done t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t
        org-edit-src-content-indentation 0
        org-list-description-max-indent 2
        org-babel-python-command "python3"
        org-startup-folded 'content
        org-outline-path-complete-in-steps nil
        org-default-notes-file inbox-file
        org-hide-leading-stars t
        org-tags-column 80
        org-use-tag-inheritance nil
        org-startup-indented t
        org-todo-keywords '((sequence
                             "TODO(t)" "WAIT(w)" "STARTED(s)"
                             "|" "DONE(d)" "POSTPONED(p)" "CANCELED(c)"))
        org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-capture-templates
        `(("t" "Task" entry
           (file+headline inbox-file "Tasks")
           "* TODO %^{Task}\nFile: %F\n%?")
          ("n" "Quick note" entry
           (file+headline inbox-file "Notes")
           "* %^{Note}")
          ("j" "Journal entry" entry
           (file+headline ,(journal-file) "Entries")
           "* %^{Title}\nDate: %U\n\n%?"
           :unnarrowed t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sql . t)
                                 (ipython . t)
                                 (shell . t)
                                 (http . t)
                                 (maxima . t)
                                 (plantuml . t)
                                 (gnuplot . t))))


(use-package gnuplot)

(use-package org-evil)

(use-package ob-ipython
  :pin "melpa"
  :config
  (setq ob-ipython-command "jupyter")
  (add-to-list 'evil-emacs-state-modes 'special-mode))

(use-package ob-http)

(provide 'init-org)
