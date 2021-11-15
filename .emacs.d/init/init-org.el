(require 'use-package)
(require 'flycheck)

;; to add file to agenda: org-agenda-file-to-front

(defvar journal-dir
  "~/documents/notes/journals"
  "Directory containing journal files.")

(defvar tasks-dir
  "/home/bart/documents/notes/greenhouse/ai-team/tasks"
  "Directory containing task files.")

(defvar clock-dir
  "~/documents/notes/greenhouse/ai-team/clock")

(defun journal-file ()
  (let ((journal-name
         (concat "journal-" (format-time-string "%Y") ".org")))
    (concat journal-dir "/" journal-name)))

(defun tasks-file ()
  (let ((file-name
         (concat "tasks-" (format-time-string "%Y-%m") ".org")))
    (concat tasks-dir "/" file-name)))

(defun clock-file ()
  (let ((clock-name
         (concat "clock-" (format-time-string "%Y-%m") ".org")))
    (concat clock-dir "/" clock-name)))

(defvar scratch-file
  "~/documents/notes/learning/etc/scratch.org")

(defvar backlog-file
  "~/documents/notes/greenhouse/ai-team/etc/backlog.org")

(defvar habits-file
  "~/documents/notes/learning/etc/habits.org")

(defun open-clock-file ()
  "Opens the active journal file."
  (interactive)
  (find-file (clock-file)))

(defun open-habits ()
  (interactive)
  (find-file habits-file))

(defun open-scratch ()
  (interactive)
  (find-file scratch-file))

(defun open-journal ()
  "Opens the active journal file."
  (interactive)
  (find-file (journal-file)))

(defun open-backlog ()
  "Opens the active journal file."
  (interactive)
  (find-file backlog-file))

(defun open-tasks-file ()
  "Opens the active tasks file'."
  (interactive)
  (find-file (tasks-file)))

(use-package ob-http)

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c j" . open-journal-file)
   ("C-c M-p" . org-latex-export-to-pdf))

  :ensure t

  :pin melpa-stable

  :functions
  (org-fill-paragraph--latex-environment
   org-element-type
   org-element-context)

  :init
  (advice-add 'org-fill-paragraph :before-while
              #'org-fill-paragraph--latex-environment)
  (setq org-agenda-files (list tasks-dir)
        org-agenda-start-on-weekday 1
        org-agenda-todo-ignore-scheduled 'all
        org-agenda-span 14)
  ;; allow emphasis to extend over two lines
  (setcar (nthcdr 4 org-emphasis-regexp-components) 4)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  (add-hook 'org-src-mode-hook
            'disable-checkers-in-org-src-block)
  (add-hook 'org-mode-hook
            (lambda ()
              (linum-mode -1)
              (flycheck-mode -1)))
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

  (setq org-log-done t
        org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t
        org-edit-src-content-indentation 0
        org-list-description-max-indent 2
        org-babel-python-command "python3"
        org-agenda-show-all-dates t
        org-startup-folded 'content
        org-outline-path-complete-in-steps nil
        org-hide-leading-stars t
        org-tags-column 80
        org-use-tag-inheritance nil
        org-startup-indented t
        org-image-actual-width nil
        org-todo-keywords '((sequence
                             "TODO(t)" "WAIT(w)" "STARTED(s)"
                             "|" "DONE(d)" "POSTPONED(p)" "CANCELED(c)"))
        org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-capture-templates
        `(("t" "Task" entry
           (file tasks-file)
           "* TODO %^{Task}\nDate: %U\n\n%?")
          ("n" "Quick note" entry
           (file+headline inbox-file "Notes")
           "* %^{Note}")
          ("j" "Journal entry" entry
           (file journal-file)
           "* %^{Title}\nDate: %U\n\n%?"
           :unnarrowed t)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sql . t)
                                 (shell . t)
                                 (http . t)
                                 (maxima . t)
                                 (plantuml . t)
                                 (gnuplot . t))))


(use-package gnuplot)

(use-package org-evil
  :after org)

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/documents/notes/personal/roam"))
  (org-roam-db-autosync-enable)
  (setq org-roam-v2-ack t)
  :after org
  :bind
  (("C-c r r" . org-roam-buffer-toggle)
   ("C-c r b" . org-roam-buffer-display-dedicated)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r c" . org-roam-capture)
   ("C-c r /" . org-roam-node-find))
  (:map org-mode-map
        ("M-[" . org-mark-ring-goto)
        ("M-]" . org-open-at-point)))

(provide 'init-org)
