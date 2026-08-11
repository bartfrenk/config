(require 'org-capture)

(defvar gtd/dir)

(defun gtd/prune-capture-templates (key)
  (setq org-capture-templates
        (cl-remove-if
         (lambda (tpl)
           (string= (car tpl) key))
         org-capture-templates)))

(defun gtd/path (file-name)
  (concat gtd/dir "/" file-name))

(defun gtd/add-capture-templates ()
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry
                 (file ,(gtd/path "inbox.org"))
                 "* TODO %?\n %U\n\n")))

(defun gtd/inbox ()
  (interactive)
  (find-file (gtd/path "inbox.org")))

(defun gtd/projects ()
  (interactive)
  (find-file (gtd/path "projects.org")))

(defun gtd/actions ()
  (interactive)
  (find-file (gtd/path "actions.org")))

(defun gtd/set-agenda-files ()
  (setq org-agenda-files (list (gtd/path "actions.org") (gtd/path "projects.org"))))

(defun my/org-agenda-top-parent ()
  (save-excursion
    (org-back-to-heading t)
    (let ((label (if (org-up-heading-safe)
                     (progn
                       (while (org-up-heading-safe))
                       (org-get-heading t t t t)))))
      (format "%-20s" (or label "")))))


(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c %?-12t %b ")
        (todo   . " %(my/org-agenda-top-parent)")
        (tags   . " %i %-12:c %b ")
        (search . " %i %-12:c %b ")))

(defun gtd/init ()
  (gtd/prune-capture-templates "t")
  (gtd/prune-capture-templates "i")
  (gtd/add-capture-templates)
  (gtd/set-agenda-files)

  (setq org-refile-targets
        `((,(gtd/path "projects.org") :maxlevel . 3)
          (,(gtd/path "actions.org")  :maxlevel . 2)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

(gtd/init)
