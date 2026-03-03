(require 'org-capture)

(defvar journal/dir)

(defun journal/file ()
  (let ((journal-name
         (concat "journal-" (format-time-string "%Y") ".org")))
    (concat journal/dir "/" journal-name)))

(defun journal/open ()
  (interactive)
  (find-file (journal/file)))

(defun journal/prune-capture-templates (key)
  (setq org-capture-templates
        (cl-remove-if
         (lambda (tpl)
           (string= (car tpl) key))
         org-capture-templates)))

(defun journal/add-capture-templates ()
  (add-to-list 'org-capture-templates
               `("j" "Journal entry" entry
                 (file journal/file)
                 "* %^{Title}\nDate: %U\n\n%?")))

(defun journal/init (&optional dir)
  (setq journal/dir dir)
  (journal/prune-capture-templates "j")
  (journal/add-capture-templates))

(journal/init)
