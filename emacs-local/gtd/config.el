(require 'org-capture)

(defvar gtd/dir)

(defun gtd--path (file)
  (concat gtd/dir "/" file))

(defun gtd/inbox ()
  (interactive)
  (find-file (gtd--path "inbox.org")))

(defun gtd/projects ()
  (interactive)
  (find-file (gtd--path "projects.org")))

(defun gtd/actions ()
  (interactive)
  (find-file (gtd--path "actions.org")))

(defun gtd/font-lock-refresh ()
  (interactive)
  (progn
    (font-lock-flush)
    (font-lock-ensure)
    (redisplay t)))

(defun gtd--set-org-todo-keyword-faces ()
  (setq
   org-modern-todo nil
   org-todo-keyword-faces
   '(("TODO"      . (:inherit success :weight bold :underline t))
     ("NEXT"      . (:inherit font-lock-keyword-face :weight bold :underline t))
     ("URGENT"    . (:inherit error :weight bold :underline t))
     ("WAITING"   . (:inherit warning :weight bold :underline t))
     ("DONE"      . (:inherit shadow :weight bold :underline t))
     ("CANCELLED" . (:inherit shadow :weight bold :underline t)))))

(defun gtd--remove-capture-templates (key)
  (setq org-capture-templates
        (cl-remove-if
         (lambda (tpl)
           (string= (car tpl) key))
         org-capture-templates)))

(defun gtd--set-capture-templates ()
  (gtd--remove-capture-templates "i")
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry
                 (file ,(gtd--path "inbox.org"))
                 "* TODO %?\n %U\n\n")))

(defun gtd--register-files ()
  (setq org-refile-targets
        `((,(gtd--path "projects.org") :maxlevel . 3)
          (,(gtd--path "actions.org")  :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-agenda-files (list (gtd--path "actions.org") (gtd--path "projects.org"))))

(defun gtd--org-agenda-project ()
  (save-excursion
    (org-back-to-heading t)
    (let ((label (if (org-up-heading-safe)
                     (progn
                       (while (org-up-heading-safe))
                       (org-get-heading t t t t)))))
      (format %-15s (or label "")))))

(defun gtd--org-agenda-scheduled-date ()
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (if scheduled
        (format "%-11s"
                (format-time-string
                 "%Y-%m-%d"
                 (org-time-string-to-time scheduled)))
      (format "%-11s" ""))))

(setq org-agenda-todo-keyword-format "%-1s")

(defun gtd--set-agenda-format ()
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c %?-12t %b ")
          (todo   . " %(gtd--org-agenda-project) %(gtd--org-agenda-scheduled-date)")
          (tags   . " %i %-12:c %b ")
          (search . " %(gtd--org-agenda-project)"))))

(gtd--set-agenda-format)

(defvar gtd--org-agenda-hide-waiting t)

(defun gtd--org-agenda-skip-waiting ()
  (when gtd--org-agenda-hide-waiting
    (org-agenda-skip-entry-if 'todo '("WAITING"))))

(setq org-agenda-skip-function-global #'gtd--org-agenda-skip-waiting)

(defun gtd--org-agenda-toggle-waiting ()
  (interactive)
  (setq gtd--org-agenda-hide-waiting
        (not gtd--org-agenda-hide-waiting))
  (org-agenda-redo)
  (let ((status (if gtd--org-agenda-hide-waiting "hidden" "shown")))
    (message "WAITING items %s" status)))

(with-eval-after-load 'org-agenda
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "w") #'gtd--org-agenda-toggle-waiting))


(defun gtd--set-keybindings ()
  (map! :leader
        :desc "Open inbox"
        "o i" #'gtd/inbox)
  (map! :leader
        :desc "Open inbox"
        "n g i" #'gtd/inbox)
  (map! :leader
        :desc "Open projects"
        "n g p" #'gtd/projects)
  (map! :leader
        :desc "Open actions"
        "n g a" #'gtd/actions))

(defun gtd/init (&optional dir)
  (if dir (setq gtd/dir dir))
  (gtd--set-capture-templates)
  (gtd--set-org-todo-keyword-faces)
  (gtd--register-files)
  (gtd--set-agenda-format)
  (gtd--set-keybindings))


(gtd/init)
