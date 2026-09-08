(require 'org-capture)
(require 'json)

(defvar gtd/dir "")

(defvar gtd--gtd-executable "~/.local/bin/gtd")

(defun gtd--path (file)
  (concat gtd/dir "/" file))

(defun gtd/sync ()
  "Run `gtd sync' to synchronize local GTD files."
  (interactive)
  (let ((exit-code (call-process (expand-file-name gtd--gtd-executable) nil nil nil "sync")))
    (if (zerop exit-code)
        (message "gtd: synced")
      (message "gtd: sync failed (exit %s)" exit-code))))

(defun gtd/inbox ()
  (interactive)
  (gtd/sync)
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

(defun gtd--set-org-todo-keywords ()
  (setq
   org-modern-todo nil
   org-todo-keywords
   '((sequence "TODO(t)" "URGENT(u)" "WAITING(w)" "ACTIVE(a)" "|" "DONE(d)" "CANCELLED(c)"))
   org-todo-keyword-faces
   '(("TODO"      . (:inherit success :weight bold :underline t))
     ("URGENT"    . (:inherit error :weight bold :underline t))
     ("WAITING"   . (:inherit warning :weight bold :underline t))
     ("ACTIVE"    . (:inherit warning :weight bold :underline t))
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


(defun gtd--refile-target-no-todo-p ()
  (not (org-get-todo-state)))

(defun gtd/prune-completed ()
  "Remove all completed headlines (TODO state DONE or CANCELLED)
from the current buffer, along with their subtrees."
  (interactive)
  (let ((markers (org-map-entries
                  (lambda () (point-marker))
                  "TODO=\"DONE\"|TODO=\"CANCELLED\""
                  'file)))
    (dolist (marker (reverse markers))
      (org-with-point-at marker
        (delete-region (point) (org-end-of-subtree t t))))
    (save-buffer)
    (message "gtd: pruned %d completed headline(s)" (length markers))))

(defun gtd/prune-completed-all ()
  "Run `gtd/prune-completed' on every .org file in `gtd/dir'."
  (interactive)
  (let ((files (directory-files gtd/dir t "\\.org\\'")))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (gtd/prune-completed)))
    (message "gtd: pruned completed headlines in %d file(s)" (length files))))

(defun gtd--register-files ()
  (setq org-refile-targets
        `((,(gtd--path "projects.org") :maxlevel . 3)
          (,(gtd--path "actions.org")  :level . 0))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-agenda-files (list (gtd--path "actions.org") (gtd--path "projects.org"))
        org-refile-target-verify-function #'gtd--refile-target-no-todo-p))

(defun gtd--org-agenda-project ()
  (save-excursion
    (org-back-to-heading t)
    (let ((headings nil)
          (levels 0))
      (while (and (< levels 2)
                  (org-up-heading-safe))
        (push (org-get-heading t t t t) headings)
        (setq levels (1+ levels)))
      (format "%-25s" (string-join headings "-")))))

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
  (gtd--set-org-todo-keywords)
  (gtd--register-files)
  (gtd--set-agenda-format)
  (gtd--set-keybindings))
