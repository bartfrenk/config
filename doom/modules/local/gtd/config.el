(require 'org-capture)
(require 'json)

(defvar gtd/dir "")

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

(defvar gtd--aven-executable "aven")

(defun gtd--aven-entry-body ()
  "Body text of the entry at point: planning line, property drawer,
and logbook excluded; child subtrees excluded."
  (org-back-to-heading t)
  (let ((subtree-end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-meta-data t)
    (let ((body-end (or (save-excursion
                           (when (re-search-forward org-outline-regexp-bol subtree-end t)
                             (match-beginning 0)))
                         subtree-end)))
      (string-trim (org-remove-indentation (buffer-substring-no-properties (point) body-end))))))

(defun gtd--aven-project-name ()
  "Top-level ancestor heading of the entry at point, i.e. its Aven project."
  (or (car (org-get-outline-path))
      (org-get-heading t t t t)))

(defun gtd--push-refiled-entry-to-aven ()
  "After a refile lands in .aven.org, create the equivalent Aven task
under the target project and remove the entry from the file. If the
CLI call fails, the entry is left in place for a retry."
  (when (and (buffer-file-name)
             (file-equal-p (buffer-file-name) (gtd--path ".aven.org")))
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (project (gtd--aven-project-name))
           (body (gtd--aven-entry-body))
           (exit-code (with-temp-buffer
                        (insert body)
                        (call-process-region (point-min) (point-max)
                                              gtd--aven-executable nil t nil
                                              "add" title
                                              "--project" project
                                              "--description-stdin"))))
      (if (zerop exit-code)
          (progn
            (org-back-to-heading t)
            (delete-region (point) (org-end-of-subtree t t))
            (save-buffer)
            (message "aven: added %S to project %S" title project))
        (message "aven: failed to add %S (exit %s); left in .aven.org" title exit-code)))))

(defun gtd--aven-project-keys ()
  "Keys of all current Aven projects, as a list of strings."
  (with-temp-buffer
    (unless (zerop (call-process gtd--aven-executable nil t nil
                                  "project" "list" "--json"))
      (error "aven: failed to list projects: %s" (buffer-string)))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (mapcar (lambda (project) (alist-get 'key project))
              (json-read-from-string (buffer-string))))))

(defun gtd/aven-generate-file ()
  "Ensure .aven.org has a top-level heading for every current Aven project.
Existing headings, and any entries left under them from a failed push,
are untouched. Headings with no matching project are reported, not removed."
  (interactive)
  (let* ((file (gtd--path ".aven.org"))
         (keys (gtd--aven-project-keys)))
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (let ((existing (org-map-entries (lambda () (org-get-heading t t t t)) "LEVEL=1")))
        (dolist (key keys)
          (unless (member key existing)
            (goto-char (point-max))
            (unless (bobp) (unless (bolp) (insert "\n")))
            (insert (format "* %s\n" key))))
        (let ((stale (seq-difference existing keys)))
          (when stale
            (message "aven: .aven.org headings with no matching project: %s"
                      (string-join stale ", ")))))
      (save-buffer))
    (message "aven: .aven.org synced with %d project(s)" (length keys))))

(defun gtd--register-files ()
  (setq org-refile-targets
        `((,(gtd--path "projects.org") :maxlevel . 3)
          (,(gtd--path "actions.org")  :level . 0)
          (,(gtd--path ".aven.org")    :level . 1))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-agenda-files (list (gtd--path "actions.org") (gtd--path "projects.org"))
        org-refile-target-verify-function #'gtd--refile-target-no-todo-p)
  (add-hook 'org-after-refile-insert-hook #'gtd--push-refiled-entry-to-aven))

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
  (gtd--set-org-todo-keyword-faces)
  (gtd--register-files)
  (gtd--set-agenda-format)
  (gtd--set-keybindings))

(gtd/init)
