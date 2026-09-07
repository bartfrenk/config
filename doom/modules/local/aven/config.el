(require 'transient)
(require 'magit-section)

(defvar aven--executable "aven")

(defvar aven-status-buffer-name "*aven-status*")

(defconst aven--ref-pattern "[A-Z][A-Z0-9]*-[A-Z0-9]+"
  "Pattern matching a task ref such as APP-7KQ9, without anchors.")

(defun aven--ref-at-point ()
  "Task ref at point, or nil."
  (or (when-let* ((section (and (fboundp 'magit-current-section) (magit-current-section)))
                   (_ (eq (oref section type) 'aven-task)))
        (oref section value))
      (let ((sym (thing-at-point 'symbol t)))
        (when (and sym (string-match-p (concat "\\`" aven--ref-pattern "\\'") sym))
          sym))))

(defun aven--ref-on-line ()
  "Task ref at the start of the current line, as printed by `list'/`search'."
  (save-excursion
    (forward-line 0)
    (when (looking-at aven--ref-pattern)
      (match-string-no-properties 0))))

(defun aven--read-ref (prompt)
  "Read a task ref, defaulting to the one at point."
  (let ((default (aven--ref-at-point)))
    (read-string (if default (format "%s(%s) " prompt default) prompt)
                 nil nil default)))

(defvar aven-output-font-lock-keywords
  `(("^\\$ aven .*$" . font-lock-comment-face)
    ("^description<<EOF$" . font-lock-preprocessor-face)
    ("^EOF$" . font-lock-preprocessor-face)
    ("^\\(Error:\\) error \\(\\S-+\\)" (1 'error) (2 'error))
    ("status=\\(todo\\)\\_>" 1 'success)
    ("status=\\(active\\)\\_>" 1 'warning)
    ("status=\\(done\\|canceled\\)\\_>" 1 'shadow)
    ("priority=\\(urgent\\)\\_>" 1 'error)
    ("priority=\\(high\\)\\_>" 1 'warning)
    ("priority=\\(low\\|none\\)\\_>" 1 'shadow)
    ("^\\s-*\\(ok\\)\\s-" 1 'success)
    ("^\\s-*\\(warn\\)\\s-" 1 'warning)
    ("^\\s-*\\(fail\\)\\s-" 1 'error)
    ("^\\s-*\\(\\.\\.\\)\\s-" 1 'shadow)
    ("^-+$" . font-lock-comment-face)
    ("^[A-Z][A-Za-z]+\\(?: [A-Za-z]+\\)*$" . font-lock-keyword-face)
    (,(concat "\\_<" aven--ref-pattern "\\_>") . font-lock-constant-face)
    ("\\_<\\([a-z][a-z_]*\\)=" 1 font-lock-variable-name-face)
    ("\"[^\"\n]*\"" . font-lock-string-face))
  "Font-lock keywords for `aven-output-mode'.")

(define-derived-mode aven-output-mode special-mode "Aven"
  "Major mode for displaying `aven' command output."
  (setq font-lock-defaults '(aven-output-font-lock-keywords)))

(evil-set-initial-state 'aven-output-mode 'motion)

(defun aven--call (buffer-name args)
  "Run aven with ARGS, a list of strings, and display the output in BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (aven-output-mode)
        (insert (format "$ aven %s\n\n"
                         (string-join (mapcar #'shell-quote-argument args) " ")))
        (apply #'call-process aven--executable nil t nil args)
        (goto-char (point-min))))
    (when (get-buffer aven-status-buffer-name)
      (aven-status-refresh))
    (display-buffer buf)))

(defun aven--run (&rest args)
  "Run aven with ARGS and display the output in `*aven*'."
  (aven--call "*aven*" args))

(defun aven--show-ref (ref)
  "Show REF's full detail in a buffer dedicated to that task."
  (aven--call (format "*aven: %s*" ref) (list "show" ref "--full")))

(defun aven-output-visit-task ()
  "Show the task on the current line in a dedicated buffer."
  (interactive)
  (let ((ref (or (aven--ref-on-line) (aven--ref-at-point))))
    (unless ref
      (user-error "No task ref on this line"))
    (aven--show-ref ref)))

(evil-define-key 'motion aven-output-mode-map
  (kbd "RET") #'aven-output-visit-task)

(transient-define-prefix aven/list ()
  "List Aven tasks."
  ["Filters"
   ("-r" "Ready only"        "--ready")
   ("-o" "Open (nonterminal)" "--open")
   ("-b" "Blocked"           "--blocked")
   ("-e" "Epics"             "--epics")
   ("-u" "Upcoming"          "--upcoming")
   ("-d" "Overdue"           "--overdue")
   ("-a" "Include deleted"   "--all")]
  ["Arguments"
   ("-p" "Project"  "--project=")
   ("-s" "Status"   "--status=")
   ("-i" "Priority" "--priority=")
   ("-l" "Label"    "--label=")
   ("-n" "Limit"    "--limit=")]
  ["Action"
   ("RET" "List tasks" aven--list-tasks)])

(defun aven--list-tasks (&optional args)
  (interactive (list (transient-args 'aven/list)))
  (apply #'aven--run "list" args))

(transient-define-prefix aven/search ()
  "Search Aven tasks."
  ["Arguments"
   ("-p" "Project"        "--project=")
   ("-n" "Limit"          "--limit=")
   ("-a" "Include deleted" "--all")]
  ["Action"
   ("RET" "Search" aven--search-tasks)])

(defun aven--search-tasks (&optional args)
  (interactive (list (transient-args 'aven/search)))
  (let ((query (read-string "Search: ")))
    (apply #'aven--run "search" (append args (split-string query)))))

(transient-define-prefix aven/show ()
  "Show an Aven task."
  ["Arguments"
   ("-f" "Full detail" "--full")]
  ["Action"
   ("RET" "Show task" aven--show-task)])

(defun aven--show-task (&optional args)
  (interactive (list (transient-args 'aven/show)))
  (let ((ref (aven--read-ref "Show task: ")))
    (apply #'aven--run "show" (append args (list ref)))))

(defun aven/context ()
  "Show a context snapshot for a task."
  (interactive)
  (aven--run "context" (aven--read-ref "Context for: ")))

(transient-define-prefix aven/add ()
  "Create an Aven task."
  ["Arguments"
   ("-p" "Project"       "--project=")
   ("-s" "Status"        "--status=")
   ("-i" "Priority"      "--priority=")
   ("-l" "Label"         "--label=")
   ("-a" "Available at"  "--available-at=")
   ("-d" "Due"           "--due=")
   ("-e" "Epic"          "--epic")]
  ["Action"
   ("RET" "Create task" aven--add-task)])

(defun aven--add-task (&optional args)
  (interactive (list (transient-args 'aven/add)))
  (let ((title (read-string "Title: ")))
    (when (string-empty-p title)
      (user-error "aven: title required"))
    (apply #'aven--run "add" (append args (list title)))))

(transient-define-prefix aven/edit ()
  "Edit an Aven task."
  ["Arguments"
   ("-s" "Status"           "--status=")
   ("-i" "Priority"         "--priority=")
   ("-p" "Project"          "--project=")
   ("-t" "Title"            "--title=")
   ("-l" "Add label"        "--label=")
   ("-L" "Remove label"     "--remove-label=")
   ("-a" "Available at"     "--available-at=")
   ("-A" "Clear available"  "--clear-available-at")
   ("-d" "Due"              "--due=")
   ("-D" "Clear due"        "--clear-due")
   ("-e" "Epic (on/off)"    "--epic=")]
  ["Action"
   ("RET" "Apply to task" aven--edit-task)])

(defun aven--edit-task (&optional args)
  (interactive (list (transient-args 'aven/edit)))
  (let ((ref (aven--read-ref "Edit task: ")))
    (apply #'aven--run "edit" (append args (list ref)))))

(defun aven--parse-sha256 (output)
  "First sha256=HASH field in OUTPUT, or nil."
  (when (string-match "sha256=\\([0-9a-f]+\\)" output)
    (match-string 1 output)))

(defvar-local aven-description--ref nil
  "Task ref this buffer's description belongs to.")

(defvar-local aven-description--field nil
  "Long text field this buffer edits, currently always \"description\".")

(defvar-local aven-description--sha256 nil
  "SHA-256 of the field's value as last read from or written to Aven.")

(defun aven-description--cleanup ()
  "Delete the scratch file backing an `aven-description-edit-mode' buffer."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (ignore-errors (delete-file buffer-file-name))))

(defun aven-description--after-save ()
  "Push this buffer's saved contents to Aven via `aven text set'."
  (let* ((ref aven-description--ref)
         (field aven-description--field)
         (file buffer-file-name)
         (sha aven-description--sha256)
         (result (with-temp-buffer
                   (let ((exit-code (call-process aven--executable nil t nil
                                                   "text" "set" ref field
                                                   "--file" file
                                                   "--if-sha256" sha)))
                     (cons exit-code (buffer-string))))))
    (if (zerop (car result))
        (progn
          (setq aven-description--sha256
                (or (aven--parse-sha256 (cdr result)) aven-description--sha256))
          (message "aven: saved %s for %s" field ref))
      (set-buffer-modified-p t)
      (message "aven: %s" (string-trim (cdr result))))))

(defvar aven-description-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'save-buffer)
    (define-key map (kbd "C-c C-k") #'kill-buffer)
    map))

(define-minor-mode aven-description-edit-mode
  "Minor mode for a buffer editing an Aven long-text field.
Saving the buffer (`save-buffer', `C-x C-s', `C-c C-c') pushes its
contents back via `aven text set', guarded by the SHA-256 read when
the buffer was opened, so a concurrent edit is refused rather than
silently overwritten."
  :lighter " Aven-Edit"
  (if aven-description-edit-mode
      (progn
        (add-hook 'after-save-hook #'aven-description--after-save nil t)
        (add-hook 'kill-buffer-hook #'aven-description--cleanup nil t))
    (remove-hook 'after-save-hook #'aven-description--after-save t)
    (remove-hook 'kill-buffer-hook #'aven-description--cleanup t)))

(defun aven/edit-description (&optional ref)
  "Open a buffer to edit REF's description, saved back via `aven text set'."
  (interactive)
  (let* ((ref (or ref (aven--read-ref "Edit description of: ")))
         (file (make-temp-file (format "aven-%s-description-" ref) nil ".md"))
         (result (with-temp-buffer
                   (let ((exit-code (call-process aven--executable nil t nil
                                                   "text" "get" ref "description"
                                                   "--output" file)))
                     (cons exit-code (buffer-string))))))
    (unless (zerop (car result))
      (delete-file file)
      (user-error "aven: %s" (string-trim (cdr result))))
    (let ((hash (aven--parse-sha256 (cdr result))))
      (find-file file)
      (cond ((fboundp 'gfm-mode) (gfm-mode))
            ((fboundp 'markdown-mode) (markdown-mode)))
      (setq-local aven-description--ref ref
                  aven-description--field "description"
                  aven-description--sha256 hash)
      (aven-description-edit-mode 1)
      (message "aven: editing description of %s (save to sync, C-c C-k to discard)" ref))))

(defun aven/note ()
  "Append a note to a task."
  (interactive)
  (let* ((ref (aven--read-ref "Note for: "))
         (text (read-string "Note: ")))
    (aven--run "note" ref text)))

(defun aven/sync ()
  "Sync Aven with its remote server."
  (interactive)
  (aven--run "sync"))

(defun aven/doctor ()
  "Diagnose Aven startup, configuration, and workspace state."
  (interactive)
  (aven--run "doctor"))

(transient-define-prefix aven/dispatch ()
  "Transient interface to the Aven CLI."
  ["Aven"
   ["Query"
    ("l" "List"    aven/list)
    ("s" "Search"  aven/search)
    ("w" "Show"    aven/show)
    ("c" "Context" aven/context)]
   ["Task"
    ("a" "Add"         aven/add)
    ("e" "Edit"        aven/edit)
    ("d" "Description" aven/edit-description)
    ("n" "Note"        aven/note)]
   ["Workspace"
    ("g" "Sync"   aven/sync)
    ("y" "Doctor" aven/doctor)]])

;;; Status buffer

(defgroup aven nil
  "Interface to the Aven task manager."
  :group 'tools)

(defcustom aven-status-fields '(priority due labels)
  "Task fields shown as columns in the Aven status buffer, in order.
The task ref and title are always shown, ref first and title last;
this controls the columns shown between them."
  :type '(repeat (choice (const :tag "Priority" priority)
                          (const :tag "Due date" due)
                          (const :tag "Labels" labels)
                          (const :tag "Project" project)))
  :group 'aven)

(defun aven--list-json (&rest args)
  "Run `aven list' with ARGS and return the parsed tasks."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process aven--executable nil t nil
                             "list" (append args (list "--json")))))
      (if (zerop exit-code)
          (let ((json-array-type 'list)
                (json-object-type 'plist)
                (json-key-type 'keyword))
            (json-read-from-string (buffer-string)))
        (error "aven: %s" (string-trim (buffer-string)))))))

(defun aven--task-priority-face (priority)
  (pcase priority
    ("urgent" 'error)
    ("high"   'warning)
    ("low"    'shadow)
    ("none"   'shadow)
    (_        nil)))

(defun aven--field-value (task field)
  "String value of FIELD in TASK, or nil if it has none worth showing."
  (pcase field
    ('priority (let ((v (plist-get task :priority))) (unless (equal v "none") v)))
    ('due      (let ((v (plist-get task :due_on))) (unless (string-empty-p v) v)))
    ('labels   (let ((v (plist-get task :labels))) (when v (string-join v ","))))
    ('project  (plist-get task :project))))

(defun aven--field-face (field value)
  (pcase field
    ('priority (aven--task-priority-face value))
    ('due      'warning)
    ('labels   'shadow)
    ('project  'shadow)))

(defun aven--column-widths (tasks)
  "Max display width of each field in `aven-status-fields' across TASKS."
  (mapcar (lambda (field)
            (cons field
                  (apply #'max 0 (mapcar (lambda (task)
                                            (length (or (aven--field-value task field) "")))
                                          tasks))))
          aven-status-fields))

(defun aven--task-row-text (task ref-width widths)
  "Text of TASK's table row: ref, the configured fields, then title.
REF-WIDTH and WIDTHS (an alist as returned by `aven--column-widths')
align the ref and field columns consistently across all sections."
  (concat
   (propertize (string-pad (plist-get task :ref) ref-width) 'face 'font-lock-constant-face)
   (mapconcat (lambda (field)
                (let ((value (aven--field-value task field)))
                  (concat "  " (propertize (string-pad (or value "") (alist-get field widths))
                                            'face (aven--field-face field value)))))
              aven-status-fields "")
   "  " (plist-get task :title)))

(defun aven--task-properties (task)
  "Alist of label/value pairs describing TASK's fields, for its drawer."
  (let ((priority  (plist-get task :priority))
        (labels    (plist-get task :labels))
        (due       (plist-get task :due_on))
        (available (plist-get task :available_at))
        (blocked   (plist-get task :blocked_by))
        (blocks    (plist-get task :blocks)))
    (delq nil
          (list (cons "status" (plist-get task :status))
                (unless (equal priority "none") (cons "priority" priority))
                (cons "project" (plist-get task :project))
                (when labels (cons "labels" (string-join labels ",")))
                (unless (string-empty-p due) (cons "due" due))
                (unless (string-empty-p available) (cons "available" available))
                (when (and blocked (> blocked 0)) (cons "blocked by" (number-to-string blocked)))
                (when (and blocks (> blocks 0)) (cons "blocks" (number-to-string blocks)))
                (when (eq (plist-get task :is_epic) t) (cons "epic" "yes"))
                (when (eq (plist-get task :has_conflict) t) (cons "conflict" "yes"))
                (cons "id" (plist-get task :id))))))

(defun aven--task-description (ref)
  "Raw description text of REF, or the empty string on failure."
  (with-temp-buffer
    (if (zerop (call-process aven--executable nil t nil
                              "text" "get" ref "description" "--raw"))
        (string-trim (buffer-string))
      "")))

(defun aven--insert-task-drawer (task)
  "Insert TASK's fields as properties, then its description, as the
body of its (folded) section."
  (insert "\n")
  (dolist (prop (aven--task-properties task))
    (insert "    " (propertize (format "%s:" (car prop)) 'face 'font-lock-comment-face)
            " " (cdr prop) "\n"))
  (insert "\n")
  (let ((description (aven--task-description (plist-get task :ref))))
    (if (string-empty-p description)
        (insert "    " (propertize "No description." 'face 'shadow) "\n")
      (dolist (line (split-string description "\n"))
        (insert "    " line "\n"))))
  (insert "\n"))

(defun aven--insert-task-line (task ref-width widths)
  "Insert TASK as a folded section: the heading is its table row,
the body is its properties drawer and description."
  (magit-insert-section (aven-task (plist-get task :ref) t)
    (magit-insert-heading (aven--task-row-text task ref-width widths))
    (aven--insert-task-drawer task)))

(defun aven--insert-task-section (heading hide tasks ref-width widths)
  "Insert a section titled HEADING listing TASKS as a table.
When HIDE is non-nil, the section starts folded. REF-WIDTH and WIDTHS
align columns consistently across all sections in the buffer."
  (when tasks
    (magit-insert-section (aven-tasks heading hide)
      (magit-insert-heading (format "%s (%d)" heading (length tasks)))
      (dolist (task tasks)
        (aven--insert-task-line task ref-width widths))
      (insert "\n"))))

(define-derived-mode aven-status-mode magit-section-mode "Aven-Status"
  "Major mode for the Aven status buffer.")

(evil-set-initial-state 'aven-status-mode 'motion)

(defun aven-status-visit-task-or-toggle ()
  "Show the task at point in a dedicated buffer, or toggle the section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (eq (oref section type) 'aven-task))
        (aven--show-ref (oref section value))
      (when section (magit-section-toggle section)))))

(defun aven-status-refresh ()
  "Rebuild the Aven status buffer."
  (interactive)
  (let* ((buf (get-buffer-create aven-status-buffer-name))
         (groups (list (cons "Active"  (aven--list-json "--status=active"))
                       (cons "Todo"    (aven--list-json "--status=todo"))
                       (cons "Backlog" (aven--list-json "--status=backlog"))
                       (cons "Inbox"   (aven--list-json "--status=inbox"))))
         (all-tasks (apply #'append (mapcar #'cdr groups)))
         (widths (aven--column-widths all-tasks))
         (ref-width (apply #'max 0 (mapcar (lambda (task) (length (plist-get task :ref)))
                                            all-tasks))))
    (with-current-buffer buf
      (unless (derived-mode-p 'aven-status-mode)
        (aven-status-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (aven-status)
          (dolist (group groups)
            (aven--insert-task-section (car group) nil (cdr group) ref-width widths)))
        (when (eq (point-min) (point-max))
          (insert (propertize "No tasks.\n" 'face 'shadow)))
        (let ((magit-section-cache-visibility nil))
          (magit-section-show magit-root-section)))
      (goto-char (point-min)))
    buf))

(defun aven/status ()
  "Open the Aven status buffer, the entry point for the Aven interface."
  (interactive)
  (switch-to-buffer (aven-status-refresh)))

(evil-define-key 'motion aven-status-mode-map
  (kbd "RET") #'aven-status-visit-task-or-toggle
  "g" #'aven-status-refresh
  "l" #'aven/list
  "s" #'aven/search
  "w" #'aven/show
  "c" #'aven/context
  "a" #'aven/add
  "e" #'aven/edit
  "d" #'aven/edit-description
  "n" #'aven/note
  "?" #'aven/dispatch)

(defun aven--register ()
  (map! :leader
        :desc "Aven"
        "n g t" #'aven/status))

(aven--register)
