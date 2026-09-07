(require 'transient)

(defvar aven--executable "aven")

(defconst aven--ref-pattern "[A-Z][A-Z0-9]*-[A-Z0-9]+"
  "Pattern matching a task ref such as APP-7KQ9, without anchors.")

(defun aven--ref-at-point ()
  "Task ref at point, or nil."
  (let ((sym (thing-at-point 'symbol t)))
    (when (and sym (string-match-p (concat "\\`" aven--ref-pattern "\\'") sym))
      sym)))

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
    ("a" "Add"  aven/add)
    ("e" "Edit" aven/edit)
    ("n" "Note" aven/note)]
   ["Workspace"
    ("g" "Sync"   aven/sync)
    ("y" "Doctor" aven/doctor)]])

(defun aven--register ()
  (map! :leader
        :desc "Aven"
        "n g t" #'aven/dispatch))

(aven--register)
