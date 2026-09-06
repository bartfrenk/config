(require 'json)

(defvar gtd-aven--executable "aven")

(defun gtd-aven--entry-body ()
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

(defun gtd-aven--project-name ()
  "Top-level ancestor heading of the entry at point, i.e. its Aven project."
  (or (car (org-get-outline-path))
      (org-get-heading t t t t)))

(defun gtd-aven--push-refiled-entry ()
  "After a refile lands in .aven.org, create the equivalent Aven task
under the target project and remove the entry from the file. If the
CLI call fails, the entry is left in place for a retry."
  (when (and (buffer-file-name)
             (file-equal-p (buffer-file-name) (gtd--path ".aven.org")))
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (project (gtd-aven--project-name))
           (body (gtd-aven--entry-body))
           (exit-code (with-temp-buffer
                        (insert body)
                        (call-process-region (point-min) (point-max)
                                              gtd-aven--executable nil t nil
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

(defun gtd-aven--project-keys ()
  "Keys of all current Aven projects, as a list of strings."
  (with-temp-buffer
    (unless (zerop (call-process gtd-aven--executable nil t nil
                                  "project" "list" "--json"))
      (error "aven: failed to list projects: %s" (buffer-string)))
    (let ((json-array-type 'list)
          (json-object-type 'alist))
      (mapcar (lambda (project) (alist-get 'key project))
              (json-read-from-string (buffer-string))))))

(defun gtd-aven/generate-file ()
  "Ensure .aven.org has a top-level heading for every current Aven project.
Existing headings, and any entries left under them from a failed push,
are untouched. Headings with no matching project are reported, not removed."
  (interactive)
  (let* ((file (gtd--path ".aven.org"))
         (keys (gtd-aven--project-keys)))
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

(defun gtd-aven--register ()
  (add-to-list 'org-refile-targets `(,(gtd--path ".aven.org") :level . 1) t)
  (add-hook 'org-after-refile-insert-hook #'gtd-aven--push-refiled-entry))

(gtd-aven--register)
