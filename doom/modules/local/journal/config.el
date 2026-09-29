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

(defun journal/location-template ()
  "Capture template whose title defaults to the org heading at point."
  ;; Org calls this before recording :original-buffer, while the buffer
  ;; capture was invoked from is still current.
  (let* ((heading (when (derived-mode-p 'org-mode)
                    (ignore-errors
                      (substring-no-properties (org-get-heading t t t t)))))
         (title (read-string (format-prompt "Title" heading) nil nil heading)))
    (concat "* " title "\nDate: %U\nLocation: %a\n\n%?")))

(defun journal/add-capture-templates ()
  (add-to-list 'org-capture-templates
               `("j" "Journal entry" entry
                 (file journal/file)
                 "* %^{Title}\nDate: %U\n\n%?"))
  (add-to-list 'org-capture-templates
               `("J" "Journal entry with location" entry
                 (file journal/file)
                 (function journal/location-template))
               t))

(defun journal/init (&optional dir)
  (when dir
    (setq journal/dir dir))
  (journal/prune-capture-templates "j")
  (journal/prune-capture-templates "J")
  (journal/add-capture-templates))

(journal/init)
