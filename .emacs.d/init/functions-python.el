(require 'subr-x)
(require 'virtualenvwrapper)

(defun python--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun python--locate-version-file ()
  (let* ((name ".python-version")
         (dir (locate-dominating-file
               (buffer-file-name)
               (lambda (dir)
                 (file-exists-p (concat dir name))))))
    (when dir (concat dir name))))

(defun python/set-dominating-venv ()
  (interactive)
  (if-let ((file (python--locate-version-file)))
      (let ((venv-name (string-trim (python--read-file file))))
        (message "Setting virtualenv to %s" venv-name)
        (venv-workon venv-name))
    (message "No dominating venv found")))

(provide 'functions-python)

