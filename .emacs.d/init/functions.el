;; This avoids having multiple themes loaded at the same time,
;; which is the default behaviour of 'load-theme'.
(defun switch-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(defun untabify-buffer ()
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'untabify))

(defun scan-list (func xs &optional init)
  "Fold LIST with FUNC and keeps the intermediate results."
  (let ((result (when init (list init))))
    (dolist (x xs result)
      (let ((head (if result
                      (funcall func (car result) x)
                    x)))
        (setq result (cons head result))))))

(defun unlines (strings)
  (mapconcat #'identity strings "\n"))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defmacro with-restore (var &rest body)
  (declare (indent defun))
  (let ((sym (gensym)))
    `(let ((,sym ,var))
       (unwind-protect
           (progn ,@body)
         (setq ,var ,sym)))))

(defun join (&rest rest)
  (mapconcat 'identity rest " "))

(provide 'functions)
