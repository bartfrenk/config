(load "~/.emacs.d/site/tla-mode/tla-mode")

(defvar tlc-executable "/opt/tla/bin/tlc"
  "The command that runs TLC.")

(defun tla/tlc ()
  "Calls the TLC checker on the current file.
To set the checker executable see `tlc-executable'.
"
  (interactive)
  (let* ((file-name (file-relative-name (buffer-file-name) default-directory))
         (cmd (concat tlc-executable " " file-name)))
    (shell-command cmd)) )

(provide 'init-tla)
