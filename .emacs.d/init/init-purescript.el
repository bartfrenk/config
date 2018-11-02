(require 'use-package)
(require 'bind-key)

(use-package purescript-mode
  :pin "melpa")

(defun purescript--guess-root-dir ()
  (let ((segments (reverse (split-string (buffer-file-name) "/"))))
    (while (and segments (not (string= (car segments) "src")))
      (setq segments (cdr segments)))
    (if (not segments)
        (psc-ide-suggest-project-dir)
      (string-join (reverse (cdr segments)) "/"))))

(defun purescript/restart-server ()
  "Starts the PSC server by non-interactively guessing the root directory."
  (interactive)
  (let ((root (purescript--guess-root-dir)))
    (message (concat "Starting server in " root))
    (psc-ide-server-start root)))

(let ((root (string-join (reverse segments) "/")))
  (message (concat "Starting server in " root))
  (psc-ide-server-start root ))

(use-package psc-ide
  :bind
  (:map purescript-mode-map
        ("M-]" . psc-ide-goto-definition)
        ("M-[" . xref-pop-marker-stack)
        ("C-c M-j" . purescript/restart-server))
  :commands (psc-ide-mode turn-on-purescript-indentation)
  :pin "melpa"
  :config
  (unbind-key "C-c C-s" psc-ide-mode-map)
  :init
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (turn-on-purescript-indentation))))


(provide 'init-purescript)
