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

(defun purescript/reformat ()
  (interactive)
  (purescript-mode-format-imports))

(let ((root (string-join (reverse segments) "/")))
  (message (concat "Starting server in " root))
  (psc-ide-server-start root ))

;; Attempt to get REPL support in Emacs (not yet working: see also
;; https://github.com/purescript/purescript/issues/2886.
(use-package psci
  :config
  (setq psci/file-path "pulp"
        psci/arguments '("repl")))


(use-package psc-ide
  :bind
  (:map purescript-mode-map
        ("M-]" . psc-ide-goto-definition)
        ("M-[" . xref-pop-marker-stack)
        ("C-c M-j" . purescript/restart-server)
        ("C-c C-a" . purescript/reformat))
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
