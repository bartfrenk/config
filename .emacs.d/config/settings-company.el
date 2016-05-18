(require 'use-package)

(use-package company
  :commands global-company-mode
  :init
  (defun define-active-map-keys()
    (define-key company-active-map "\C-x\M-h" 'company-show-doc-buffer)
    (define-key company-active-map "\C-n" 'company-select-next)
    (define-key company-active-map "\C-p" 'company-select-previous)
    (define-key company-active-map "\C-h" 'delete-backward-char))
  (add-hook 'company-mode-hook 'define-active-map-keys)
  (global-company-mode)
  (setq company-global-modes '(not sage-mode
                                   message-mode))

  :bind
  (("M-<tab>" . company-complete))

  :config
  (setq company-tooltip-limit 20
        company-idle-delay .1
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-backends (delq 'company-capf company-backends)))


(provide 'settings-company)
