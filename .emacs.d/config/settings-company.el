(require 'use-package)

(use-package company
  :commands global-company-mode
  :init
  (global-company-mode)
  (setq company-global-modes '(not sage-mode
                                   message-mode))
  :config
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-backends (delq 'company-capf company-backends))
  :bind
  (("M-<tab>" . company-complete)))

(provide 'settings-company)
