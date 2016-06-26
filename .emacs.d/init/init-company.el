(require 'use-package)

(use-package company
  :bind
  (("M-<tab>" . company-complete)
   :map company-active-map
   ("C-d" . company-show-doc-buffer)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-h" . delete-backward-char))
  :commands global-company-mode
  :config
  (setq company-tooltip-limit 20
        company-idle-delay .1
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-backends (delq 'company-capf company-backends))
  :ensure t)

(global-company-mode)

(provide 'init-company)
