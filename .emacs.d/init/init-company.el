(require 'use-package)

(use-package company
  :bind
  (("M-<tab>" . company-complete)
   :map company-active-map
   ("C-d" . company-show-doc-buffer)
   ("C-h" . delete-backward-char))
  :commands global-company-mode
  :config
  (setq company-tooltip-limit 20
        company-idle-delay 0
        company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-begin-commands '(self-insert-command))
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  :diminish company-mode
  :ensure t)

(global-company-mode)

(provide 'init-company)
