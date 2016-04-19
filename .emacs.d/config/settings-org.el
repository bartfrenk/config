(require 'use-package)

(use-package org
  :init
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  :config
  (setq org-log-done t)
  (setq org-src-fontify-natively t))

(provide 'settings-org)
