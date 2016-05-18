(require 'use-package)
(require 'org)

(defun org-fill-paragraph--latex-environment (&rest args)
  "Use default fill-paragraph in latex environments."
  (not (eql (org-element-type (org-element-context)) 'latex-environment)))
(advice-add 'org-fill-paragraph :before-while #'org-fill-paragraph--latex-environment)

(use-package org
  :init

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  :config
  (setq org-log-done t
        org-src-fontify-natively t))

(provide 'settings-org)
