(require 'use-package)

(use-package ess-site
  :config
  (setq inferior-R-args "--quiet --no-restore-history --no-save"))

(provide 'settings-ess)
