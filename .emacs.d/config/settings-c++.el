(require 'use-package)
(require 'whitespace)

(use-package cc-mode
  :init
  (add-hook 'c-mode-common-hook (lambda()
                                  (setq whitespace-line-column 100)))
  :config
  (set-fill-column 120)
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4
        c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (c-common-mode . "stroustrup-custom")))
  :bind
  (("C-c o" . ff-find-other-file)))

(provide 'settings-c++)
