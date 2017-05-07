(require 'paredit)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode 1)
                                  (paredit-mode 1)))

(provide 'init-elisp)
