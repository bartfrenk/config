(require 'paredit)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode 1)
                                  (paredit-mode 1)
                                  (evil-paredit-mode 1)))

(define-key emacs-lisp-mode-map (kbd "M-]") 'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-[") 'xref-pop-marker-stack)

(use-package emacs-lisp-mode)

(provide 'init-elisp)
