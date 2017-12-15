(require 'use-package)
(require 'functions)

(setf (cdr (rassoc 'html-mode auto-mode-alist)) 'web-mode)


(defun html-reformat ()
    (interactive)
    (web-mode-buffer-indent)
    (untabify-buffer))

(use-package web-mode
  :ensure t
  :functions web-mode-buffer-indent
  :bind (:map web-mode-map
              ("C-c C-a" . html-reformat)))

(use-package tidy
  :ensure t)

(provide 'init-html)
