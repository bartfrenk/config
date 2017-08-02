(require 'use-package)
(require 'evil)
(require 'helm-buffers)
(require 'functions)

(use-package idris-mode
  :ensure t
  :config
  (add-to-list 'evil-emacs-state-modes 'idris-hole-list-mode)
  (append-to-list 'helm-boring-buffer-regexp-list
                  '("*idris-events*"
                    "*idris-connection*"
                    "*idris-process*"))
  (add-to-list 'helm-boring-buffer-regexp-list "*idris-events*")
  (add-to-list 'helm-boring-buffer-regexp-list "*idris-events*")
  )

(provide 'init-idris)

