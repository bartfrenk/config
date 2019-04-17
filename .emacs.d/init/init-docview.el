(require 'use-package)

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-continuous t))

;; (use-package evil-collection
;;   ;; Evil keybindings for many packages
;;   :after pdf-tools
;;   :config
;;   (evil-collection-init 'pdf)
;;   (evil-collection-init 'doc-view)
;;   (evil-collection-init 'view))

(provide 'init-docview)


