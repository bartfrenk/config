(require 'use-package)
(require 'evil)

(use-package cider
  :init
  (add-hook 'cider-mode-hook (lambda ()
                               (eldoc-mode 1)))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (paredit-mode 1)))
  :config
  (use-package helm-buffers)
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-use-overlays nil)
  (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-server.*")
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-messages.*")
  :diminish cider-mode
  :ensure t)

(use-package paredit
  :ensure t)

;; (use-package helm-clojuredocs
;;  :ensure t)

;; TODO: does not work too well yet
;; (use-package flycheck-clojure
;;   :commands flycheck-clojure-setup
;;   :ensure t)

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(provide 'init-clojure)
