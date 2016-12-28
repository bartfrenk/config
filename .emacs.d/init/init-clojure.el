(require 'use-package)
(require 'evil)

(use-package clojure-mode
  :commands clojure-mode
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (paredit-mode 1)
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (flycheck-clojure-setup)))
  :functions put-clojure-indent
  :config
  (setq clojure-indent-style :always-align)
  (define-clojure-indent
    (send-off 1) (cli 1) (go-loop 1) (fdef 1) (context 1) (expect-call 1)
    (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2)
    (OPTIONS 2)))

(use-package cider
  :bind (:map clojure-mode-map
              ("M-]" . cider-find-var)
              ("M-[" . cider-pop-back)
              ("C-c C-a" . cider-format-buffer))
  :init
  (add-hook 'cider-mode-hook (lambda ()
                               (eldoc-mode 1)))
  :config
  (use-package helm-buffers)
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-use-overlays t)
  (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-test-report-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-server.*")
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-messages.*")
  :diminish cider-mode
  :ensure t)

(use-package paredit
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package clj-refactor
  :ensure t)


;; (use-package helm-clojuredocs
;;  :ensure t)

;; TODO: does not work too well yet
(use-package flycheck-clojure
  :commands flycheck-clojure-setup
  :ensure t)


;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(provide 'init-clojure)
