(require 'use-package)
(require 'evil)
(require 'helm-buffers)

(defun clojure-reformat ()
  (interactive)
  (cider-format-buffer)
  (cljr-clean-ns))


(use-package flycheck-joker
  :ensure t
  :pin melpa-stable)

(use-package clojure-mode
  :commands clojure-mode
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (paredit-mode 1)
                                 (evil-paredit-mode 1)
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 ;;(flycheck-clojure-setup)
                                 ))
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  :functions put-clojure-indent
  :config
  (setq clojure-indent-style :align-arguments)

  (define-clojure-indent
    ;; Core
    (send-off 1) (cli 1) (go-loop 1)

    ;; Miscellaneous
    (fdef 1) (cli 1) (send-off 1)

    ;; Compojure
    (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2)
    (OPTIONS 2) (context 2) (api 2) (for-all 2)

    ;; Expect-call
    (expect-call 1)))


(use-package cider
  :bind (:map clojure-mode-map
              ("M-]" . cider-find-var)
              ("M-[" . cider-pop-back)
              ("C-c C-a" . clojure-reformat))
  :init
  (add-hook 'cider-mode-hook (lambda ()
                               (eldoc-mode 1)))
  :config
  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-use-overlays t
        cider-repl-display-help-banner nil)
  (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-test-report-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-server.*")
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-messages.*")
  :diminish cider-mode
  :ensure t)


(use-package clj-refactor)

(provide 'init-clojure)
