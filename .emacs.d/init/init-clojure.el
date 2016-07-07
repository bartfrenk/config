(require 'use-package)
(require 'evil)

(use-package cider
  :bind (:map clojure-mode-map
              ("M-]" . cider-find-var)
              ("M-[" . cider-pop-back))
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
  (add-to-list 'evil-emacs-state-modes 'cider-test-report-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-server.*")
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-messages.*")


  :diminish cider-mode
  :ensure t)

(use-package paredit
  :ensure t)

;; Endy: indentation expect-call etc.
 ;; '(define-clojure-indent
 ;;    ;; Core
 ;;    (send-off 1) (cli 1) (go-loop 1)

 ;;    ;;Compojure
 ;;    (ANY 2) (GET 2) (POST 2) (PUT 2) (PATCH 2) (DELETE 2)
 ;;    (OPTIONS 2)

 ;;    ;; expect-call
 ;;    (expect-call 1)
 ;;    ;; Korma
 ;;    ;; (select 1) (insert 1) (update 1) (where 1) (set-fields 1)
 ;;    ;; (values 1) (delete 1) (upsert 1) (subselect 1)
 ;;    ))

;; (use-package helm-clojuredocs
;;  :ensure t)

;; TODO: does not work too well yet
;; (use-package flycheck-clojure
;;   :commands flycheck-clojure-setup
;;   :ensure t)

;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(provide 'init-clojure)
