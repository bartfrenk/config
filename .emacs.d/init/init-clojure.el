(require 'use-package)
(require 'evil)
(require 'helm-buffers)
(require 'clojure-mode)
(require 'functions)
(require 'cider)

(defun clojure-reformat ()
  (interactive)
  (cider-format-buffer)
  (cljr-clean-ns))

(use-package flycheck-joker)

(use-package sesman)

(use-package clojure-mode
  :commands clojure-mode
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (paredit-mode 1)
                                 (evil-paredit-mode 1)
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)))
  :config

  (define-clojure-indent
    (GET 2)
    (POST 2)
    (go-loop 1))

  ;;   ;;(api 2)
  ;;   ;; (send-off 1)
  ;;   ;; (cli 1)
  ;;   ;;
  ;;   ;; (fdef 1)
  ;;   ;; (cli 1)
  ;;   ;; (send-off 1)
  ;;   ;; (api 2)
  ;;   ;; (ANY 2)
  ;;   ;;
  ;;   ;;
  ;;   ;; (PUT 2)
  ;;   ;; (PATCH 2)
  ;;   ;; (DELETE 2)
  ;;   ;; (OPTIONS 2)
  ;;   ;; (context 2)
  ;;   ;; (for-all 2)
  ;;   ;; (expect-call 1)

  ;;   )

)

(defvar clojure/resolve-deps
  '("test")
  "Aliases in `deps.edn` for which to resolve dependencies when starting a REPL.")

(defun clojure--cli-with-resolve-deps (fn &rest args)
    "Prepends the resolve-deps aliases to the command line
parameters for the clojure CLI and restores it to the old value
when `fn` returns or fails."
    (with-restore cider-clojure-cli-parameters
      (if (null clojure/resolve-deps)
          (apply fn args)
        (let* ((prefix (->> clojure/resolve-deps
                            (mapcar (lambda (s) (format "-R:%s" s)))
                            (apply #'join)))
               (tmp-val (join prefix cider-clojure-cli-parameters)))
          (setq cider-clojure-cli-parameters tmp-val)
          (apply fn args)))))

(advice-add 'cider-jack-in-clj :around #'clojure--cli-with-resolve-deps)

(use-package cider
  :bind (:map clojure-mode-map
              ("M-]" . cider-find-var)
              ("M-[" . cider-pop-back)
              ("C-c C-a" . cider-format-buffer))
  :init
  (add-hook 'cider-mode-hook (lambda ()
                               (eldoc-mode 1)
                               (paredit-mode 1)))
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  :config

  (setq cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t
        cider-use-overlays t
        cider-repl-display-help-banner nil
        cider-clojure-cli-parameters "-m nrepl.cmdline --middleware '%s'")

  (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-test-report-mode)
  (add-to-list 'evil-emacs-state-modes 'cider-docview-mode)
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-server.*")
  (add-to-list 'helm-boring-buffer-regexp-list "*nrepl-messages.*")
  :diminish cider-mode
  :ensure t)

(use-package clj-refactor
  :diminish clj-refactor-mode)

(provide 'init-clojure)
