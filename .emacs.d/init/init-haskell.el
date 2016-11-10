(require 'use-package)
(require 'evil)

(defun hoogle-info ()
  "Shows extended hoogle documentation."
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'haskell-hoogle))

(defun haskell-reformat ()
  (interactive)
  (haskell-sort-imports)
  (hindent-reformat-buffer))

(use-package haskell-snippets
  :ensure t)

(use-package haskell-mode
  :bind
  (:map haskell-mode-map
        ("C-c C-l" . haskell-process-load-file)
        ("C-`" . haskell-interactive-bring)
        ("C-c c" . haskell-process-cabal)
        ("C-c h" . helm-hoogle)
        ("C-c C-d" . hoogle-info)
        ("C-c C-a" . haskell-reformat)
        ("M-]" . haskell-mode-jump-to-def-or-tag))
  :config
  (use-package haskell-doc)
  (setq haskell-process-type 'ghci ; use dir-locals for stack projects
        haskell-process-path-ghci "ghci"
        haskell-hoogle-command "hoogle"
        haskell-process-args-ghci '("-i.")
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-hoogle-imports t
        haskell-process-log t
        ; Quick fix for REPL's failing to obtain directory for non-cabal
        ; projects. Wil be fixed in a later release of haskell-mode. See
        ; issue-784.
        haskell-interactive-popup-errors nil
        haskell-process-load-or-reload-prompt t)
  (add-to-list 'evil-emacs-state-modes 'haskell-error-mode)
  :ensure t
  :init
  (use-package helm-hoogle :ensure t)

  (add-hook 'haskell-mode-hook (lambda ()
                                 (haskell-indentation-mode)
                                 (interactive-haskell-mode))))

(use-package flycheck-haskell
  :config
  (setq flycheck-ghc-search-path '("/home/bart/.cabal/lib/x86_64-linux-ghc-7.10.3"))
  :ensure t)
(add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)

(use-package company-ghci
  :config
  (add-to-list 'company-backends 'company-ghci)
  :ensure t)

(use-package hindent
  :ensure t)

(provide 'init-haskell)
