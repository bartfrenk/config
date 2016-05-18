(require 'use-package)
(require 'haskell-mode)
(require 'haskell-doc)
(require 'haskell-indentation)
(require 'company)
(require 'flycheck)

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook (lambda ()
                                 (haskell-indentation-mode)
                                 (interactive-haskell-mode)))
  :config
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ; Use C-u C-c C-h to display extra info (i.e. pass --info to hoogle)
  ; where C-u stands for activating the prefix command
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
  (setq haskell-process-type 'auto
        haskell-hoogle-command "hoogle"
        haskell-process-path-ghci "ghci"
        haskell-process-args-ghci '("-i.")
        flycheck-ghc-search-path '("/home/bart/.cabal/lib/x86_64-linux-ghc-7.10.3")
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-suggest-hoogle-imports t
        haskell-process-log t
        ; Quick fix for REPL's failing to obtain directory for non-cabal
        ; projects. Wil be fixed in a later release of haskell-mode. See
        ; issue-784.
        haskell-interactive-popup-errors nil))
        haskell-process-load-or-reload-prompt t

(require 'flycheck-haskell)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)

(require 'company-ghci)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)

(provide 'settings-haskell)
