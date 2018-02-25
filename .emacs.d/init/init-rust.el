(require 'use-package)

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-a" . rust-format-buffer))
  :config
  (setq rust-format-on-save t
        rust-rustfmt-bin "rustfmt-force")
  :ensure t)

(use-package flycheck-rust
  :ensure t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
