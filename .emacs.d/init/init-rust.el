(require 'use-package)
(require 'helm-dash)

(defun rust/setup-dash ()
  (setq-local helm-dash-docsets '("Rust")))

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-a" . rust-format-buffer)
              ("C-c C-d" . helm-dash-at-point))
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (rust/setup-dash)))
  :config
  (setq rust-format-on-save t
        rust-rustfmt-bin "rustfmt-force")
  :ensure t)


(use-package flycheck-rust
  :pin "melpa"
  :ensure t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
