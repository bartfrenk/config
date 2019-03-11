(require 'use-package)
(require 'helm-dash)
(require 'eldoc)
(require 'evil)

(defun rust/setup-dash ()
  (setq-local helm-dash-docsets '("Rust")))

(use-package racer
  :bind (:map racer-mode-map
              ("M-]" . racer-find-definition)
              ("M-[" . xref-pop-marker-stack)
              ("C-c C-d" . racer-describe))
  :init
  (add-hook 'racer-mode #'eldoc-mode)
  :config
  (setq racer-rust-src-path
        (concat "/home/bart/.rustup/toolchains"
                "/stable-x86_64-unknown-linux-gnu"
                "/lib/rustlib/src/rust/src"))
  (add-to-list 'evil-emacs-state-modes 'racer-help-mode))


(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-a" . rust-format-buffer)
              ("C-c C-h" . helm-dash-at-point))
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (rust/setup-dash)
                              (racer-mode)
                              ))
  :config
  (setq rust-format-on-save t
        rust-rustfmt-bin "rustfmt-force")
  :ensure t)


(use-package flycheck-rust
  :pin "melpa"
  :ensure t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
