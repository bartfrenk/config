(require 'use-package)
(require 'helm-dash)
(require 'eldoc)
(require 'evil)

(defun rust/setup-dash ()
  (setq-local helm-dash-docsets '("Rust")))

(use-package racer
  :bind (:map rust-mode-map
              ("M-]" . racer-find-definition)
              ("M-[" . xref-pop-marker-stack)
              ("C-c C-d" . racer-describe))
  :diminish racer-mode
  :init
  (add-hook 'racer-mode #'eldoc-mode)
  :config
  (setq racer-rust-src-path
        (concat "/home/bart/.rustup/toolchains"
                "/stable-x86_64-unknown-linux-gnu"
                "/lib/rustlib/src/rust/src"))
  (add-to-list 'evil-emacs-state-modes 'racer-help-mode))

(require 'company)

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-a" . rust-format-buffer)
              ("C-c C-h" . helm-dash-at-point))
  :init
  (add-hook 'rust-mode-hook (lambda ()
                              (rust/setup-dash)
                              (racer-mode)
                              ;; Sometimes racer completion is slow. Turn of
                              ;; automatic autocompletion completely. The issue
                              ;; has been noticed,
                              ;; see:https://github.com/racer-rust/emacs-racer/issues/86.
                              ;; Use Alt-Tab to complete.
                              (setq company-idle-delay nil)))
  :config
  (setq rust-format-on-save t
        rust-rustfmt-bin "rustfmt")
  :ensure t)


(use-package cargo
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode)
  :diminish cargo-minor-mode
  :bind (:map cargo-minor-mode-map
              ("C-c C-c C-b" . cargo-process-build)
              ("C-c C-c C-r" . cargo-process-run)
              ("C-c C-c C-t" . cargo-process-test)))


(use-package flycheck-rust
  :pin "melpa"
  :ensure t)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
