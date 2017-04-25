(require 'use-package)

(use-package erlang
  :ensure t
  :init
  (add-hook 'erlang-mode-hook (lambda ()
                                 (erlang-edoc-mode)))

  :config
  (setq erlang-electric-commands nil
        erlang-indent-level 2))

(provide 'init-erlang)


