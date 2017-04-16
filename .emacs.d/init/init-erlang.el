(require 'use-package)

(use-package erlang
  :ensure t
  :config
  (setq erlang-electric-commands nil
        erlang-indent-level 2))

(provide 'init-erlang)


