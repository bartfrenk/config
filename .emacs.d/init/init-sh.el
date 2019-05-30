(require 'use-package)
(require 'flycheck)

(use-package sh-script
  :config
  (add-hook 'sh-mode-hook (lambda ()
                            (flycheck-select-checker 'sh-posix-bash)))
  (setq sh-basic-offset 2
        smie-indent-basic 2
        flycheck-shellcheck-follow-sources nil
        flycheck-shellcheck-excluded-warnings
        '("SC1090" ;; No warnings on non-constant sources
          "SC2039" ;; No warnings on sh incompatible constructions
          )
        ))

(provide 'init-sh)
