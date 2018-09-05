(require 'use-package)
(require 'constants)
(require 'helm)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-cache-file (expand-file-name "projectile.cache" cache-dir)
        projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"))
  :bind
  ("<f5>" . projectile-compile-project)
  ("<f6>" . next-error)
  :diminish projectile-mode)

(use-package helm-projectile
  :commands (helm-projectile-on)
  :config
  (setq projectile-switch-project-action 'helm-projectile)
  :bind (:map projectile-mode-map
              ("C-c p s" . helm-projectile-ag)))

(projectile-mode)
(helm-projectile-on)

(provide 'init-projectile)
