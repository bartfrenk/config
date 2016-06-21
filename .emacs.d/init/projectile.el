(require 'use-package)

(use-package projectile
  :init
  (use-package settings-globals)
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-cache-file (expand-file-name  "projectile.cache" cache-dir)
        projectile-switch-project-action 'helm-projectile
        projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"))
  (bind-key (kbd "C-c p s") 'helm-projectile-ag projectile-mode-map)
  :bind
  ("<f5>" . projectile-compile-project)
  ("<f6>" . next-error))


(provide 'settings-projectile)
