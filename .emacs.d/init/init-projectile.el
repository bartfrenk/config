(require 'use-package)
(require 'constants)
(require 'helm)

(use-package projectile
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-cache-file (expand-file-name "projectile.cache" cache-dir)
        projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"))
  :bind (:map projectile-mode-map
              ("C-c p i" . projectile-invalidate-cache))
  :diminish projectile-mode)

(use-package helm-projectile
  :commands (helm-projectile-on)
  :pin "melpa"
  :config
  (setq projectile-switch-project-action 'helm-projectile)
  :bind (:map projectile-mode-map
              ("C-c p s" . helm-projectile-ag)
              ("C-c p p" . helm-projectile-switch-project)
              ("C-c p f" . helm-projectile-find-file)))


(projectile-mode)
(helm-projectile-on)

(provide 'init-projectile)
