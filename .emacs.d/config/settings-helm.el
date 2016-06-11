(require 'use-package)

(use-package helm

  :init
  (use-package helm-mode)
  (use-package helm-config)
  (use-package helm-misc)
  (use-package helm-buffers)
  (use-package helm-files)
  (use-package helm-locate)
  (use-package helm-bookmark)
  (use-package helm-gtags)
  (use-package helm-flycheck)
  (use-package helm-projectile)

  (setq helm-split-window-in-side-p t
        helm-bookmark-show-location t
        helm-buffers-fuzzy-matching t
        helm-buffer-max-length nil
        helm-buffer-details-flag t
        helm-input-idle-delay 0.01)
  (add-to-list 'helm-boring-buffer-regexp-list "*epc con \\d+*")

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (defadvice helm-display-mode-line
    (after undisplay-header activate)
    (setq header-line-format nil))

  :bind
  (("M-x" . helm-M-x)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-c e" . helm-flycheck)
   ("C-c i" . helm-imenu)))

(use-package helm-swoop
  :bind
  (("C-c C-SPC" . helm-swoop)
   ("C-c s" . helm-multi-swoop-all)
   ("C-s" . helm-swoop)
   ("C-r" . helm-resume)))

(use-package helm-ring
  :bind (("M-y" . helm-show-kill-ring)))

(use-package helm-ag
  :bind (("C-c a" . helm-ag)))

(provide 'settings-helm)
