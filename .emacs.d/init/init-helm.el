(require 'use-package)

(use-package helm
  :diminish helm-mode
  :ensure t
  :init
  (helm-mode)
  :config
  (use-package helm-buffers)
  (use-package helm-mode)
  (use-package helm-config)
  (use-package helm-bookmark)
  (use-package helm-misc)
  (use-package helm-files)
  (use-package helm-command)
  (use-package helm-locate)
  (use-package helm-org)
  (setq helm-split-window-in-side-p t
        helm-bookmark-show-location t
        helm-buffers-fuzzy-matching t
        helm-buffer-max-length nil
        helm-buffer-details-flag nil
        helm-input-idle-delay 0.01)
  (add-to-list 'helm-boring-buffer-regexp-list "*epc con \\d+*")

  ;; TODO: what does this do?
  (defadvice helm-display-mode-line
    (after undisplay-header activate)
    (setq header-line-format nil))

  :bind
  (("C-x C-b" . helm-buffers-list) ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-c i" . helm-imenu)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))

(use-package helm-gtags
  :ensure t)

(use-package helm-swoop
  :ensure t
  :bind
  (("C-c s" . helm-multi-swoop-all)
   ("C-s" . helm-swoop)
   ("C-r" . helm-resume)))

(use-package helm-ag
  :ensure t
  :bind (("C-c a" . helm-ag)))
(use-package helm-flycheck
  :ensure t
  :bind
  ("C-c e" . helm-flycheck)
  :commands helm-flycheck)

(provide 'init-helm)
