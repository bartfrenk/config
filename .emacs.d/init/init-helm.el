(require 'use-package)
(require 'helm-buffers)
(require 'helm-mode)
(require 'helm-config)
(require 'helm-bookmark)
(require 'helm-misc)
(require 'helm-files)
(require 'helm-command)
(require 'helm-locate)

(use-package helm
  :diminish helm-mode
  :ensure t
  :init
  (helm-mode)
  :config
  (setq helm-split-window-inside-p t
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
  (("C-x C-b" . helm-buffers-list)
   ("M-x" . helm-M-x)
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
