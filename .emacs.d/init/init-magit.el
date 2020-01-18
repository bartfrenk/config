(use-package magit)

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

(use-package magit-todos
  :init
  (add-hook 'magit-mode-hook (lambda () (magit-todos-mode t))))

;; To get forge working:
;; https://www.reddit.com/r/emacs/comments/a7r265/work_with_git_forges_from_the_comfort_of_magit/

;; For now, it seems that the only thing to get it working is to do `git config
;; gitlab.user bartfrenk` in the repository you want to use forge with.

(use-package forge
  :pin melpa
  :after magit)

(use-package ghub
  :pin melpa)

(use-package evil-magit
  :pin melpa
  :after magit)

(provide 'init-magit)
