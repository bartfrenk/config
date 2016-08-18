(require 'use-package)

(setq-default tab-width 4 indent-tabs-mode nil fill-column 80)
(setq inhibit-startup-message t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      initial-scratch-message nil scroll-preserve-screen-position nil
      scroll-conservatively 101 scroll-margin 20) (fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)

(set-frame-font "Source Code Pro 10" nil t)
(add-to-list 'default-frame-alist '(font . "Source Code Pro 10"))
(global-set-key (kbd "C-x s") nil)
(global-set-key (kbd "C-x M-s") 'save-some-buffers)
(global-set-key (kbd "C-x C-o") nil)
(global-set-key (kbd "C-x M-o") 'delete-blank-lines)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C--") 'linum-mode)

(use-package hl-line
  :commands global-hl-line-mode
  :init (setq global-hl-line-sticky-flag nil))

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (setq-default evil-move-cursor-back nil))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook (lambda ()
                              (rainbow-delimiters-mode)))
  :config
  (use-package color :ensure t)

  (defun rainbow-delimiters-saturate (percent)
    "Saturate rainbow delimiters by specified percentage."
    (interactive "nPercentage: ")
    (dolist (index (number-sequence 1 rainbow-delimiters-max-face-count))
      (let* ((face (intern (format "rainbow-delimiters-depth-%d-face" index)))
             (old-color (face-foreground face)))
        (unless (null old-color)
          (let ((new-color (color-saturate-name old-color percent)))
            (set-face-foreground face new-color))))))

  (setq rainbow-delimiters-outermost-only-face-count 1)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                      :foreground 'unspecified
                      :weight 'bold))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :init (global-whitespace-mode t)
  :config (setq whitespace-style '(face empty tabs lines-tail trailing)
                whitespace-line-column 100
                ;; avoid highlighting large tables in org-mode
                whitespace-global-modes '(not latex-mode org-mode)))

(use-package yasnippet
  :commands yas-global-mode
  :ensure t
  :diminish yas-minor-mode
  :init
  (use-package helm-c-yasnippet :ensure t)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(use-package linum
  :ensure t
  :init
  ;; Ensure that there is sufficient space for line numbers.
  (global-linum-mode)
  (defvar linum-format-fmt "%d" "Cached line number format string.")
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt (let
                              ((w (length (number-to-string
                                           (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string (+ w 1)) "d")))))
  (defun linum-format-func (line)
    (concat
     (propertize (format linum-format-fmt line) 'face 'linum)
     (propertize " " 'face 'linum)))
  :config
  (setq linum-format 'linum-format-func))

(use-package smart-mode-line
   :ensure t
   :functions (sml/faces-from-theme
               sml/theme-p)
   :init
   (setq sml/no-confirm-load-theme t
         sml/theme 'light)
   :commands sml/setup)

(sml/setup)

(use-package git-gutter-fringe
  :commands global-git-gutter-mode
  :diminish git-gutter-mode
  :ensure t)

(use-package smooth-scrolling :ensure t)
(use-package fringe-helper :ensure t)
(use-package phabricator :ensure t)
(use-package yaml-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package eldoc :diminish eldoc-mode :ensure t)
(use-package undo-tree :diminish undo-tree-mode :ensure t)
(use-package dockerfile-mode :ensure t)

(require 'git-gutter-fringe) ; to avoid regular git-gutter mode
(global-git-gutter-mode)
(global-hl-line-mode)
(yas-global-mode)

(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

(provide 'init-core)
