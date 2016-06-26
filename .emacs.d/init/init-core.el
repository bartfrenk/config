(require 'constants)
(require 'use-package)

(setq-default tab-width 4 indent-tabs-mode nil
              fill-column 100)

(setq inhibit-startup-message t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      initial-scratch-message nil)

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(set-frame-font "Source Code Pro 10" nil t)
(add-to-list 'default-frame-alist '(font . "Source Code Pro 10"))

(global-set-key (kbd "C-x s") nil)
(global-set-key (kbd "C-x M-s") 'save-some-buffers)
(global-set-key (kbd "C-x C-o") nil)
(global-set-key (kbd "C-x M-o") 'delete-blank-lines)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t)

(use-package hl-line
  :commands global-hl-line-mode
  :init
  (setq global-hl-line-sticky-flag nil))

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (setq-default evil-move-cursor-back nil))

(use-package smooth-scrolling :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :init (global-whitespace-mode t)
  :config (setq whitespace-style '(face empty tabs lines-tail trailing)
                whitespace-line-column fill-column
                ;; avoid highlighting large tables in org-mode
                whitespace-global-modes '(not latex-mode org-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(use-package linum
  :ensure t
  :init
  ;; Ensure that there is sufficient space for line numbers.
  (global-linum-mode)
  (defvar linum-format-fmt "%d" "Cached line number format string.")
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let
                              ((w (length (number-to-string
                                           (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string (+ w 1)) "d")))))
  (defun linum-format-func (line)
    (concat
     (propertize (format linum-format-fmt line) 'face 'linum)
     (propertize " " 'face 'linum)))
  :config
  (setq linum-format 'linum-format-func))

(global-hl-line-mode)

(provide 'init-core)
