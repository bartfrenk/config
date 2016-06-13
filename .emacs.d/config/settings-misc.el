(require 'use-package)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))) (use-package evil
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (setq-default evil-move-cursor-back nil))

(use-package ws-butler
  :commands ws-butler-mode
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (setq-default undo-tree-visualizer-timestamps t)
  (setq-default undo-tree-visualizer-diff t))

(use-package smooth-scrolling)

(use-package speedbar
  :config (setq speedbar-use-images nil))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package whitespace
  :init (global-whitespace-mode t)
  :config (setq whitespace-style '(face empty tabs lines-tail trailing)
                whitespace-line-column 120
                ;; avoid highlighting large tables in org-mode
                whitespace-global-modes '(not latex-mode org-mode)))

(use-package ace-window
  :bind ("M-a" . ace-window))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(use-package linum
  :init
  ;; Ensure that there is sufficient space for line numbers.
  (global-linum-mode)
  (defvar linum-format-fmt "Cached line number format string.")
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

(when window-system
  (scroll-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)


;; This avoids having multiple themes loaded at the same time,
;; which is the default behavious of 'load-theme'.
(defun switch-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(switch-theme 'material)
(show-paren-mode)
(global-hl-line-mode)
(column-number-mode t)
(tool-bar-mode -1)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups"))
              tab-width 4 indent-tabs-mode nil
              fill-column 100)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c d") 'insert-current-date)

(provide 'settings-misc)
