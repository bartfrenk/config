(require 'use-package)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(use-package evil
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (setq-default evil-move-cursor-back nil))

(use-package ws-butler
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package undo-tree
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

(when window-system
  (scroll-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode)

(when (display-graphic-p)
  (global-hl-line-mode 1))

;; TODO set options depending on whether client opened in text terminal or in X
(defadvice make-frame-command (after set-hl-line-mode last activate)
  "Sets hl-line-mode if the frame is graphical."
  (if (display-graphic-p)
      (progn
        (message (symbol-name window-system))
        (global-hl-line-mode 1))
    (message (symbol-name window-system))
    (global-hl-line-mode 1)))

;(add-hook 'after-make-frame-functions
;          (lambda (frame)
;            (if window-system
;                (progn
;                  (message (symbol-name window-system))
;                  (global-hl-line-mode))
;              (message (symbol-name window-system)))))

(show-paren-mode)
(column-number-mode t)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(setq-default backup-directory-alist '(("." . "~/.emacs.d/backups"))
              tab-width 4 indent-tabs-mode nil
              fill-column 100)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c d") 'insert-current-date)

(provide 'settings-misc)
