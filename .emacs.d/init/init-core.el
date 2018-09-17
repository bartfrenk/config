(require 'use-package)
(require 'functions)


(setq-default tab-width 4
              indent-tabs-mode nil
              fill-column 80)
(setq inhibit-startup-message t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      initial-scratch-message nil scroll-preserve-screen-position nil
      scroll-conservatively 101
      scroll-margin 20
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default line-spacing '0.4)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(add-to-list 'default-frame-alist '(font . "Source Code Pro 8"))

(global-set-key (kbd "C-x s") nil)
(global-set-key (kbd "C-x M-s") 'save-some-buffers)
(global-set-key (kbd "C-x C-o") nil)
(global-set-key (kbd "C-x M-o") 'delete-blank-lines)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C--") 'linum-mode)

(use-package diminish)

(use-package ace-window
  :config
  (global-set-key (kbd "M-m") 'ace-window))

(use-package evil
  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (setq-default evil-move-cursor-back nil))

(use-package evil-surround)

(use-package color)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package rainbow-delimiters
  :functions color-saturate-name
  :init
  (add-hook 'prog-mode-hook (lambda ()
                              (rainbow-delimiters-mode)))
  :config
  (defun rainbow-delimiters-saturate (percent)
    "Saturate rainbow delimiters by specified percentage."
    (interactive "nPercentage: ")
    (dolist (index (number-sequence 1 rainbow-delimiters-max-face-count))
      (let* ((face (intern (format "rainbow-delimiters-depth-%d-face" index)))
             (old-color (face-foreground face)))
        (unless (null old-color)
          (let ((new-color (color-saturate-name old-color percent)))
            (set-face-foreground face new-color))))))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq rainbow-delimiters-outermost-only-face-count 1)
              (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                                  :foreground 'unspecified
                                  :weight 'bold))))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :init (global-whitespace-mode t)
  :config (setq whitespace-style '(face tab-mark lines-tail trailing)
                whitespace-line-column 100
                ;; avoid highlighting large tables in org-mode
                whitespace-global-modes '(not latex-mode org-mode web-mode mhtml-mode)))

(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :init
  (use-package helm-c-yasnippet :ensure t)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(use-package smart-mode-line
   :functions (sml/faces-from-theme
               sml/theme-p)
   :init
   (setq sml/no-confirm-load-theme t
         sml/theme 'respectful)
   :commands sml/setup)

(use-package git-gutter
  :diminish git-gutter-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package fringe-helper)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package edit-indirect)
(use-package eldoc :diminish eldoc-mode)
(use-package undo-tree :diminish undo-tree-mode)
(use-package dockerfile-mode)
(use-package magit)
(use-package material-theme)
(use-package paredit :pin "melpa")
(use-package evil-paredit :pin "melpa")
(use-package smooth-scrolling)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (load-theme 'material t)))
  (load-theme 'material t))

(global-git-gutter-mode)
(yas-global-mode)
(global-hl-line-mode 1)
(which-function-mode 1)
(sml/setup)

(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

(provide 'init-core)
