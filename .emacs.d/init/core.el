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
  :init (global-whitespace-mode t)
  :config (setq whitespace-style '(face empty tabs lines-tail trailing)
                whitespace-line-column 100
                ;; avoid highlighting large tables in org-mode
                whitespace-global-modes '(not latex-mode org-mode)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  (global-set-key (kbd "C-c y") 'helm-yas-complete))

(use-package linum
  :ensure t
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



(provide 'core)
