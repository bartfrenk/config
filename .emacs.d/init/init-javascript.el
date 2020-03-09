(require 'browse-url)
(require 'company)
(require 'helm-dash)

(defun javascript/setup-dash ()
  (setq-local helm-dash-docsets '("React" "JavaScript" "Bootstrap_4")))

(use-package skewer-mode
  :bind (:map skewer-mode-map
              ("C-c C-l" . skewer-load-buffer)
              ("C-M-x" . skewer-eval-defun))
  :commands skewer-setup
  :diminish skewer-mode)

(use-package js2-mode
  :bind (:map js2-mode-map
              ("M-]" . tern-find-definition)
              ("M-[" . tern-pop-find-definition)
              ("C-c C-l" . skewer-load-buffer)
              ("C-M-x" . skewer-eval-defun)
              ("C-c C-d" . helm-dash-at-point))
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook (lambda ()
                             (javascript/setup-dash)
                             (tern-mode t)
                             (prettier-js-mode)
                             (skewer-mode)
                             (js2-refactor-mode))))

;; Useful key binding: C-c C-m l t to log the identifier at point.
(use-package js2-refactor
  :diminish js2-refactor-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package tern
  :bind (:map tern-mode-keymap
              ("M-]" . tern-find-definition)
              ("M-[" . tern-pop-find-definition))
  :commands tern-mode
  :diminish tern-mode
  :config
  (define-key tern-mode-keymap (kbd "C-c C-d") nil))

(use-package company-tern)

;; Runs the hooks of its parent modes, among which js2-mode
(use-package rjsx-mode)

(use-package js-auto-format-mode)



(use-package prettier-js
  :pin "melpa"
  :config
  (let ((indent-level 4))
    (setq-default js-indent-level indent-level
                  js2-indent-level indent-level
                  jsx-indent-level indent-level
                  sgml-basic-offset indent-level
                  js2-basic-offset indent-level)))

(use-package add-node-modules-path
  :config
  (add-hook 'prettier-js-mode-hook 'add-node-modules-path))

(skewer-setup)

(provide 'init-javascript)

;; Silence warning about assignment to free variable helm-dash-docsets.
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
