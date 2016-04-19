(require 'use-package)
(require 'font-latex)

(defun latex-mode-defaults ()
  "Hook for latex mode."
  (visual-line-mode t)
  (abbrev-mode +1))

(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning nil)

(use-package tex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-PDF-mode t
        TeX-brace-indent-level 0))

(use-package latex
  :init
  (add-hook 'LaTeX-mode-hook 'latex-mode-defaults)
  :config
  (setq LaTeX-indent-level 0
        LaTeX-item-indent 0))

(use-package company-auctex
  :config
  (company-auctex-init))

(provide 'settings-latex)
