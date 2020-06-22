(require 'use-package)
(require 'diminish)


(load "~/.emacs.d/site/emacs-sql-indent/sql-indent.el" t t)
(add-hook 'sql-mode-hook (lambda () (sqlind-minor-mode t)))
(diminish 'sqlind-minor-mode)

(use-package sqlformat
  :pin melpa
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

(provide 'init-sql)
