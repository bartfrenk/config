(require 'diminish)


(load "~/.emacs.d/site/emacs-sql-indent/sql-indent.el" t t)
(add-hook 'sql-mode-hook (lambda () (sqlind-minor-mode t)))
(diminish 'sqlind-minor-mode)

(provide 'init-sql)
