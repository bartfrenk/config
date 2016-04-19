(require 'semantic)
(require 'semantic/bovine/gcc)

(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects.el" cache-dir))

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(semantic-mode 1)
(global-ede-mode t)
(ede-enable-generic-projects)

(provide 'settings-cedet)
