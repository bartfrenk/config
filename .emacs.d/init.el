(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))
(require 'functions (expand-file-name "~/.emacs.d/init/functions.el"))

(setq-default custom-file (expand-file-name "custom.el" init-dir))
(load custom-file 'noerror)

(add-to-list 'load-path init-dir)

(defvar core/modules
  '(packages
    core
    projectile
    helm
    company
    flycheck
    magit))

(defvar core/languages
  '(org
    python
    elisp
    jinja2
    sql
    html
    sh))

(defvar archived/languages
  '(rust
    go
    clojure
    haskell
    rst
    javascript))

(dolist (module core/modules)
  (let ((name (symbol-name module))
        (prefix "init-"))
    (message name)
    (require (intern (concat prefix name)))))

(dolist (module core/languages)
  (let ((name (symbol-name module))
        (prefix "init-"))
    (message name)
    (require (intern (concat prefix name)))))
