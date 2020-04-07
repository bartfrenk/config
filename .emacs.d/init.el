(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))
(require 'functions (expand-file-name "~/.emacs.d/init/functions.el"))

(setq-default custom-file (expand-file-name "custom.el" init-dir))
(load custom-file 'noerror)

(add-to-list 'load-path init-dir)

(defvar core/modules
  '(packages
    core
    magit
    projectile
    helm
    flycheck
    company))

(defvar core/languages
  '(org
    python
    clojure
    elisp
    go
    haskell
    javascript
    rust
    jinja2
    rst
    sql
    html
    sh))

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
