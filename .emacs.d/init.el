(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))
(require 'functions (expand-file-name "~/.emacs.d/init/functions.el"))

(setq-default custom-file (expand-file-name "custom.el" init-dir))
(load custom-file 'noerror)

(add-to-list 'load-path init-dir)

(let ((modules '(packages
                 core
                 magit
                 projectile
                 helm
                 flycheck
                 company
                 elisp
                 javascript
                 org
                 python
                 clojure
                 haskell-intero
                 erlang
                 elixir
                 jinja2
                 rust
                 rst
                 idris
                 plantuml
                 go
                 html
                 coq
                 tla
                 sh
                 sql
                 maxima
                 groovy
                 antlr
                 scala
                 agda
                 fsharp
                 purescript)))

  (dolist (module modules)
    (let ((name (symbol-name module))
          (prefix "init-"))
      (message name)
      (require (intern (concat prefix name))))))


(message "local")
;; do not fail if 'init-local is not provided
(require 'init-local nil :no-error)


