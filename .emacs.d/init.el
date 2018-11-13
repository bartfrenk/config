(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))
(require 'functions (expand-file-name "~/.emacs.d/init/functions.el"))

(setq-default custom-file (expand-file-name "custom.el" init-dir))
(load custom-file 'noerror)

(add-to-list 'load-path init-dir)

(package-initialize)

; silence 'tramp-read-passwd and 'find-tag-noselect redefinition warnings
; (setq ad-redefinition-action 'accept)
(let ((modules '(packages
                 core
                 projectile
                 helm
                 flycheck
                 company
                 elisp
                 javascript
                 org
                 python-pyenv
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
                 maxima
                 groovy
                 antlr
                 purescript)))

  (dolist (module modules)
    (let ((name (symbol-name module))
          (prefix "init-"))
      (message name)
      (require (intern (concat prefix name))))))


(message "local")
;; do not fail if 'init-local is not provided
(require 'init-local nil :no-error)

; GNU TLS gives a fatal error when connecting to marmalade
; Only an issue when compiled with GNU TLS support
(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
