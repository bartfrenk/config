;; TODO: move layout from init-core here, to avoid switching layout during initialization.
(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))
(require 'functions (expand-file-name "~/.emacs.d/init/functions.el"))

(add-to-list 'load-path init-dir)

(package-initialize)

; silence 'tramp-read-passwd and 'find-tag-noselect redefinition warnings
(setq ad-redefinition-action 'accept)
(let ((modules '(packages
                 core
                 projectile
                 helm
                 flycheck
                 company
                 elisp
                 org
                 python
                 clojure
                 haskell)))
 (dolist (module modules)
   (let ((name (symbol-name module))
         (prefix "init-"))
      (message name)
      (require (intern (concat prefix name))))))

; GNU TLS gives a fatal error when connecting to marmalade
; Only an issue when compiled with GNU TLS support
(defun gnutls-available-p ()
  "Function redefined in order not to use built-in GnuTLS support"
  nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((haskell-process-args-ghci "ghci")
     (haskell-process-path-ghci . "stack")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
