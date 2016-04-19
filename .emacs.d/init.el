(menu-bar-mode -1)
(require 'settings-globals "/home/bart/.emacs.d/config/settings-globals.el")


(setq-default custom-file (expand-file-name "settings-custom.el" config-dir))
(load custom-file 'noerror)
(package-initialize)
(add-to-list 'load-path config-dir)
(add-to-list 'load-path local-dir)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

; silence 'tramp-read-passwd and 'find-tag-noselect redefinition warnings
(setq ad-redefinition-action 'accept)
(let ((modules '(packages misc helm projectile
                          company flycheck cvs
                          lisp c++ java scala clojure
                          python haskell lua ess
                          octave latex org)))
  (dolist (elt modules)
    (let ((name (symbol-name elt))
          (prefix "settings-"))
      (message (symbol-name elt))
      (require (intern (concat prefix name))))))
