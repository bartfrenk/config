(require 'constants (expand-file-name "~/.emacs.d/init/constants.el"))

(add-to-list 'load-path init-dir)

(package-initialize)

(let ((modules '(packages core functions)))
  (dolist (module modules)
    (let ((name (symbol-name module)))
      (message name)
      (require (intern name)))))
