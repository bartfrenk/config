(require 'init-constants (expand-file-name "~/.emacs.d/init/init-constants.el"))

(add-to-list 'load-path init-dir)

(package-initialize)

(let ((modules '(packages core functions projectile helm)))
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
