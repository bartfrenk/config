; TODO: split off company related config in a seperate file
(require 'company)
(require 'help-at-pt)
(with-no-warnings (require 'cl))

(require 'eclimd)
(require 'use-package)

(use-package eclim
  :init
  (setq eclim-eclipse-dirs '("/opt/eclipse/eclim")
        eclimd-executable "/opt/eclipse/eclim/eclimd")
  :config
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (setq company-backends (remove-if
                          (lambda (b)
                            (find b '(company-nxml company-eclim)))
                          company-backends))
  :bind
  (("M-]" . eclim-problems-next)
   ("M-[" . eclim-problems-previous)))


(add-hook 'java-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends)
                                 '((company-dabbrev-code
                                    company-gtags
                                    company-etags
                                    company-keywords)))))

(add-hook 'eclim-mode-hook 'setup-emacs-eclim)

(defun setup-emacs-eclim ()
  "Add company-emacs-eclim to company backends when it is not already there."
  (progn
    (require 'company-emacs-eclim)
    (set (make-local-variable 'company-backends) '(company-emacs-eclim))
    (semantic-mode 0)
  ))

(provide 'settings-java)
