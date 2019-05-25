(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


(package-initialize)

(unless (package-installed-p 'use-package)
;;  (add-to-list 'package-pinned-packages '(use-package . "melpa-stable") t)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-pin "melpa-stable"
      use-package-always-ensure t)

(provide 'init-packages)
