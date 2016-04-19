(require 'use-package)

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/") t))

(provide 'settings-packages)
