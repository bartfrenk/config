(require 'use-package)

(use-package kotlin-mode
  :pin "melpa")

(use-package flycheck-kotlin
  :config
  (add-to-list 'flycheck-checkers 'kotlin-ktlint t))

(use-package ob-kotlin
  :pin "melpa")
