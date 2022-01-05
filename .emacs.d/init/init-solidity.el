(require 'use-package)

(use-package solidity-mode)

(use-package company-solidity
  :after solidity-mode)

(use-package solidity-flycheck
  :after solidity-mode)
