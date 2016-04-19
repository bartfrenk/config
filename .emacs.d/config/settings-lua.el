(require 'evil)
(require 'use-package)


(use-package lua-mode
  :init
  (add-hook 'lua-mode-hook '(lambda() (setq tab-width 2
                                            evil-shift-width 2)))
  :config
  (setq lua-indent-level 2
        lua-documentation-function 'browse-url
        lua-documentation-url "http://pgl.yoyo.org/luai/i/"
        lua-always-show t))

;  (bind-key (kbd "C-c C-`") 'lua-show-process-buffer lua-mode-map)
;  (bind-key (kbd "C-c C-l") 'lua-send-buffer lua-mode-map)
;  (define-key lua-mode-map (kbd "C-c C-k") 'lua-send-current-line)
;  (define-key lua-mode-map (kbd "C-c C-f") 'lua-send-defun)
;  (bind-key (kbd "C-c C-h") 'lua-search-documentation lua-mode-map))
(provide 'settings-lua)
