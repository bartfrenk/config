;; Emacs interface comes bundled with maxima, installed through apt
;; Start an imaxima repl by invoking 'imaxima'

(add-to-list 'load-path "/usr/share/maxima/5.37.2/emacs")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

(setq imaxima-fnt-size "Large")

(provide 'init-maxima)
