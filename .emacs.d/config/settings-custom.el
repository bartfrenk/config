(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"])
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "05411251e1232959144334e4359f8af0931c6c1a2f3a109d0d9e6753b6dfecfe" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "0022e0b80aaf697a4dc41322d5270aff5c4dae342c09a559abb91fd2bc64e755" "70b9c3d480948a3d007978b29e31d6ab9d7e259105d558c41f8b9532c13219aa" "b7b2cd8c45e18e28a14145573e84320795f5385895132a646ff779a141bbda7e" "18aa799ae149ceed4e337310319ad612b4f2526f058f8fd8ee00b8d8d79ed678" "9e1e2e7590c2f443c09a3f6240a05ada53f06566a6873c37eeae10d13dc372c9" "47ac4658d9e085ace37e7d967ea1c7d5f3dfeb2f720e5dec420034118ba84e17" "f979e71b37e53197803f8ac8417ea98fa3b104f38da5dea2992b520871a58b35" default)))
 '(fci-rule-color "#232A2F")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(font-latex-fontify-sectioning 1.0)
 '(global-hl-line-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(hl-sexp-background-color "#efebe9")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(org-agenda-files
   (quote
    ("~/remote/Dropbox/documents/notes/computer-science/chronology.org")))
 '(projectile-known-projects-file "/home/bart/.emacs.d/cache/projectile-bookmarks.eld")
 '(safe-local-variable-values
   (quote
    ((haskell-process-args-ghci "ghci" "--test")
     (haskell-process-args-ghci "ghci --test")
     (flycheck-checker quote haskell-ghc)
     (flycheck-checker . "haskell-ghc")
     (haskell-process-args-ghci "ghci")
     (haskell-process-path-ghci . "stack")
     (haskell-process-args-ghci "-ferror-spans")
     (haskell-process-path-ghci . "ghci")
     (haskell-process-type quote ghci)
     (haskell-process-args-ghci "-ferror-spans" "-i.")
     (haskell-process-args-ghci "-ferror-spans" "-isrc")
     (haskell-process-args-ghci "-hide-package" "gtk")
     (haskell-process-args-ghci "-hide-packages" "gtk")
     (whitespace-style)
     (whitespace-line-column . 200)
     (flycheck-clang-include-path
      (list "/usr/include/lua5.1"))
     (flycheck-clang-include-path
      (("/usr/include/lua5.1")))
     (flycheck-clang-include-path
      ("/usr/include/lua5.1"))
     (flycheck-clang-include-path
      ("/usr/include/lua5.2"))
     (haskell-process-args-ghci "-ferror-spans" "-i/home/bart/.xmonad/lib")
     (haskell-process-args-ghci "-i/home/bart/.xmonad/lib")
     (flycheck-ghc-search-path
      ("/home/bart/.xmonad/lib"))
     (flycheck-ghc-search-path
      (quote
       ("/home/bart/.xmonad/lib")))
     (flycheck-ghc-search-path quote
                               ("/home/bart/.xmonad/lib"))
     (flycheck-clang-args "-std=c++11"))))
 '(scheme-program-name "mit-scheme")
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 90 :width normal))))
 '(haskell-literate-comment-face ((t (:inherit font-lock-doc-face))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-source-header ((t (:height 1.0))))
 '(whitespace-empty ((t nil))))
