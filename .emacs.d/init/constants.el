(defvar cache-dir (expand-file-name "~/.emacs.d/cache/")
  "This folder stores automatically generated persistent session files.")
(defvar init-dir (expand-file-name "~/.emacs.d/init/")
  "This folder stores configuration files.")
(defvar max-columns 100
  "The maximal number of columns that should be used in a buffer")

;; maybe also define shortcut keys here, e.g.
;; "C-c C-a" autoformat buffer key
;; "C-c C-l" source buffer
;; "M-]" goto-definition
;; "M-[" pop-definition
;; etc...

(provide 'constants)
