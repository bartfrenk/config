;; The Aven CLI interface itself lives in a standalone package at
;; ~/code/projects/aven-emacs (see doom/packages.el for the straight.el
;; recipe). This module just loads it and wires it into Doom's leader
;; key; `aven--executable' and friends become available to other
;; modules (e.g. `gtd-aven') once this has run.

(use-package! aven
  :demand t
  :config
  (map! :leader
        :desc "Aven"
        "n g t" #'aven/status))
