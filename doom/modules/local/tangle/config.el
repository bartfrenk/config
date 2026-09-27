;;; local/tangle/config.el -*- lexical-binding: t; -*-

(defun heading-slug (title)
  "Turn TITLE into a file name: \"Ábrete Corazon\" -> \"abrete-corazon\"."
  (require 'ucs-normalize)
  (let* ((s (ucs-normalize-NFD-string title))
         (s (replace-regexp-in-string "[̀-ͯ]" "" s)))
    (string-trim (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase s)) "-" "-")))

(defun heading-tangle-path (folder extension)
  "Return FOLDER/<slug>.EXTENSION, where <slug> comes from the current heading."
  (concat (file-name-as-directory folder)
          (heading-slug (org-get-heading t t t t))
          "." extension))
