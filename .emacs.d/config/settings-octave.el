(require 'use-package)

(defun octave-source-file (&optional file)
  "Parse and execute FILE in the inferior Octave process. This is done using
Octave's source function.  If FILE is nil then the variable
`buffer-file-name' is used instead."
  (interactive)
  (unless file
    (setq file buffer-file-name))
  (unless file
    (user-error "Current buffer has no file"))
  (setq file (expand-file-name file))
  (inferior-octave t)
  (with-current-buffer inferior-octave-buffer
    (setq file (file-relative-name file))
    (comint-send-string inferior-octave-process
                        (concat "source '" file "'\n"))
    (comint-send-string inferior-octave-process
                        (concat "display ('Sourced " file "')\n"))))

(use-package octave
  :config
  ; TODO: where is this defined?
  (setq inferior-octave-prompt ">> ")
  (bind-key (kbd "C-c C-i a") 'octave-source-file octave-mode-map))

  ; TODO: figure out how to make keybindings more consistent with e.g. lua-mode
  ;(bind-key (kbd "C-c C-l") 'octave-send-line octave-mode-map)
  ;(bind-key (kbd "C-c C-f") 'octave-send-defun octave-mode-map)
  ;(bind-key (kbd "C-c C-b") 'octave-send-block octave-mode-map)

(provide 'settings-octave)
