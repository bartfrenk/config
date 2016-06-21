;; This avoids having multiple themes loaded at the same time,
;; which is the default behaviour of 'load-theme'.
(defun switch-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'init-functions)
