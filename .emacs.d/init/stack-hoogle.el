(require 'haskell-mode)

;; TODO: adapt helm-hoogle to work with stack

;; Generate local documentation by `stack hoogle -- generate --local`

;; Adapted from haskell-hoogle to work with stack
(defun stack-hoogle (query &optional info)
  "Do a Hoogle search

If prefix argument INFO is given, then `haskell-hoogle-command'
is asked to show extra info for the items matching QUERY.."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def)
           current-prefix-arg)))
  (let ((command (concat "stack hoogle " (shell-quote-argument query) " -- "
                         (if info "--info " "")
                         "--color " )))
    (with-help-window "*hoogle*"
      (with-current-buffer standard-output
        (insert (shell-command-to-string command))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(defun stack-hoogle-info ()
  "Shows extended hoogle documentation."
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'stack-hoogle))

(provide 'stack-hoogle)
