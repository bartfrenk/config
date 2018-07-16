(require 'haskell-mode)
(require 'helm)

;; Somehow this silence the compiler warning
(declare-function format-spec "elisp")

;;; Generate local documentation by `stack hoogle -- generate --local`

;;; Adapted from haskell-hoogle to work with stack

(defvar hs-hoogle/command "stack hoogle -- %q --color %f"
  "The Hoogle command. The placeholder %q is replaced by the
  query term, while the placeholder %f is replaced by flags.")


(defun hs-hoogle/run (query &rest flags)
  (let* ((flags (apply #'concat flags))
         (quoted (shell-quote-argument query))
         (cmd (format-spec hs-hoogle/command `((?q . ,quoted)
                                            (?f . ,flags)))))
    (with-help-window "*hoogle*"
      (with-current-buffer standard-output
        (insert (shell-command-to-string cmd))
        (ansi-color-apply-on-region (point-min) (point-max))))))


(defun hs-hoogle/search (query &optional info)
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
  (hs-hoogle/run query))


(defun hs-hoogle/info ()
  (interactive
   (let ((term (haskell-ident-at-point)))
     (if (and term (symbolp term)) (setq term (symbol-name term)))
     (hs-hoogle/run term "--info"))))


;;; Adapted from helm-hoogle to work with stack

(defun hs-hoogle/helm-set-candidates (&optional request-prefix)
  (let* ((pattern (or (and request-prefix
                           (concat request-prefix
                                   " " helm-pattern))
                      helm-pattern))
         (short-pattern
          (if (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\) " pattern)
              (match-string 1 pattern)
            pattern))
         (lim helm-candidate-number-limit)
         (args (append (list "hoogle" "--" "search" "-l")
                       (and lim (list "-n" (int-to-string lim)))
                       (list short-pattern))))
    (let (candidates)
      (with-temp-buffer
        (apply #'call-process "stack" nil t nil args)
        (run-hooks nil)
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "\\(.+?\\) -- \\(.+\\)")
              (push (cons (match-string 1)
                          (match-string-no-properties 2))
                    candidates))
          (forward-line 1)))
      (nreverse candidates))))

(defun hs-hoogle/helm ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Hoogle"
          :candidates #'hs-hoogle/helm-set-candidates
          :action '(("Lookup Entry" . browse-url))
          :filtered-candidate-transformer (lambda (candidates source) candidates)
          :volatile t)
        :prompt "Hoogle: "
        :buffer "*Hoogle search*"
        :input (haskell-ident-at-point)))

(provide 'hs-hoogle)

