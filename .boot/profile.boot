(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.15.0-SNAPSHOT"]
                [refactor-nrepl "2.3.0-SNAPSHOT"]]
       )

(swap! boot.repl/*default-middleware*
       conj '[cider.nrepl/cider-middleware
              refactor-nrepl.middleware/wrap-refactor])
