(require 'boot.repl)

;; (swap! boot.repl/*default-dependencies*
;;        concat '[[cider/cider-nrepl "0.15.0-SNAPSHOT"]
;;                 [refactor-nrepl "2.3.0-SNAPSHOT"]])

;; (swap! boot.repl/*default-middleware*
;;        conj '[cider.nrepl/cider-middleware
;;               refactor-nrepl.middleware/wrap-refactor])

(swap! boot.repl/*default-dependencies*
       concat '[[criterium "0.4.4" :scope "test"]
                [pjstadig/humane-test-output "0.8.3" :scope "test"]])


(configure-repositories!
  (fn [{:keys [url] :as repo-map}]
    (->> (condp re-find url
           #"^http://maven.scientiamobile.com:8081/nexus/content/repositories/private-repository"
           {:username (System/getenv "SCIENTIAMOBILE_USER" )
            :password (System/getenv "SCIENTIAMOBILE_PASSWORD")}
           #".*" nil)
         (merge repo-map))))
