(defproject fractagons "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.6.0"]]
  :aot          [fractagons.core]
  :main         fractagons.core
  :repl-options {
             ;; If nREPL takes too long to load it may timeout,
             ;; increase this to wait longer before timing out.
             ;; Defaults to 30000 (30 seconds)
             :timeout 120000
             }
)