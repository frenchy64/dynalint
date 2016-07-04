(ns dynalint.lang
  (:require
    [dynalint.lint :as dyna]
    [clojure.core.typed.lang :as lang]))

(defn dynalint-load [base-resource-path]
  (clojure.lang.RT/load base-resource-path))

(defn install-lang []
  (alter-var-root #'lang/lang-dispatch
                  assoc :dynalint #'dynalint-load)
  (lang/monkey-patch-extensible-load))
