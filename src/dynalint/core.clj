(ns dynalint.core
  (:require [clojure.repl :as repl]
            [io.aviso.repl :as pretty]
            [clojure.string :as str]))

(def corrupt-vars (atom #{}))

(def ^:dynamic *altering-var* nil)

(defn alter-original-var-root
  "Takes a var and passes its original
  root binding to f, which should return the
  new root binding."
  [v f]
  (letfn [(already-linted? [v] 
            (boolean
              (-> v meta ::lint)))
          (prepare-root [root-fn original]
            (with-meta
              (root-fn original)
              {::lint true
               ::original original}))
          (original-root [v]
            (-> v meta ::original))]
    (when (@corrupt-vars v)
      (prn "WARNING: " v " root binding is corrupt!"))
    (add-watch v
      ::linter
      (fn [& args]
        (when-not *altering-var*
          (swap! corrupt-vars conj v))))
    (binding [*altering-var* true]
      (alter-var-root 
        v
        (fn [root-val]
          (if (already-linted? root-val)
            (prepare-root f (original-root root-val))
            (prepare-root f root-val)))))))

(defn relevant-stacktrace []
  (try (throw (Exception. ""))
       (catch Exception e
         (let [epst (with-out-str
                      (binding [*err* *out*]
                        (repl/pst e)))]
           (->>
             epst
             str/split-lines
             (filter (fn [^String s]
                       (.contains s (str (ns-name *ns*) "/"))))
             doall)))))

(defn print-relevant-stacktrace []
  (doseq [r (relevant-stacktrace)]
    (println r))
  (flush))

(defn warn [& args]
  (println "WARNING: " (apply str args))
  (flush))

(defn short-ds [ds]
  (with-out-str
    (binding [*print-length* 3
              *print-level*  3]
      (prn ds))))

(defn rel? [a]
  (and (set? a)
       (every? map? a)))

(def lint-vars
  {#'clojure.core/keys
    (fn [original]
      (fn [map]
        (when-not ((some-fn nil? map?) map)
          (warn "Calling clojure.core/keys with non-map: "
                (short-ds map))
          (print-relevant-stacktrace))
        (original map)))
   #'clojure.core/vals
    (fn [original]
      (fn [& [map :as all]]
        (when-not ((some-fn nil? map?) map)
          (warn "Calling clojure.core/vals with non-map: "
                (short-ds map))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.core/update-in
    (fn [original]
      (fn [& [m ks f & args :as all]]
        (when-not (seq ks)
          (warn "clojure.core/update-in key path should be non-empty: "
                (short-ds ks))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.core/assoc-in
    (fn [original]
      (fn [& [m ks v :as all]]
        (when-not (seq ks)
          (warn "clojure.core/assoc-in key path should be non-empty: "
                (short-ds ks))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.core/get-in
    (fn [original]
      (fn [& [m ks :as all]]
        (when-not (seq ks)
          (warn "clojure.core/get-in key path should be non-empty: "
                (short-ds ks))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.core/select-keys
    (fn [original]
      (fn [& [m keyseq :as all]]
        (when-not (map? m)
          (warn "clojure.core/select-keys first argument should be a map: "
                (short-ds m))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/union
    (fn [original]
      (fn [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/union should have set arguments: "
                  (short-ds s))
            (print-relevant-stacktrace)))
        (apply original all)))
   #'clojure.set/intersection
    (fn [original]
      (fn [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/intersection should have set arguments: "
                  (short-ds s))
            (print-relevant-stacktrace)))
        (apply original all)))
   #'clojure.set/difference
    (fn [original]
      (fn [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/difference should have set arguments: "
                  (short-ds s))
            (print-relevant-stacktrace)))
        (apply original all)))
   #'clojure.set/select
    (fn [original]
      (fn [& [pred xset :as all]]
        (when-not (set? xset)
          (warn "clojure.set/select should have set for second argument: "
                (short-ds xset))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/project
    (fn [original]
      (fn [& [xrel ks :as all]]
        (when-not (map? xrel)
          (warn "clojure.set/project first argument should be map: "
                (short-ds xrel))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/rename-keys
    (fn [original]
      (fn [& [m kmap :as all]]
        (when-not (map? m)
          (warn "clojure.set/rename-keys first argument should be map: "
                (short-ds m))
          (print-relevant-stacktrace))
        (when-not (map? kmap)
          (warn "clojure.set/rename-keys second argument should be map: "
                (short-ds kmap))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/rename
    (fn [original]
      (fn [& [xrel kmap :as all]]
        (when-not (rel? xrel)
          (warn "clojure.set/rename first argument should be a relation: "
                (short-ds xrel))
          (print-relevant-stacktrace))
        (when-not (map? kmap)
          (warn "clojure.set/rename second argument should be a map: "
                (short-ds kmap))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/index
    (fn [original]
      (fn [& [xrel ks :as all]]
        (when-not (rel? xrel)
          (warn "clojure.set/index first argument should be a relation: "
                (short-ds xrel))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/map-invert
    (fn [original]
      (fn [& [m :as all]]
        (when-not (map? m)
          (warn "clojure.set/map-invert first argument should be map: "
                (short-ds m))
          (print-relevant-stacktrace))
        (apply original all)))
   #'clojure.set/join
    (fn [original]
      (fn [& [xrel yrel km :as all]]
        (when (#{2 3} (count all))
          (when-not (rel? xrel)
            (warn "clojure.set/join first argument should be a relation: "
                  (short-ds xrel))
            (print-relevant-stacktrace))
          (when-not (rel? yrel)
            (warn "clojure.set/join second argument should be a relation: "
                  (short-ds yrel))
            (print-relevant-stacktrace)))
        (when (#{3} (count all))
          (when-not (map? km)
            (warn "clojure.set/join third argument should be a map: "
                  (short-ds km))
            (print-relevant-stacktrace)))
        (apply original all)))
   #'clojure.set/subset?
    (fn [original]
      (fn [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/subset? should have set arguments: "
                  (short-ds s))
            (print-relevant-stacktrace)))
        (apply original all)))
   #'clojure.set/superset?
    (fn [original]
      (fn [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/superset? should have set arguments: "
                  (short-ds s))
            (print-relevant-stacktrace)))
        (apply original all)))

   })

(defn lint []
  (doseq [[v f] lint-vars]
    (alter-original-var-root v f)))
    
(comment
  (lint)
  )
