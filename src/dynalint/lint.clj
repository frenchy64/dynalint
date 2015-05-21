(ns 
  ^{:core.typed {:collect-only true}}
  ^{:doc "The main namespace to load the linter. See `lint`."}
  dynalint.lint
  (:import (clojure.lang Var))
  (:refer-clojure :exclude [nil?])
  (:require [clojure.repl :as repl]
            [clojure.string :as str]
            [clojure.set :as set]
            #_[clojure.core.typed :as t]))

(when-not (#{"1.5.1" "1.6.0"} (clojure-version))
  (prn "WARNING: Dynalint is designed for Clojure 1.5.1 and 1.6.0, running "
       (clojure-version)))

;(t/tc-ignore
(set! *warn-on-reflection* true)
  ;)

(defmacro tc-ignore [& body]
  (assert (seq body))
  `(do :core.typed/tc-ignore
       ~@body))

;(t/ann dynalint-meta Any)
(def dynalint-meta ::dynalint-meta)

;(t/ann corrupt-vars (t/Atom1 (t/Set (Var Nothing Any))))
(def ^:private corrupt-vars (atom #{}))

(defn nil? [a]
  (clojure.lang.Util/identical a nil))

;(t/ann *altering-var* Any)
(def ^:dynamic ^:private *altering-var* nil)

(defn var->symbol [^clojure.lang.Var v]
  (symbol (str (ns-name (.ns v))) (str (.sym v))))

(def error-history
  (atom (sorted-map)))

(def warning-history
  (atom (sorted-map)))

(defn add-error [id e]
  (swap! error-history assoc id e))

(defn add-warning [id e]
  (swap! warning-history assoc id e))

(def id (atom 0))

(defn inc-id []
  (swap! id inc))

(declare prettify-stack-trace drop-until-stack-entry)

(defn error [& args]
  (let [id (inc-id)
        msg (apply str "ERROR " (str "(Dynalint id " id "): ") args)
        e (->
            (try
              (throw
                (ex-info
                  msg
                  {::dynalint true
                   ::error true
                   ::id id}))
              (catch clojure.lang.ExceptionInfo e
                e))
            ; we want dynalint.lint/error at the top
            (prettify-stack-trace 
              :process-entries (drop-until-stack-entry 'dynalint.lint/error)))]
    (add-error id e)
    (throw e)))

(defn print-error
  "Print information on a Dynalint error. If id
  is provided, print error with that Dynalint ID,
  otherwise default to the latest error. Use depth
  to specify the depth of the stack trace printed."
  ([]
   (when-let [[_ e] (last @error-history)]
     (repl/pst e)))
  ([id]
   (when-let [e (@error-history id)]
     (repl/pst e)))
  ([id depth]
   (when-let [e (@error-history id)]
     (repl/pst e depth))))

(defn print-warning
  "Print information on a Dynalint warning. If id
  is provided, print warning with that Dynalint ID,
  otherwise default to the latest warning. Use depth
  to specify the depth of the stack trace printed."
  ([]
   (when-let [[_ e] (last @warning-history)]
     (repl/pst e)))
  ([id]
   (when-let [e (@warning-history id)]
     (repl/pst e)))
  ([id depth]
   (when-let [e (@warning-history id)]
     (repl/pst e depth))))

(def default-dump-lint-history-filename (atom "target/dynalint-output/output"))
(def default-dump-lint-history-stacktrace-depth (atom 200))

(defn dump-lint-history
  ([]
   (dump-lint-history @default-dump-lint-history-filename))
  ([fname]
   (dump-lint-history fname @default-dump-lint-history-stacktrace-depth))
  ([fname depth]
   (.mkdirs (.getParentFile (clojure.java.io/file fname)))
   (when (.exists (clojure.java.io/file fname))
     (clojure.java.io/delete-file fname))
   (doseq [e (->> (concat (vals @error-history) (vals @warning-history))
                  (sort-by (comp :dynalint.lint/id ex-data)))]
     (spit fname
           (with-out-str
             (binding [*err* *out*]
               (repl/pst e depth)
               (prn)))
           :append true))
   (println "Output Dynalint results to" fname)
   (flush)))

(declare errors-disabled?)

(def ^:dynamic *disable-warnings-in* false)
(def ^:dynamic *disable-errors-in* false)

(defmacro with-disabled-linting 
  [& args]
  `(do
     (push-thread-bindings
       {#'*disable-warnings-in* true
        #'*disable-errors-in* true})
     (try
       (do ~@args)
       (finally
         (pop-thread-bindings)))))

(defmacro when-errors-enabled [& args]
  `(when-not (errors-disabled?)
     (with-disabled-linting
       ~@args)))

(defmacro when-warnings-enabled [& args]
  `(when-not (warnings-disabled?)
     (with-disabled-linting
       ~@args)))

(defmacro warn-if [p & args]
  `(when-warnings-enabled
     (when ~p
       (warn ~@args))))

(defmacro warn-if-not [p & args]
  `(when-warnings-enabled
     (when-not ~p
       (warn ~@args))))

(defmacro error-if [p & args]
  `(when-errors-enabled
     (when ~p
       (error ~@args))))

(defmacro error-if-not [p & args]
  `(when-errors-enabled
     (when-not ~p
       (error ~@args))))


(defn check-nargs*
  [expectedfn the-var args]
  (error-if-not (expectedfn (count args))
    "Wrong number of arguments to " (var->symbol the-var) ": "
    (count args)))

(defmacro check-nargs [& args]
  `(check-nargs* ~@args))

;(t/ann already-linted? [Any -> Boolean])
(defn ^:private already-linted?
  [v] 
  (boolean
    (-> v meta ::lint)))

;(t/ann unwrap-original [[[Any * -> Any] Any * -> [Any * -> Any]] Any * -> [Any * -> Any]])
(defn ^:no-check ^:private unwrap-original [root-fn original & args]
  (with-meta
    (apply root-fn original args)
    {::lint true
     ::original original}))

;(t/ann ^:no-check unlinted-original [[Any * -> Any] -> [Any * -> Any]])
(defn unlinted-original [v]
  (-> v meta ::original))

;(t/ann maybe-unwrap [[[Any * -> Any] Any * -> [Any * -> Any]] [Any * -> Any]
;                     Any * -> [Any * -> Any]])
(defn maybe-unwrap
  ([root-val f & args]
    (if (already-linted? root-val)
      (apply unwrap-original f (unlinted-original root-val) args)
      (apply unwrap-original f root-val args))))

;(t/ann alter-original-var-root
;       [(Var Nothing Any) [Any -> Any] -> Any])
(defn ^:private alter-original-var-root
  "Takes a var and passes its original
  root binding to f, which should return the
  new root binding."
  [v f & args]
    (when (@corrupt-vars v)
      (prn "DYNALINT WARNING: " v " root binding is corrupt!"))
    (add-watch v
      ::var-root-watcher
      (fn [& args]
        (when-not *altering-var*
          (swap! corrupt-vars conj v))))
    (binding [*altering-var* true]
      (apply alter-var-root v maybe-unwrap f args)))

;(t/ann ^:no-check alter-var-inlining!
;       [(Var Nothing Any) [Any -> Any] -> Any])
(defn ^:private alter-var-inlining! [v f & args]
  (letfn [(safe-unwrap [old-inline f]
            (assert old-inline (str "Dynalint internal error: No inlining for " v))
            (maybe-unwrap old-inline f))]
    (alter-meta! v update-in [:inline] safe-unwrap #(apply f % args))))

;(t/ann ^:no-check relevant-stacktrace [-> Any])
(defn ^:private relevant-stacktrace [& {:keys [verbose]}]
  (try (throw (Exception. ""))
       (catch Exception e
         (let [epst (with-out-str
                      (binding [*err* *out*]
                        (repl/pst e 25)))]
           (or
             (when-not verbose
               (->>
                 epst
                 str/split-lines
                 (filter (fn [^String s]
                           (.contains s (str (ns-name *ns*) "/"))))
                 doall
                 seq))
             (str/split-lines
               epst))))))

;used in macros
;(t/ann ^:no-check print-relevant-stacktrace [& :optional {:verbose Any} -> Any])
(defn ^:skip-wiki print-relevant-stacktrace [& {:keys [verbose]}]
  (doseq [r (relevant-stacktrace :verbose verbose)]
    (println r))
  (flush))

(defn prettify-stack-trace [^Throwable e & {:keys [process-entries]}]
  (letfn [(wrapper-marker? [^String s]
            {:pre [(or (nil? s) (string? s))]}
            ;(prn "wrapper-marker?" s)
            (when s
              (.startsWith s "wrapper")))
          (ste-for-var [^clojure.lang.Var v]
            {:pre [(var? v)]
             :post [(or (nil? %)
                        (instance? StackTraceElement %))]}
            (let [{:keys [ns name line file]} (meta v)]
              (let [file-name (when file 
                                (last (str/split file #"/")))]
                ;(prn "file-name" file-name)
                (when (and ns name line file-name)
                  (StackTraceElement.
                    (str (namespace-munge ns) "$" (.sym v))
                    (str 'invoke)
                    file-name
                    line)))))
          (process-ste [^StackTraceElement ste]
            {:pre [(instance? StackTraceElement ste)]
             :post [(or (nil? %)
                        (instance? StackTraceElement %))]}
            (let [clstr (.getClassName ste)
                  [nstr wrapper-of innermarker right-more :as allcls]
                  (->> (partition-by #{\$} clstr)
                       (map (partial apply str))
                       (remove #{"$"}))
                 ; _ (prn "in dynalint marker" allcls)
                 ; _ (prn "original clstr" clstr)
                 ; _ (prn "nstr" nstr)
                 ; _ (prn "wrapper-of" wrapper-of)
                 ; _ (prn "innermarker" innermarker)
                  ]
              (if-not (and (= "dynalint.lint" nstr)
                           (wrapper-marker? innermarker))
                ste
                (let [;_ (prn "is a wrapper..")
                      [munged-ns munged-name] (str/split wrapper-of #"_SLASH_")
                      demunged-ns (str (str/replace munged-ns #"_DOT_" "."))
                      demunged-name (str (repl/demunge munged-name))
                      wrapping-varsym (symbol demunged-ns demunged-name)
                      ;_ (prn "demunged varsym" wrapping-varsym)
                      v (when (find-ns (symbol demunged-ns))
                          (find-var wrapping-varsym))
                      ;_ (prn ".. of var" v)
                      ]
                  (if-not (var? v)
                    ste 
                    (cond
                      ; drop any inner calls inside a dynalint wrapper
                      (seq right-more) nil
                      :else (or (ste-for-var v)
                                ste)))))))]
    (doto e
      (.setStackTrace
        (into-array 
          StackTraceElement
          (let [original-st (.getStackTrace e)
                ;_ (prn "original-st" (map (comp :className bean) original-st))
                es (->> original-st
                     (map process-ste)
                     (remove nil?))]
            ((or process-entries identity) es)))))))

(defn drop-until-stack-entry [clsym]
  #(drop-while 
     (fn [^StackTraceElement el]
       (not= (repl/demunge (.getClassName el))
             (str clsym)))
     %))

(def ^clojure.lang.Atom disable-warnings 
  "If set to true, skip all warning checks installed by Dynalint.
  Defaults to false"
  (atom false))
(def ^clojure.lang.Atom disable-errors 
  "If set to true, skip all error checks installed by Dynalint.
  Defaults to false"
  (atom false))

(defn ^:private reset-globals! [opts val]
  (doseq [o opts]
    (case o
      :warn (reset! disable-warnings val)
      :error (reset! disable-errors val)
      :all (do (reset! disable-warnings val)
               (reset! disable-errors val))
      nil)))

(defn configure-linting!
  "Globally configure linting options, applied from left-to-right.
  Only has effect after loading the linter with (lint).
  
  Options
  - :enable    enable linting for the given options
  - :disable   enable linting for the given options
               - valid options: a collection or keyword consisting of
                 :warn   configure warnings
                 :error  configure errors
                 :all    configure all linting
  
  eg. ;Enable all linting
      (configure-linting! :enable :all)
      
      ;Disable all linting
      (configure-linting! :disable :all)

      ; Just enable errors and warnings
      (configure-linting! :disable :all, :enable [:error :warn])"
  [& opt]
  ;TODO complain on bad arguments
  (when (even? (count opt))
    (doseq [[k v] (partition 2 opt)]
      (let [v (if (keyword? v)
                [v]
                v)]
        (when (coll? v)
          (case k
            :enable (reset-globals! v false)
            :disable (reset-globals! v true)
            nil))))))

(defn errors-disabled? []
  (or 
    ; must be very careful calling c.c/deref here
    (.deref disable-errors)
    *disable-errors-in*))

(defn warnings-disabled? []
  (or 
    ; must be very careful calling c.c/deref here
    (.deref disable-warnings)
    *disable-warnings-in*))

(def last-warning-nano-time (atom Long/MIN_VALUE))

(def warning-interval
  "Miniumum number of seconds between warnings."
  (atom 1))

(defn should-throw-warning? []
  (let [curr (System/nanoTime)
        next-warning-time (+' (*' 1e9 @warning-interval) @last-warning-nano-time)]
    (if (< next-warning-time curr)
      (do (reset! last-warning-nano-time curr)
          true)
      false)))

;used in macros
;(t/ann warn [Any * -> Any])
(defn ^:skip-wiki warn [& args]
  (when (should-throw-warning?)
    (let [id (inc-id)
          msg (print-str "WARNING" (str "(Dynalint id " id "): ") 
                         (apply str args))
          e (->
              (try (throw (ex-info
                            msg
                            {::dynalint true
                             ::warning true
                             ::id id}))
                   (catch clojure.lang.ExceptionInfo e
                     e))
              ; we want dynalint.lint/warn at the top
              (prettify-stack-trace 
                :process-entries (drop-until-stack-entry 'dynalint.lint/warn)))]
      (add-warning id e)
      (println msg)
      (flush))))

;used in macros
;(t/ann ^:no-check short-ds [Any -> Any])
(defn ^:skip-wiki short-ds [ds]
  (with-out-str
    (binding [*print-length* 3
              *print-level*  3]
      (pr ds)
      (flush))))

;(t/ann rel? [Any -> Any])
(defn ^:private rel? [a]
  (and (set? a)
       (every? map? a)))

(defmacro throws-exception? [& body]
  `(try (do ~@body false)
        (catch Throwable _#
          true)))

;(t/ann seq-succeeds? [Any -> Any])
(defn seq-succeeds? [s]
  (or (instance? clojure.lang.Seqable s)
      (nil? s)
      (instance? Iterable s)
      (let [^Class c (class s)]
        (.isArray c))
      (instance? CharSequence s)
      (instance? java.util.Map s)))

(defn to-array-succeeds? [s]
  (or (nil? s)
      (instance? (Class/forName "[Ljava.lang.Object;")
                 s)
      (instance? java.util.Collection s)
      (instance? java.util.Map s)
      (string? s)
      (let [^Class c (class s)]
        (.isArray c))))
      
(defn ^:private check-kw-params [the-var opts validators]
  (if (and (errors-disabled?)
           (warnings-disabled?))
    opts
    (with-disabled-linting
      (let [vsym (var->symbol the-var)]
        (when-not (even? (count opts))
          (error "Uneven number of keyword parameters passed to " vsym))
        (let [ks (take-nth 2 opts)
              bad-ks (remove (set (keys validators)) ks)]
          (when (seq bad-ks)
            (warn "Undocumented keyword arguments to " vsym ": "
                  (apply str (mapv short-ds bad-ks)))))
        (let [kopts (apply hash-map opts)
              new-kvals
              (loop [[& [[k vfn] & vnext :as vall]] validators
                     new-kvals {}]
                (if vall
                  (recur vnext
                         (conj new-kvals 
                               [k (vfn (get kopts k) (contains? kopts k) kopts)]))
                  new-kvals))
              new-flat
              (loop [[& [[k v] :as flatopts]] (partition 2 opts)
                     new-flat []]
                (if flatopts
                  (recur (next flatopts)
                         (conj new-flat
                               k (if (contains? new-kvals k)
                                   (get new-kvals k)
                                   v)))
                  new-flat))
              ]
          new-flat)))))

; [Var -> [Any Any Map -> Any]]
(defn check-iref-meta-kw
  [the-var]
  (let [vsym (var->symbol the-var)]
    (fn 
      [m present? kmap]
      (when present?
        (cond
          ; false does not throw exception
          (false? m)
          (warn ":meta keyword argument to " vsym " should be nil or map: " 
                (short-ds m))
          (not ((some-fn nil? map?) m))
          (error ":meta keyword argument to " vsym " must be nil or map: "
                 (short-ds m))))
      m)))

(defn check-iref-validator-kw
  [this-var]
  (let [vsym (var->symbol this-var)]
    (fn [m present? kmap]
      (when present?
        (cond
          ; false does not throw exception
          (false? m)
          (warn ":validator keyword argument to " vsym " should be an ifn or nil: " 
                (short-ds m))
          (not ((some-fn nil? ifn?) m))
          (error ":validator keyword argument to " vsym " must be ifn or nil: "
                 (short-ds m))))
      m)))

; to avoid cyclic checking we keep track of
; the operations being checked on the current thread
;
; If the current var being checked is part of 
(def ^:private ^:dynamic *currently-linting* #{})

;(t/ann new-var-mappings (t/Map Var [[Any * -> Any] (Var Nothing Any) -> [Any * -> Any]]))
(def ^:private new-var-mappings
  {#'clojure.core/keys
    (fn clojure.core_SLASH_keys
      [original this-var]
      (fn wrapper
        [& [map :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? map)
          "First argument to clojure.core/keys must be seqable: "
          (short-ds map))
        (when-not ((some-fn nil? map?) map)
          (warn "Calling clojure.core/keys with non-map: "
                (short-ds map)))
        (apply original all)))
   #'clojure.core/vals
    (fn clojure.core_SLASH_vals
      [original this-var]
      (fn wrapper
        [& [map :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? map)
          "First argument to clojure.core/vals must be seqable: "
          (short-ds map))
        (when-not ((some-fn nil? map?) map)
          (warn "Calling clojure.core/vals with non-map: "
                (short-ds map)))
        (apply original all)))
   #'clojure.core/key
    (fn clojure.core_SLASH_key
      [original this-var]
      (fn wrapper
        [& [e :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (instance? java.util.Map$Entry e)
          "First argument to clojure.core/key must be a map entry: "
          (short-ds e))
        (original e)))
   #'clojure.core/val
    (fn clojure.core_SLASH_val
      [original this-var]
      (fn wrapper
        [& [e :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (instance? java.util.Map$Entry e)
          "First argument to clojure.core/val must be a map entry: "
          (short-ds e))
        (original e)))
   #'clojure.core/rseq
    (fn clojure.core_SLASH_rseq
      [original this-var]
      (fn wrapper
        [& [rev :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (reversible? rev)
          "First argument to clojure.core/rseq must be reversible: "
          (short-ds rev))
        (original rev)))
   #'clojure.core/name
    (fn clojure.core_SLASH_name
      [original this-var]
      (fn wrapper
        [& [x :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.Named %) string?) x)
          "First argument to clojure.core/name must be string or named: "
          (short-ds x))
        (original x)))
   #'clojure.core/namespace
    (fn clojure.core_SLASH_namespace
      [original this-var]
      (fn wrapper
        [& [x :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.Named %)) x)
          "First argument to clojure.core/namespace must be named: "
          (short-ds x))
        (original x)))
   #'clojure.core/remove-all-methods
    (fn clojure.core_SLASH_remove-all-methods
      [original this-var]
      (fn wrapper
        [& [multifn :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/remove-all-methods must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/remove-method
    (fn clojure.core_SLASH_remove-method
      [original this-var]
      (fn wrapper
        [& [multifn dispatch-val :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/remove-method must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/prefer-method
    (fn clojure.core_SLASH_prefer-method
      [original this-var]
      (fn wrapper
        [& [multifn dispatch-val-x dispatch-val-y :as all]]
        (check-nargs #{3} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/prefer-method must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/methods
    (fn clojure.core_SLASH_methods
      [original this-var]
      (fn wrapper
        [& [multifn dispatch-val-x dispatch-val-y :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/methods must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/get-method
    (fn clojure.core_SLASH_get-method
      [original this-var]
      (fn wrapper
        [& [multifn dispatch-val :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/get-method must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/prefers
    (fn clojure.core_SLASH_prefers
      [original this-var]
      (fn wrapper
        [& [multifn :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not ((some-fn #(instance? clojure.lang.MultiFn %)) multifn)
          "First argument to clojure.core/prefers must be a multimethod: "
          (short-ds multifn))
        (apply original all)))
   #'clojure.core/find-var
    (fn clojure.core_SLASH_find-var
      [original this-var]
      (fn wrapper
        [& [sym :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (symbol? sym)
          "First argument to clojure.core/find-var must be a symbol: "
          (short-ds sym))
        (error-if-not (namespace sym)
          "First argument to clojure.core/find-var must be namespace qualified: "
          (short-ds sym))
        (error-if-not (find-ns (symbol (namespace sym)))
          "First argument to clojure.core/find-var must have a namespace that exists"
          " (no such namespace " (namespace sym) "): "
          (short-ds sym))
        (apply original all)))
   #'clojure.core/agent
    (fn clojure.core_SLASH_agent
      [original this-var]
      (fn wrapper
        [& [i & opts :as all]]
        (check-nargs #(< 0 %) this-var all)
        (check-kw-params 
          this-var
          opts
          {:meta (check-iref-meta-kw this-var)
           :validator (check-iref-validator-kw this-var)
           :error-handler
           (fn [m present? kmap]
             (when present?
               (cond
                ; false does not throw exception
                (false? m)
                  (warn ":error-handler keyword argument to clojure.core/agent should be an ifn or nil: " 
                        (short-ds m))
                (not ((some-fn nil? ifn?) m))
                  (error ":error-handler keyword argument to clojure.core/agent must be ifn or nil: "
                         (short-ds m))))
             m)
           :error-mode
           (fn [m present? kmap]
             (when present?
               (cond
                 ; false and nil do not throw exceptions
                 (or (false? m)
                     (nil? m)
                     (and (keyword? m)
                          (not (#{:continue :fail} m))))
                   (warn ":error-mode keyword argument to clojure.core/agent should be keyword :continue or :fail: " 
                         (short-ds m))
                 (not (keyword? m))
                   (error ":error-mode keyword argument to clojure.core/agent must be keyword :continue or :fail: " 
                          (short-ds m))))
             m)
           })
        (apply original all)))
   #'clojure.core/ref
    (fn clojure.core_SLASH_ref
      [original this-var]
      (fn wrapper
        [& [i & opts :as all]]
        (check-nargs #(< 0 %) this-var all)
        (check-kw-params 
          this-var
          opts
          {:meta (check-iref-meta-kw this-var)
           :validator (check-iref-validator-kw this-var)
           :error-handler
           (fn [m present? kmap]
             (when present?
               (cond
                ; false does not throw exception
                (false? m)
                  (warn ":error-handler keyword argument to clojure.core/agent should be an ifn or nil: " 
                        (short-ds m))
                (not ((some-fn nil? ifn?) m))
                  (error ":error-handler keyword argument to clojure.core/agent must be ifn or nil: "
                         (short-ds m))))
             m)
           :error-mode
           (fn [m present? kmap]
             (when present?
               (cond
                 ; false and nil do not throw exceptions
                 (or (false? m)
                     (nil? m)
                     (and (keyword? m)
                          (not (#{:continue :fail} m))))
                   (warn ":error-mode keyword argument to clojure.core/agent should be keyword :continue or :fail: " 
                         (short-ds m))
                 (not (keyword? m))
                   (error ":error-mode keyword argument to clojure.core/agent must be keyword :continue or :fail: " 
                          (short-ds m))))
             m)
           })
        (apply original all)))
   #'clojure.core/set-agent-send-executor!
    (fn clojure.core_SLASH_set-agent-send-executor!
      [original this-var]
      (fn wrapper
        [& [exs :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (instance? java.util.concurrent.ExecutorService exs)
          "First argument to clojure.core/set-agent-send-off-executor! must be an executor service: "
          (short-ds exs))
        (apply original all)))
   #'clojure.core/set-agent-send-off-executor!
    (fn clojure.core_SLASH_set-agent-send-off-executor!
      [original this-var]
      (fn wrapper
        [& [exs :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (instance? java.util.concurrent.ExecutorService exs)
          "First argument to clojure.core/set-agent-send-off-executor! must be an executor service: "
          (short-ds exs))
        (apply original all)))
   #'clojure.core/dissoc
    (fn clojure.core_SLASH_dissoc
      [original this-var]
      (fn wrapper
        [& [m & ks :as all]]
        (check-nargs #(<= 1 %) this-var all)
        (when (or ; the real dissoc just returns for 1 argument
                  (and (not (map? m))
                       (empty? ks))
                  ; if nil is passed, dissoc never throws
                  (nil? m))
          (warn "clojure.core/dissoc first argument should be a map: "
                (short-ds m)))
        ; give a better error for more than 1 argument, this will always
        ; fail if given anything other than a map or nil
        (error-if (and (seq ks) (not ((some-fn map? nil?) m)))
          "clojure.core/dissoc first argument must be a map: "
          (short-ds m))
        (apply original all)))
   #'clojure.core/update-in
    (fn clojure.core_SLASH_update-in
      [original this-var]
      (fn wrapper
        [& [m ks f & args :as all]]
        (check-nargs #(<= 3 %) this-var all)
        (when-not (coll? ks)
          (warn "clojure.core/update-in key path does not look like a collection: "
                (short-ds ks)))
        (when (coll? ks)
          (when-not (seq ks)
            (warn "clojure.core/update-in key path should be non-empty: "
                  (short-ds ks))))
        (apply original all)))
   #'clojure.core/assoc-in
    (fn clojure.core_SLASH_assoc-in
      [original this-var]
      (fn wrapper
        [& [m ks v :as all]]
        (check-nargs #{3} this-var all)
        (warn-if (empty? ks) 
          "clojure.core/assoc-in key path should be non-empty: "
          (short-ds ks))
        (apply original all)))
   #'clojure.core/get-in
    (fn clojure.core_SLASH_get-in
      [original this-var]
      (fn wrapper
        [& [m ks :as all]]
        (check-nargs #{2 3} this-var all)
        (error-if-not (seq-succeeds? ks)
          "Second argument to clojure.core/get-in must be seqable: "
          (short-ds ks))
        (when ((every-pred seq-succeeds? empty?) 
               ks)
          (warn "clojure.core/get-in key path should be non-empty: "
                (short-ds ks)))
        (apply original all)))
   #'clojure.core/select-keys
    (fn clojure.core_SLASH_select-keys
      [original this-var]
      (fn wrapper
        [& [m keyseq :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (seq-succeeds? keyseq)
          "Second argument to clojure.core/select-keys must be seqable: "
          (short-ds keyseq))
        (when-not (instance? java.util.Map m)
          (if (or (nil? m) (empty? keyseq))
            (warn "clojure.core/select-keys first argument should be a map: "
                  (short-ds m))
            (error "clojure.core/select-keys first argument must be a map: "
                   (short-ds m))))
        (apply original all)))
   #'clojure.core/zipmap
    (fn clojure.core_SLASH_zipmap
      [original this-var]
      (fn wrapper
        [& [ks vs :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (seq-succeeds? ks)
          "First argument to clojure.core/zipmap must be seqable: "
          (short-ds ks))
        (error-if-not (seq-succeeds? vs)
          "Second argument to clojure.core/zipmap must be seqable: "
          (short-ds vs))
        (when-not ((some-fn sequential? nil?) ks vs)
          (warn "clojure.core/zipmap arguments should be sequential or nil: "
                (short-ds ks) ", " (short-ds vs)))
        (apply original all)))
   #'clojure.core/reverse
    (fn clojure.core_SLASH_reverse
      [original this-var]
      (fn wrapper[& [rev :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? rev)
          "First argument to clojure.core/reverse must be seqable: "
          (short-ds rev))
        (when (reversible? rev)
          (warn "clojure.core/reverse argument is reversible, consider clojure.core/rseq: "
                (short-ds rev)))
        (apply original all)))
   #'clojure.core/unchecked-inc
     (fn clojure.core_SLASH_unchecked-inc
       [original this-var]
       (fn wrapper
         [& [x :as all]]
         (check-nargs #{1} this-var all)
         (let [res (apply original all)]
           (when-not (< x res)
             (warn "clojure.core/unchecked-inc overflow detected: "
                   (short-ds x) " (" (class x) ")" " -> " (short-ds res)
                   " (" (class x) ")"))
           res)))
   #'clojure.core/unchecked-add
     (fn clojure.core_SLASH_unchecked-add
       [original this-var]
       (fn wrapper
         [& [x y :as all]]
         (check-nargs #{2} this-var all)
         ;FIXME should be inlined like unchecked-inc
         (let [res (cond
                     ; this case doesn't throw an exception, like the inline version.
                     ; This differs from the non-inline unchecked-add, which throws an
                     ; exception (it delegates to inc).
                     (and (instance? Long x)
                          (instance? Long y)) 
                       (let [^long x1 x
                             ^long y1 y]
                         ;(prn 'special-add)
                         (clojure.lang.Numbers/unchecked_add x y))
                     :else (clojure.lang.Numbers/unchecked_add x y))]
           (when-not (< (+ x y) res)
             (warn "clojure.core/unchecked-add overflow detected: "
                   (short-ds x) " (" (class x) ") + " 
                   (short-ds y) " (" (class y) ") -> " 
                   (short-ds res) " (" (class x) ")"))
           res)))
   #'clojure.set/union
    (fn clojure.set_SLASH_union
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/union should have set arguments: "
                  (short-ds s))))
        (apply original all)))
   #'clojure.set/intersection
    (fn clojure.set_SLASH_intersection
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (check-nargs #(<= 1 %) this-var all)
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/intersection should have set arguments: "
                  (short-ds s))))
        (apply original all)))
   #'clojure.set/difference
    (fn clojure.set_SLASH_difference
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (check-nargs #(<= 1 %) this-var all)
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/difference should have set arguments: "
                  (short-ds s))))
        (apply original all)))
   #'clojure.set/select
    (fn clojure.set_SLASH_select
      [original this-var]
      (fn wrapper
        [& [pred xset :as all]]
        (check-nargs #{2} this-var all)
        (when-not (set? xset)
          (warn "clojure.set/select should have set for second argument: "
                (short-ds xset)))
        (apply original all)))
   #'clojure.set/project
    (fn clojure.set_SLASH_project
      [original this-var]
      (fn wrapper
        [& [xrel ks :as all]]
        (check-nargs #{2} this-var all)
        (when-not (map? xrel)
          (warn "clojure.set/project first argument should be map: "
                (short-ds xrel)))
        (apply original all)))
   #'clojure.set/rename-keys
    (fn clojure.set_SLASH_rename-keys
      [original this-var]
      (fn wrapper
        [& [m kmap :as all]]
        (check-nargs #{2} this-var all)
        (when-not (map? m)
          (warn "clojure.set/rename-keys first argument should be map: "
                (short-ds m)))
        (when-not (map? kmap)
          (warn "clojure.set/rename-keys second argument should be map: "
                (short-ds kmap)))
        (apply original all)))
   #'clojure.set/rename
    (fn clojure.set_SLASH_rename
      [original this-var]
      (fn wrapper
        [& [xrel kmap :as all]]
        (check-nargs #{2} this-var all)
        (when-not (rel? xrel)
          (warn "clojure.set/rename first argument should be a relation: "
                (short-ds xrel)))
        (when-not (map? kmap)
          (warn "clojure.set/rename second argument should be a map: "
                (short-ds kmap)))
        (apply original all)))
   #'clojure.set/index
    (fn clojure.set_SLASH_index
      [original this-var]
      (fn wrapper
        [& [xrel ks :as all]]
        (check-nargs #{2} this-var all)
        (when-not (rel? xrel)
          (warn "clojure.set/index first argument should be a relation: "
                (short-ds xrel)))
        (apply original all)))
   #'clojure.set/map-invert
    (fn clojure.set_SLASH_map-invert
      [original this-var]
      (fn wrapper
        [& [m :as all]]
        (check-nargs #{1} this-var all)
        (when-not (map? m)
          (warn "clojure.set/map-invert first argument should be map: "
                (short-ds m)))
        (apply original all)))
   #'clojure.set/join
    (fn clojure.set_SLASH_join
      [original this-var]
      (fn wrapper
        [& [xrel yrel km :as all]]
        (check-nargs #{2 3} this-var all)
        (when (#{2 3} (count all))
          (when-not (rel? xrel)
            (warn "clojure.set/join first argument should be a relation: "
                  (short-ds xrel)))
          (when-not (rel? yrel)
            (warn "clojure.set/join second argument should be a relation: "
                  (short-ds yrel))))
        (when (#{3} (count all))
          (when-not (map? km)
            (warn "clojure.set/join third argument should be a map: "
                  (short-ds km))))
        (apply original all)))
   #'clojure.set/subset?
    (fn clojure.core_SLASH_subset?
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/subset? should have set arguments: "
                  (short-ds s))))
        (apply original all)))
   #'clojure.set/superset?
    (fn clojure.set_SLASH_superset?
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (when-let [non-sets (seq (filter (complement set?) all))]
          (doseq [s non-sets]
            (warn "clojure.set/superset? should have set arguments: "
                  (short-ds s))))
        (apply original all)))
   #'clojure.core/meta
    (fn clojure.core_SLASH_meta
      [original this-var]
      (fn wrapper
        [& [x m :as all]]
        (check-nargs #{1} this-var all)
        (apply original all)))
   #'clojure.core/with-meta
    (fn clojure.core_SLASH_with-meta
      [original this-var]
      (fn wrapper
        [& [x m :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (instance? clojure.lang.IMeta x)
          "First argument to clojure.core/with-meta must implement clojure.lang.IMeta: "
          (short-ds x))
        (error-if-not ((some-fn map? nil?) m)
          "Second argument to clojure.core/with-meta must be a map or nil: "
          (short-ds m))
        (apply original all)))
   #'clojure.core/last
    (fn clojure.core_SLASH_last
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/last must be seqable: "
          (short-ds the-seq))
        (original the-seq)))
   #'clojure.core/butlast
    (fn clojure.core_SLASH_butlast
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/butlast must be seqable: "
          (short-ds the-seq))
        (original the-seq)))
   #'clojure.core/cons
    (fn clojure.core_SLASH_cons 
      [original this-var]
      (fn wrapper
        [& [x the-seq :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "Second argument to clojure.core/cons must be seqable: "
          (short-ds the-seq)
          " (instance of " (class the-seq) ")")
        (apply original all)))
   #'clojure.core/first
    (fn clojure.core_SLASH_first
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/first must be seqable: "
          (short-ds the-seq)
          " (instance of " (class the-seq) ")")
        (original the-seq)))
   #'clojure.core/second
    (fn clojure.core_SLASH_second
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/second must be seqable: "
          (short-ds the-seq))
        (original the-seq)))
   #'clojure.core/ffirst
    (fn clojure.core_SLASH_ffirst
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/ffirst must be seqable: "
          (short-ds the-seq))
        (error-if-not (seq-succeeds? (first the-seq))
          "First argument to clojure.core/ffirst must have seqable first element: "
          (short-ds (first the-seq)))
        (original the-seq)))
   #'clojure.core/nfirst
    (fn clojure.core_SLASH_nfirst
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/nfirst must be seqable: "
          (short-ds the-seq))
        (error-if-not (seq-succeeds? (first the-seq))
          "First argument to clojure.core/nfirst must have seqable first element: "
          (short-ds (first the-seq)))
        (original the-seq)))
   #'clojure.core/fnext
    (fn clojure.core_SLASH_fnext
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/fnext must be seqable: "
          (short-ds the-seq))
        (original the-seq)))
   #'clojure.core/nnext
    (fn clojure.core_SLASH_nnext
      [original this-var]
      (fn wrapper
        [& [the-seq :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? the-seq)
          "First argument to clojure.core/nnext must be seqable: "
          (short-ds the-seq))
        (original the-seq)))
   #'clojure.core/seq?
    (fn clojure.core_SLASH_seq?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   #'clojure.core/char?
    (fn clojure.core_SLASH_char?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   #'clojure.core/string?
    (fn clojure.core_SLASH_string?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   #'clojure.core/map?
    (fn clojure.core_SLASH_map?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   #'clojure.core/vector?
    (fn clojure.core_SLASH_vector?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   ; this is often a compile time check anyway
   #'clojure.core/instance?
    (fn clojure.core_SLASH_instance? 
      [original this-var]
      (fn wrapper
        [& [cls x :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (class? cls)
          "First argument to clojure.core/instance? must be a Class: "
          (short-ds cls))
        (apply original all)))
; apply uses seq, results in infinite cycles
   #'clojure.core/seq
    (fn clojure.core_SLASH_seq
      [original this-var]
      (fn wrapper
        [& [coll :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (seq-succeeds? coll)
          "First argument to clojure.core/seq must be seqable: "
          (short-ds coll))
        (original coll)))
   #'clojure.core/symbol
    (fn clojure.core_SLASH_symbol
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (check-nargs #{1 2} this-var all)
        (case (count all)
          1 
            (let [[s1] all]
              (when-not ((some-fn string? symbol?) s1)
                (error "First argument to clojure.core/symbol (with 1 argument) must be a string or symbol: "
                       (short-ds s1))))
          2 
            (let [[s1 s2] all]
              (when-not (string? s1)
                (error "First argument to clojure.core/symbol (with 2 arguments) must be a string: "
                       (short-ds s1)))
              (when-not (string? s2)
                (error "Second argument to clojure.core/symbol (with 2 arguments) must be a string: "
                       (short-ds s2)))))
        (apply original all)))
   #'clojure.core/cast
    (fn clojure.core_SLASH_cast
      [original this-var]
      (fn wrapper
        [& [c i :as all]]
        (check-nargs #{2} this-var all)
        (error-if-not (class? c)
          "First argument to clojure.core/cast must be a class: "
          (short-ds c))
        (original c i)))
   #'clojure.core/to-array
    (fn clojure.core_SLASH_to-array
      [original this-var]
      (fn wrapper
        [& [coll :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (to-array-succeeds? coll)
          "First argument to clojure.core/to-array must be a collection: "
          (short-ds coll))
        (apply original all)))
   #'clojure.core/vec
    (fn clojure.core_SLASH_vec
      [original this-var]
      (fn wrapper
        [& [coll :as all]]
        (check-nargs #{1} this-var all)
        (error-if-not (or (instance? java.util.Collection coll) (to-array-succeeds? coll))
          "First argument to clojure.core/vec must be a collection or array: "
          (short-ds coll))
        (original coll)))
   #'clojure.core/hash-map
    (fn clojure.core_SLASH_hash-map
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (error-if-not (even? (count all))
          "Must pass even number of arguments to clojure.core/hash-map, actual: "
          (count all))
        (apply original all)))
   #'clojure.core/sorted-map
    (fn clojure.core_SLASH_sorted-map
      [original this-var]
      (fn wrapper
        [& [:as all]]
        (error-if-not (even? (count all))
          "Must pass even number of arguments to clojure.core/sorted-map, actual: "
          (count all))
        (apply original all)))
   #'clojure.core/sorted-map-by
    (fn clojure.core_SLASH_sorted-map-by
      [original this-var]
      (fn wrapper
        [& [c & args :as all]]
        (check-nargs #(<= 1 %) this-var all)
        (error-if-not (instance? java.util.Comparator c)
          "First argument to clojure.core/sorted-map-by must be a comparator, actual: "
          (count args))
        (error-if-not (even? (count args))
          "Must pass even number of variable arguments to clojure.core/sorted-map-by, actual: "
          (count args))
        (apply original all)))
   #'clojure.core/sorted-set-by
    (fn clojure.core_SLASH_sorted-set-by
      [original this-var]
      (fn wrapper
        [& [c & args :as all]]
        (check-nargs #(<= 1 %) this-var all)
        (error-if-not (instance? java.util.Comparator c)
          "First argument to clojure.core/sorted-set-by must be a comparator, actual: "
          (count args))
        (apply original all)))
   #'clojure.core/find
    (fn clojure.core_SLASH_find
      [original the-var]
      (fn wrapper 
        [& [m k :as all]]
        (check-nargs #{2} the-var all)
        (error-if-not ((some-fn #(instance? java.util.Map %) nil?) m)
          "First argument to clojure.core/find must be a map or nil: " (short-ds m))
        (original m k)))
   #'clojure.core/nil?
    (fn clojure.core_SLASH_nil?
      [original this-var]
      (fn wrapper
        [& [a :as all]]
        (check-nargs #{1} this-var all)
        (original a)))
   #'clojure.core/disj
    (fn clojure.core_SLASH_disj
      [original the-var]
      (fn wrapper 
        [& [s & args :as all]]
        (check-nargs #(< 0 %) the-var all)
        (when-not ((some-fn set? nil?) s)
          (if (or (not args) (false? s))
            ; false doesn't throw a runtime error
            (warn "First argument to clojure.core/disj should be a set or nil: "
                  (short-ds s))
            (error "First argument to clojure.core/disj must be a set or nil: "
                   (short-ds s))))
        (apply original all)))
   #'clojure.core/conj
    (fn clojure.core_SLASH_conj
      [original the-var]
      (fn wrapper 
        [& [t a :as all]]
        (check-nargs #(<= 2 %) the-var all)
        (cond
          (not ((some-fn nil? coll?) t))
            (error "First argument to clojure.core/conj must be a persistent collection or nil: "
                   (short-ds t))
          (and (map? t)
               ((complement 
                  (some-fn 
                    #(instance? java.util.Map$Entry %)
                    (every-pred vector?
                                #(#{2} (count %)))
                    (every-pred seq-succeeds?
                                (partial every? #(instance? java.util.Map$Entry %)))
                    nil?))
                a))
            (error "Can only conj nil, a map entry, a vector pair or a seqable of map entries onto a map: "
                   (short-ds a)))
        (apply original all)))
   #'clojure.core/next
    (fn clojure.core_SLASH_next
      [original the-var]
      (fn wrapper 
        [& [coll :as all]]
        (check-nargs #{1} the-var all)
        (error-if-not (seq-succeeds? coll)
          "First argument to clojure.core/next must be seqable: "
          (short-ds coll))
        (original coll)))
   #'clojure.core/rest
    (fn clojure.core_SLASH_rest
      [original the-var]
      (fn wrapper 
        [& [coll :as all]]
        (check-nargs #{1} the-var all)
        (error-if-not (seq-succeeds? coll)
          "First argument to clojure.core/rest must be seqable: "
          (short-ds coll))
        (original coll)))
  ; TODO some complicated invariants and error conditions with reduce
;   #'clojure.core/reduce
;    (fn clojure.core_SLASH_reduce
;      [original the-var]
;      (fn wrapper
;        [& [:as all]]
;        (check-nargs #{2 3} the-var all)
;        (apply original all)))
   #'clojure.core/deref
    (fn clojure.core_SLASH_deref
      [original the-var]
      (fn wrapper 
        [& [r :as all]]
        ; should support 3 args also
        (check-nargs #{1 3} the-var all)
        (when (#{1} (count all))
          (when-not (or (instance? clojure.lang.IDeref r)
                        (instance? java.util.concurrent.Future r))
            (error "First argument to clojure.core/deref must be IDeref or a future: "
                   (short-ds r))))
        (when (#{3} (count all))
          (when-not (or (instance? clojure.lang.IBlockingDeref r)
                        (instance? java.util.concurrent.Future r))
            (error "First argument to clojure.core/deref must be IBlockingDeref or a future: "
                   (short-ds r))))
        (apply original all)))
   })

;(t/ann new-var-inlines (t/Map Var [[Any * -> Any] -> [Any * -> Any]]))
(def ^:private new-var-inlines
  {#'clojure.core/unchecked-inc
    (fn [original the-var]
      (fn [& [x :as all]]
        (error-if-not (#{1} (count all))
          "Wrong number of arguments to clojure.core/unchecked-inc: " (count all))
        (let [gx (gensym 'x)]
          `(let [~gx ~x
                 res# ~(original gx)]
             (tc-ignore
               (when-not (< ~gx res#)
                 (warn "clojure.core/unchecked-inc (inlining) overflow detected : "
                       (short-ds ~gx) " (" (class ~gx) ")" " -> " (short-ds res#)
                       " (" (class ~gx) ")")))
             ; help the type inference out
             ~(original gx)))))
   })

(def ^:dynamic *inside-lazy-seq* false)

(defn check-for-like-macro
  [vsym seq-exprs]
  (error-if (zero? (count seq-exprs))
    vsym " takes at least one binding form")
  (error-if-not (even? (count seq-exprs))
    vsym "takes an even number of forms in binding vector, "
    "given: " (count seq-exprs))
  (error-if (#{:let :while :when} (first seq-exprs))
    vsym "takes at least one binding form before a " (first seq-exprs)
    " modifier"))

(defn wrap-for-like-binding
  [vsym seq-exprs]
  (check-for-like-macro vsym seq-exprs)
  (loop [[& [[b i] & ne :as es]] (partition 2 seq-exprs)
         new-exprs []]
    (if es
      (cond
        (= :let b)
          (recur ne (conj new-exprs b i))
        (= :while b)
          (recur ne (conj new-exprs b i))
        (= :when b)
          (recur ne (conj new-exprs b i))
        (keyword? b)
          (error vsym " invalid keyword: " b)
        :else 
          (let [rhs `(let [gs# ~i]
                       (tc-ignore
                         (error-if-not (seq-succeeds? gs#)
                           '~vsym " initial binding must be seqable: "
                           (class gs#)))
                       gs#)]
            (recur ne (conj new-exprs b rhs))))
      new-exprs)))

; TODO check :or is a map
;(defn validate-destructure-syntax [s]
;  (cond
;    (vector? s)
;      (doall 
;        (map validate-destructure-syntax s))
;    (map? s)

(def ^:private new-macro-mappings
  {#'clojure.core/lazy-seq
    (fn clojure.core_SLASH_lazy-seq
      [original the-var]
      (fn wrapper 
        [&form &env & all]
        (original
          &form &env
          `(let [s# (do ~@all)]
             (tc-ignore
               (when-not (seq-succeeds? s#)
                 (binding [*inside-lazy-seq* true]
                   (error "clojure.core/lazy-seq argument must be seqable: "
                          (class s#)
                          "\n\n\t in:\n" '~&form))))
             s#))))
   #'clojure.core/let
    (fn clojure.core_SLASH_let
      [original the-var]
      (fn wrapper
        [&form &env & [bindings & body :as all]]
        (error-if-not (<= 1 (count all))
          "clojure.core/let takes at least 1 argument, given " (count all))
        ;TODO check destructuring
        (apply original
          &form
          &env
          bindings
          body)))
   #'clojure.core/for
    (fn clojure.core_SLASH_for
      [original the-var]
      (fn wrapper 
        [&form &env & [seq-exprs body-expr :as all]]
        (error-if-not (#{2} (count all))
          "clojure.core/for takes 2 arguments, given " (count all))
        (error-if-not (vector? seq-exprs)
          "clojure.core/for takes a vector as first argument, given: "
          (class seq-exprs))
        (let [new-exprs (wrap-for-like-binding
                          'clojure.core/for
                          seq-exprs)]
          (original
            &form
            &env
            new-exprs
            body-expr))))
   })

;(t/ann ^:no-check todo-var-inlines [-> Any])
(defn ^:private todo-var-inlines []
  (set/difference (set (->> (keys new-var-mappings)
                            (filter #(-> % meta (contains? :inline)) )))
                  (set (keys new-var-inlines))))

;(t/ann ^:no-check lint-var-mappings [-> Any])
(defn ^:private lint-var-mappings []
  (doseq [[v f] new-var-mappings]
    (alter-original-var-root v f v)))

;(t/ann ^:no-check lint-inline-vars [-> Any])
(defn ^:private lint-inline-vars []
  (doseq [[v f] new-var-inlines]
    (alter-var-inlining! v f v)))

;(t/ann ^:no-check lint-macros [-> Any])
(defn ^:private lint-macros []
  (doseq [[v f] new-macro-mappings]
    (alter-original-var-root v f v)))


;(t/ann lint [Any -> Any])
(defn lint 
  "Load the linter. Takes the same options as configure-linting!.
  
  Prefer calling configure-linting! to further configure options
  after initially loading the linter. Dynalint will attempt to reload
  itself cleanly on multiple calls to `lint`, but it may conflict
  with other monkey-patching libraries. A warning will be thrown if
  reloading fails."
  [& opts]
  (when opts
    (let [start-message (get (apply hash-map opts) :start-message)]
      (when start-message
        (if (= start-message :with-call-stack)
          (try
            (println "starting dynalint.lint/lint with call stack:")
            (throw (Exception.))
            (catch Exception e
              (clojure.repl/pst e 300)))
          (println "starting dynalint.lint/lint"))
        (flush))))
  (lint-macros)
  (lint-inline-vars)
  (lint-var-mappings)
  (when opts
    (apply configure-linting! opts))
  :ok)
    
(comment
  (lint)
  (todo-var-inlines)
  )

