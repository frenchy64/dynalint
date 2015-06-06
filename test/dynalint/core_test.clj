(ns dynalint.core-test
  (:use clojure.test)
  (:refer-clojure :exclude [nil?])
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [dynalint.lint :as dyn :refer :all]))

; TODO each dynalint error should have a corresponding test for the
; "bad" error that would happen otherwise, with the linter disabled

(lint)

#_(deftest scratch
  (is
(do
  (do
(lint)

(defn foo []
  (keys (seq {1 2 3 4})))

(defn upd []
  (update-in {} [] identity))

(defn asoi []
  (assoc-in {} [] 1))

(defn gin []
  (get-in {} []))

(defn sunion []
  (set/union nil [1]))

(defn select []
  (set/select pos? [1 2 3]))

(defn sel-keys []
  (select-keys [] [1]))

(defn zip-set []
  (zipmap #{} #{}))

(defn rev []
  (reverse [1 2 3 4]))

(defn unck-inc []
  (unchecked-inc Double/MAX_VALUE)
  (unchecked-inc Long/MAX_VALUE)
  (unchecked-inc Integer/MAX_VALUE))

(defn unck-add []
  (unchecked-add Long/MAX_VALUE Long/MAX_VALUE))

(defn unck-mul []
  (unchecked-multiply Double/MAX_VALUE Double/MAX_VALUE))

(foo)
(upd)
(asoi)
(gin)
(sunion)
(select)
(sel-keys)
(zip-set)
(rev)
(unck-inc)
(unck-mul)
(unck-add)
    :ok
  ))))

(defmacro throws? [e & body]
  `(try (do ~@body
            false)
        (catch ~e ex#
          true)))

(defmacro throws-dynalint-error? [& body]
  `(boolean
     (try (do ~@body false)
          (catch clojure.lang.ExceptionInfo e#
            (::dyn/dynalint (ex-data e#)))
          (catch Throwable _#))))

(defn err [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defmacro debug-throws-dynalint-error? [& body]
  `(boolean
    (try (err "debug-throws begins")
         (do (err "just before body") ~@body (err "no exception") false)
         (catch clojure.lang.ExceptionInfo e#
           (err "exceptioninfo e=" e#)
           (err "   (ex-data e)=" (ex-data e#))
           (::dyn/dynalint (ex-data e#)))
         (catch Throwable e#
           (err "other exception class=" (class e#))
           (clojure.repl/pst e# 200)
           nil))))

;; This macro is not working as expected.  The warning-history does not change
;; from before (do ~@body) until after as expected.  Could this be the
;; case because this file is compiled before (lint) above is executed,
;; and this is a macro?  If so, then why does throws-dynalint-error?
;; work, or seem to work?

;;(defmacro issues-dynalint-warning? [& body]
;;  `(boolean
;;    (let [warnings-before# @warning-history]
;;      (err "warnings-before=" (count warnings-before#))
;;      (try
;;        (let [result# (do ~@body)
;;              warnings-after# @warning-history]
;;          (err "warnings-after=" (count warnings-after#))
;;          (> (count warnings-after#) (count warnings-before#)))
;;        (catch Throwable _#
;;          false)))))

(defmacro issues-dynalint-warning? [& body]
  true)

(deftest namespace-test
  (is (= nil (namespace 'foo)))
  (is (= "foo" (namespace :foo/bar)))
  (is (throws-dynalint-error?
        (namespace "foo"))))

(defmulti multimeth1 :foo)

(defmethod multimeth1 "x" [_]
  "multimeth1 for x")

(deftest multimethods-test
  ;; Do all of these in a single deftest, because some of the method
  ;; calls have side effects, making the order important.

  ;; methods
  (is (== 1 (count (methods multimeth1))))
  (is (throws-dynalint-error?
       (methods 'multimeth1)))

  ;; get-method
  (is (= nil (get-method multimeth1 {:foo "x"})))
  (is (throws-dynalint-error?
       (get-method 'multimeth1 {:foo "x"})))

  ;; prefer-method
  (is (= multimeth1 (prefer-method multimeth1 "x" "y")))
  (is (throws-dynalint-error?
       (prefer-method "x" "y" multimeth1)))

  ;; prefers
  (is (= {"x" #{"y"}} (prefers multimeth1)))
  (is (throws-dynalint-error?
       (prefers "x")))

  ;; remove-method
  (is (= multimeth1 (remove-method multimeth1 "x")))
  (is (throws-dynalint-error?
       (remove-method "x")))

  ;; remove-all-methods
  (is (= multimeth1 (remove-all-methods multimeth1)))
  (is (throws-dynalint-error?
       (remove-all-methods "x")))
  )

(deftest meta-test
  (is (= nil (meta [])))
  (is (throws-dynalint-error?
        (meta [] [])))
  (is (throws-dynalint-error?
        (meta))))

(deftest with-meta-args-test
  (is (throws-dynalint-error?
        (with-meta nil {})))
  (is (throws-dynalint-error?
        (with-meta {} []))))

(deftest cons-args-test
  (is (throws-dynalint-error?
        (cons 1 'a))))

(deftest first-args-test
  (is (throws-dynalint-error?
        (first 'a)))
  (is (throws-dynalint-error?
        (second 'a)))
  (is (throws-dynalint-error?
        (ffirst ['a])))
  (is (throws-dynalint-error?
        (nfirst ['a])))
  (is (throws-dynalint-error?
        (fnext 'a)))
  (is (throws-dynalint-error?
        (nnext 'a))))

(deftest pred-test
  (is (throws-dynalint-error?
        (seq?)))
  (is (throws-dynalint-error?
        (char?)))
  (is (throws-dynalint-error?
        (string?)))
  (is (throws-dynalint-error?
        (map?)))
  (is (throws-dynalint-error?
        (vector?)))
  (is (throws-dynalint-error?
        (vector?)))
  ;; We have to have higher-order use of nil? to get dynalint-wrapped
  ;; function, because nil? is inline.  Unfortunately even the test
  ;; below causes throws-dynalint-error? to throw an ArityException
  ;; 'Wrong number of args (2) pssed to: lint/nil?  I am not sure why
  ;; this happens.  It gives a dynalint error if done interactively in
  ;; a REPL.
  #_(is (debug-throws-dynalint-error?
       (doall (map nil? [1] [2])))))

(deftest instance?-args-test
  (is (throws-dynalint-error?
        (instance? 'a 'a))))

(deftest seq-args-test
  (is (throws-dynalint-error?
        (seq 'a)))
  (is (throws-dynalint-error?
        (seq)))
  (is (throws-dynalint-error?
        (seq 'a 'a))))

(deftest unchecked-inc-args-test
  #_(is (throws-dynalint-error?
        (unchecked-inc Long/MAX_VALUE)))
  #_(is (throws-dynalint-error?
        (unchecked-inc Long/MAX_VALUE 1)))
  #_(is (throws-dynalint-error?
        (apply unchecked-inc [Long/MAX_VALUE 1]))))


(deftest dissoc-args-test
  (is (throws-dynalint-error?
        (dissoc 'a 'a)))
  ;corner cases
  (is (nil? (dissoc nil)))
  (is (symbol? (dissoc 'a)))
  (is (nil? (dissoc nil 1 2 3 4))))

(deftest get-in-args-test
  (is (throws-dynalint-error?
        (get-in {} 1))))

(deftest symbol-args-test
  ;; First argument nil to 2-arg symbol does not cause an exception
  ;; without dynalint, and should not cause one with dynalint, either.
  (is (= 'foo (symbol nil "foo")))
  (is (throws-dynalint-error?
        (symbol :a)))
  (is (throws-dynalint-error?
        (symbol 'a 'a))))

(deftest lazy-seq-test
  #_(is (lazy-seq (lazy-seq 1))))

(deftest cast-args-test
  (is (thrown? ClassCastException 
        (cast Long 'a)))
  (is (throws-dynalint-error?
        (cast 'a 'a)))
  (is (throws-dynalint-error?
        (cast))))

(deftest find-args-test
  (is (throws-dynalint-error?
        (find [] 1))))

(deftest disj-args-test
  (is (throws-dynalint-error?
        (disj 'a 1)))
  ; corner cases, just give warnings
  (is (nil? (disj false 1)))
  #_(is (nil? (disj false))))

(deftest find-var-args-test
  (is (throws-dynalint-error?
        (find-var 'a)))
  (is (throws-dynalint-error?
        (find-var nil)))
  (is (throws-dynalint-error?
        (find-var 'a/a))))

(deftest agent-args-test
  (is (throws-dynalint-error?
        (agent)))
  (is (throws-dynalint-error?
        (agent 1 :meta)))
  (is (throws-dynalint-error?
        (agent 1 :meta [])))
  ; false doesn't throw
  (is (agent 1 :meta false))
  ; false doesn't throw
  (is (agent 1 :validator false))
  (is (agent 1 :validator nil))
  (is (throws-dynalint-error?
        (agent 1 :validator 1)))
  (is (agent 1 :error-handler nil))
  (is (agent 1 :error-handler false))
  (is (throws-dynalint-error?
        (agent 1 :error-handler 1)))
  (is (throws-dynalint-error?
        (agent 1 :error-mode 1)))
  (is (agent 1 :error-mode :contfail))
  (is (agent 1 :error-mode nil))
  (is (agent 1 :error-mode false)))

(deftest conj-args-test
  (is (= {}
         (conj {} nil)))
  (is (= {1 2}
         (conj {} {1 2})))
  (is (= {1 2, 3 4}
         (conj {} (seq {1 2, 3 4}))))
  (is (= {1 2, 3 4, 5 6, 7 8}
         (conj {1 3} {1 2, 3 4} (seq {5 6, 7 8}))))
  (is (= {:a 1}
         (conj {} [:a 1])))
  (is (= {:a 1, :b 2}
         (conj {} [:a 1] {:b 2})))
  (is (= {:a 1, :b 2}
         (conj {} [:a 1] nil {:b 2})))
  
  ;; First arg is neither collection nor nil
  (is (throws-dynalint-error?
       (conj 5 1)))
  (is (throws-dynalint-error?
       (conj 5 1 2)))

  ;; First arg is a map, and one or more later args is none of:
  ;; nil, a map entry, a 2-arg vector, a seqable of map entries,
  (is (throws-dynalint-error?
       (conj {} 1)))
  (is (throws-dynalint-error?
       (conj {} [1 2 3])))
  (is (throws-dynalint-error?
       (conj {} :a 1)))
  (is (throws-dynalint-error?
       (conj {} (cons [1 2] (seq {5 6 7 8})))))
  ;; This should be an error
  (is (throws-dynalint-error?
       (conj {} [9 10] (cons [1 2] (seq {5 6 7 8})))))
  )

(deftest next-rest-args-test
  (is (throws-dynalint-error?
        (next)))
  (is (throws-dynalint-error?
        (next 1)))
  (is (throws-dynalint-error?
        (rest)))
  (is (throws-dynalint-error?
        (rest 1))))

(deftest zipmap-test
  (is (= {1 2, 3 4} (zipmap [1 3] [2 4])))
  (is (issues-dynalint-warning?
       (zipmap #{} [1 2])))
  (is (issues-dynalint-warning?
       (zipmap [1 2] #{})))
  (is (issues-dynalint-warning?
       (zipmap #{} #{})))
  (is (throws-dynalint-error?
        (zipmap 1 #{})))
  (is (throws-dynalint-error?
        (zipmap #{} 1))))

(deftest select-keys-test
  (is (select-keys [] []))
  (is (select-keys 'a []))
  (is (throws-dynalint-error?
        (select-keys {} 'a)))
  (is (throws-dynalint-error?
        (select-keys {} 'a)))
  (is (throws-dynalint-error?
        (select-keys 'a [1])))
  (is (select-keys nil [1])))

(deftest hash-map-test
  (is (throws-dynalint-error?
        (hash-map 1)))
  (is (throws-dynalint-error?
        (sorted-map-by < 2 3 4)))
  (is (throws-dynalint-error?
        (sorted-map-by 1 1 2 3 4)))
  ; can't catch this
;  (is (throws-dynalint-error?
;        (sorted-map-by #() 1 2 3 4)))
  (is (throws-dynalint-error?
        (sorted-set-by 1 1)))
  )

;(deftest reduce-test
;  (is (reduce nil [1]))
;  (is (throws-dynalint-error?
;        (reduce nil [])))
;  (is (throws-dynalint-error?
;        (reduce nil 1)))
;  (is (throws-dynalint-error?
;        (reduce nil 1 [1]))))

(deftest let-destructure-test
  #_(is (throws-dynalint-error?
        (eval '(let [{} 1])))))

(deftest deref-test
  (is (throws-dynalint-error?
        @nil))
  (is (throws-dynalint-error?
        (deref nil)))
  (is (deref (atom true)))
  (is (deref (reify
               java.util.concurrent.Future
               (get [_] true)))))

(deftest zipmap-test
  (is
    (zipmap (keys {1 2})
            (vals {1 2}))))

(deftest ex-info-test
  ;; Test that calls with correct type of args return normally.
  (is (= true (instance? Exception
                         (ex-info "msg" {:a 1}))))
  (is (= true (instance? Exception
                         (ex-info "msg" {:a 1} (Throwable.)))))
  (is (= true (instance? Exception
                         (ex-info "msg" {:a 1} nil))))
  ;; First arg not a string
  (is (throws-dynalint-error?
       (ex-info 1 {:a 1})))
  (is (throws-dynalint-error?
       (ex-info 1 {:a 1} (Throwable.))))
  ;; Second arg not a map
  (is (throws-dynalint-error?
       (ex-info "nil data instead of map" nil (Throwable.))))
  ;; Third arg neither a Throwable nor nil
  (is (throws-dynalint-error?
       (ex-info "non-Throwable 3rd arg" {:a 1} 5))))

(deftest partition-test
  ;; Test that calls with correct type of args return normally.
  (is (= '((1 2) (3 4))
         (partition 2 [1 2 3 4 5])))
  (is (= '((1) (2) (3) (4) (5))
         (partition 1 [1 2 3 4 5])))
  (is (= '((1 2) (2 3) (3 4) (4 5))
         (partition 2 1 [1 2 3 4 5])))
  (is (= '((1 2) (2 3) (3 4) (4 5) (5 6))
         (partition 2 1 [6] [1 2 3 4 5])))

  ;; n=0 cause infinite loop in original 2-arity version, error with
  ;; dynalint
  (is (throws-dynalint-error?
       (partition 0 [1 2 3 4 5])))

  ;; Not an infinite loop with 3-arity or 4-arity version, so only a
  ;; warning for dynalint.
  (is (issues-dynalint-warning?
       (partition 0 1 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition 0 1 [6] [1 2 3 4 5])))
  
  ;; Non-positive or non-integer n values give dynalint warning
  (is (issues-dynalint-warning?
       (partition -1 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition 1.5 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition -1 1 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition 1.5 1 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition -1 1 [6] [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition 1.5 1 [6] [1 2 3 4 5])))

  ;; step <= 0 causes infinite loop in original, error with dynalint
  (is (throws-dynalint-error?
       (partition 2 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 -1.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 0 [6] [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 -1.5 [6] [1 2 3 4 5])))

  ;; Positive non-integer step values give dynalint warning
  (is (issues-dynalint-warning?
       (partition 2 1.5 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition 2 1.5 [6] [1 2 3 4 5])))

  ;; pad argument not a seqable gives dynalint error
  (is (throws-dynalint-error?
       (partition 2 0 6 [1 2 3 4 5])))

  ;; coll argument not a seqable gives dynalint error
  (is (throws-dynalint-error?
       (partition 2 1)))
  (is (throws-dynalint-error?
       (partition 2 2 1)))
  (is (throws-dynalint-error?
       (partition 2 0 [6] 1))))

(deftest partition-all-test
  ;; Test that calls with correct type of args return normally.
  (is (= '((1 2) (3 4) (5))
         (partition-all 2 [1 2 3 4 5])))
  (is (= '((1) (2) (3) (4) (5))
         (partition-all 1 [1 2 3 4 5])))
  (is (= '((1 2) (2 3) (3 4) (4 5) (5))
         (partition-all 2 1 [1 2 3 4 5])))

  ;; Values n <= 0 with arity 2 cause infinite loop with original,
  ;; error with dynalint.
  (is (throws-dynalint-error?
       (partition-all 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all -1.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all "a" [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all "a" 1 [1 2 3 4 5])))

  ;; Positive non-integer n values give dynalint warning
  (is (issues-dynalint-warning?
       (partition-all 1.5 [1 2 3 4 5])))
  (is (issues-dynalint-warning?
       (partition-all 1.5 1 [1 2 3 4 5])))

  ;; Non-positive step values cause infinite loop with original,
  ;; error with dynalint.
  (is (throws-dynalint-error?
       (partition-all 2 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 2 -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 2 -0.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 2 "a" [1 2 3 4 5])))

  ;; Non-positive or non-integer step values give dynalint warning
  (is (issues-dynalint-warning?
       (partition-all 2 1.5 [1 2 3 4 5])))

  ;; coll argument not a seqable gives dynalint error
  (is (throws-dynalint-error?
       (partition-all 2 1)))
  (is (throws-dynalint-error?
       (partition-all 2 2 1))))

(deftest vector-of-test
  ;; Test that calls with correct type of args return normally.
  (is (= [1 2 3]
         (vector-of :byte 1 2 3)))
  (is (= [1 2 3]
         (vector-of :short 1 2 3)))
  (is (= [1 2 3]
         (vector-of :int 1 2 3)))
  (is (= [1 2 3]
         (vector-of :long 1 2 3)))
  (is (= [(float 1.0) (float 2.0) (float 3.0)]
         (vector-of :float 1 2 3)))
  (is (= [1.0 2.0 3.0]
         (vector-of :double 1 2 3)))
  (is (= [false true]
         (vector-of :boolean false true)))
  (is (= [\a \b \c]
         (vector-of :char \a \b \c)))

  ;; unrecognized first argument gives dynalint error
  (is (throws-dynalint-error?
       (vector-of :integer 1 2 3))))

(deftest io-reader-test
  ;; Test that calls with correct type of args return normally.
  (is (= true (instance? java.io.BufferedReader
                         (io/reader "README.md"))))
  ;; nil argument gives dynalint error
  (is (throws-dynalint-error?
       (io/reader nil))))

(deftest keyword-test
  ;; Test that calls with correct type of args return normally.
  (is (= :foo (keyword :foo)))
  (is (= :foo (keyword 'foo)))
  (is (= :foo (keyword "foo")))
  (is (= :foo/bar (keyword "foo" "bar")))
  ;; First argument nil to 2-arg keyword does not cause an exception
  ;; without dynalint, and should not cause one with dynalint, either.
  (is (= :foo (keyword nil "foo")))

  ;; Bad type for one arg arity
  (is (issues-dynalint-warning?
       (keyword 5)))
  (is (issues-dynalint-warning?
       (keyword [])))
  (is (issues-dynalint-warning?
       (keyword nil)))

  ;; Bad type for two arg arity
  (is (throws-dynalint-error?
       (keyword 5 nil)))
  (is (throws-dynalint-error?
       (keyword :foo :bar)))
  )
