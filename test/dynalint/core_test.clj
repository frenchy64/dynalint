(ns dynalint.core-test
  (:use clojure.test)
  (:refer-clojure :exclude [nil?])
  (:require [clojure.set :as set]
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
  (is (conj {} {1 2}))
  (is (conj {} (seq {1 2 3 4})))
  (is (throws-dynalint-error?
        (conj {} 1 2))))

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
  (is (zipmap #{} #{}))
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
  ;; First arg not a string
  (is (throws-dynalint-error?
       (ex-info 1 {:a 1})))
  (is (throws-dynalint-error?
       (ex-info 1 {:a 1} (Throwable.))))
  ;; Second arg not a map
  (is (throws-dynalint-error?
       (ex-info "nil data instead of map" nil (Throwable.))))
  ;; Third arg not a Throwable
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

  ;; Non-positive or non-integer n values give dynalint error
  (is (throws-dynalint-error?
       (partition 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 1.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 0 1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 1.5 1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 0 1 [6] [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 1.5 1 [6] [1 2 3 4 5])))

  ;; Non-positive or non-integer step values give dynalint error
  (is (throws-dynalint-error?
       (partition 2 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 1.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition 2 0 [6] [1 2 3 4 5])))
  (is (throws-dynalint-error?
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

  ;; Non-positive or non-integer n values give dynalint error
  (is (throws-dynalint-error?
       (partition-all 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 1.5 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 0 1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 1.5 1 [1 2 3 4 5])))

  ;; Non-positive or non-integer step values give dynalint error
  (is (throws-dynalint-error?
       (partition-all 2 0 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 2 -1 [1 2 3 4 5])))
  (is (throws-dynalint-error?
       (partition-all 2 1.5 [1 2 3 4 5])))

  ;; coll argument not a seqable gives dynalint error
  (is (throws-dynalint-error?
       (partition-all 2 1)))
  (is (throws-dynalint-error?
       (partition-all 2 2 1))))
