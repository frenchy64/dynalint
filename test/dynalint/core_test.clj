(ns dynalint.core-test
  (:use clojure.test
        dynalint.core)
  (:require [clojure.set :as set]))

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

(foo)
(upd)
(asoi)
(gin)
(sunion)
(select)
(sel-keys)
