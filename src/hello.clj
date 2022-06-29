(ns hello
  (:require [clojure.core :refer :all]))

(defn runA
  [opts]
   (let [msg (if (nil? opts) "please provide some opts" (opts :foo))]
     (println "HELLO WORLD" msg)))

(defn runB
  [opts]
  (println "IN B"))

