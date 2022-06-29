(ns log
  (:require [clojure.string :as string]))

(defn log
  [& msgs]
  (.println *err* (string/join " " msgs)))

