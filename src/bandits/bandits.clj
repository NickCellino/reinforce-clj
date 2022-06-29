(ns bandits.bandits
  (:require [clojure.core :refer :all])
  (:require [clojure.repl :refer :all])
  (:require [kixi.stats.distribution :refer [normal sample]]))

(defn create-bandit
  [arms]
  (vec (sample arms (normal {:location 0 :scale 1}))))
(def n-armed-bandit create-bandit)

(defn pull-arm
  [bandit arm]
  (let [mean (get bandit arm)]
    (nth (sample 1 (normal {:location mean :scale 1})) 0)))

(defn get-arm-totals
  [arms]
  (into [] (take arms (repeat [0 0]))))

(defn argmax
  [values]
  (let [indexed (map-indexed vector values)
        ret (apply max-key second indexed)]
    (first (apply max-key second indexed))))

(defn get-arm-avgs
  [totals]
  (map (fn [[pulls total]] (if (= pulls 0) 0 (/ total pulls))) totals))

(defn get-greedy-choice
  [arm-totals]
  (let [arm-avgs (get-arm-avgs arm-totals)]
    (argmax arm-avgs)))

(defn choose-arm
  [epsilon totals]
  (let [random-choice (rand)
        greedy (< random-choice epsilon)]
    (get-greedy-choice totals)))

(defn perform-run
  ([bandit steps epsilon arm-totals total-score scores]
    (if (= steps 0)
      scores
      (let [chosen-arm (choose-arm epsilon arm-totals)
            reward (pull-arm bandit chosen-arm)
            new-totals (update arm-totals chosen-arm
                               (fn [[pulls total-reward]] [(+ pulls 1) (+ total-reward reward)]))]
        (recur bandit (- steps 1) epsilon new-totals (+ total-score reward) (conj scores reward)))))
  ([bandit steps epsilon] (perform-run bandit steps epsilon (get-arm-totals (count bandit)) 0 [])))

