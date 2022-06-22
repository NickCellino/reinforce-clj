(ns bandits
  (:require [clojure.core :refer :all])
  (:require [clojure.repl :refer :all])
  (:require [kixi.stats.distribution :refer [normal sample]]))

(defn create-bandit
  [arms]
  (vec (sample arms (normal {:location 0 :scale 1}))))

(defn pull-arm
  [bandit arm]
  (let [mean (get bandit arm)]
    (nth (sample 1 (normal {:location mean :scale 1})) 0)))

(defn get-arm-totals
  [arms]
  (into [] (take arms (repeat [0 0]))))

(def rs (into [] (take 10 (repeatedly #(rand-int 100)))))
(def rsv (take 10 (repeatedly #(rand-int 100))))

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

(defn run-bandit-testbed
  [arms epsilon-values runs steps-per-run]
  (let [bandit (create-bandit arms)]
    (reduce
      (fn [results epsilon] (assoc results epsilon (take runs (repeatedly #(perform-run bandit steps-per-run epsilon)))))
      {}
      epsilon-values)))

(conj (conj [] 4) 0)

(assoc (assoc {} 5 18) 9 1)
;
; Testing
;
(def bandito (create-bandit 10))
(perform-run bandito 100 0)

(run-bandit-testbed 10 [0 0.01 0.1] 2000 1000)
(def test-vals (run-bandit-testbed 10 [0 0.01 0.1] 10 100))

(defn avg
  [& args]
  (/ (apply + args) (count args)))

; get the average score at each step
(map
  (fn [[epsilon run-scores]] [epsilon (apply map avg run-scores)])
  test-vals)

