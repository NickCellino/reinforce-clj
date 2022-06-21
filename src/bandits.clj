(ns bandits
  (:require [clojure.core :refer :all])
  (:require [clojure.repl :refer :all])
  (:require [kixi.stats.distribution :refer [normal sample]]))

(defn randn
  (sample 1 (normal {:location 0 :scale 1})))

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

(doc update)
(update-in [1] (get-arm-totals 5) (fn [[x y]] [(+ x 1) (+ y 2)]) )
(update [1 2 3] 0 + 1)

(def rs (into [] (take 10 (repeatedly #(rand-int 100)))))
(def rsv (take 10 (repeatedly #(rand-int 100))))

(defn argmax
  [values]
  (let [indexed (map-indexed vector values)
        ret (apply max-key second indexed)]
    (first (apply max-key second indexed))))

(defn get-arm-avgs
  [totals]
  (map (fn [[pulls total]] (if (= pulls 0) (rand) (/ total pulls))) totals))

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
  ([bandit steps epsilon arm-totals total-score]
    (if (= steps 0)
      [(get-arm-avgs arm-totals) total-score]
      (let [chosen-arm (choose-arm epsilon arm-totals)
            reward (pull-arm bandit chosen-arm)
            new-totals (update arm-totals chosen-arm
                               (fn [[pulls total-reward]] [(+ pulls 1) (+ total-reward reward)]))]
        (recur bandit (- steps 1) epsilon new-totals (+ total-score reward)))))
  ([bandit steps epsilon] (perform-run bandit steps epsilon (get-arm-totals (count bandit)) 0)))

;
; Testing
;
(def bandito (create-bandit 10))
(nth (pull-arm bandito 3) 0)
(perform-run bandito 100 0)

