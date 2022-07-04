(ns bandits.bandits
  (:require [kixi.stats.distribution :refer [normal sample]]))

(defn n-armed-bandit
  [arms]
  (vec (sample arms (normal {:location 0 :scale 1}))))

(defn pull-arm
  [bandit arm]
  (let [mean (get bandit arm)]
    (nth (sample 1 (normal {:location mean :scale 1})) 0)))

