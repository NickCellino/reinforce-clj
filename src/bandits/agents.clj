(ns bandits.agents
  (:require [clojure.core :refer :all])
  (:require [clojure.repl :refer :all])
  (:require [bandits.bandits :as bandits])
  (:require [bandits.testbed :as testbed]))

(defn argmax
  "Returns the index of the largest element in values."
  [values]
  (let [indexed (map-indexed vector values)
        ret (apply max-key second indexed)]
    (first ret)))

(defprotocol BanditAgent
  (choose-arm [agent] "Choose which arm to pull")
  (update-agent [agent arm result] "Return an bandit updated with the most recent result"))

(defrecord EpsilonGreedyAgent [epsilon pulls-per-arm value-estimates]
  BanditAgent
  (choose-arm
    [agent]
    (if
      (< (rand) (:epsilon agent))
      (rand-int (count (:value-estimates agent)))
      (argmax (:value-estimates agent))))
  (update-agent
    [agent arm reward]
      (let [n (+ 1 (nth pulls-per-arm arm))
            alpha (/ 1 n)
            old-value-estimate (get (:value-estimates agent) arm)
            error (- reward old-value-estimate)
            new-value-estimate (+ old-value-estimate (* alpha error))
            agent-with-updated-n (update-in agent [:pulls-per-arm arm] (partial + 1))]
        (assoc-in agent-with-updated-n [:value-estimates arm] new-value-estimate))))

; Create an EpsilonGreedyAgent
(defn epsilon-greedy-agent
  [epsilon num-arms]
  (let [zeros (vec (repeat num-arms 0))]
    (if
      (or (> epsilon 1) (< epsilon 0))
      nil
      (->EpsilonGreedyAgent epsilon zeros zeros))))

; running an experiment
(def r (testbed/run-experiment 2000 1000 10))

;
; TODO:
;  Epsilon Greedy agent
;  Softmax agent
;
; let [result (pull agent bandit)
;      updated-agent (update-agent agent result)]
;  (pull updated-agent bandit
;
