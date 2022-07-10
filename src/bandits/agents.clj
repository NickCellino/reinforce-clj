(ns bandits.agents
  (:require [reinforce-lib.seq :as rls]))

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
      (rls/argmax (:value-estimates agent))))
  (update-agent
    [agent arm reward]
    (println "updating agent" agent "arm" arm "reward" reward)
    (let [n (+ 1 (nth pulls-per-arm arm))
          alpha (/ 1 n)
          old-value-estimate (get (:value-estimates agent) arm)
          error (- reward old-value-estimate)
          new-value-estimate (+ old-value-estimate (* alpha error))
          agent-with-updated-n (update-in agent [:pulls-per-arm arm] (partial + 1))]
      (assoc-in agent-with-updated-n [:value-estimates arm] new-value-estimate))))

(defn epsilon-greedy-agent
  [epsilon num-arms]
  (let [zeros (vec (repeat num-arms 0))]
    (if
      (or (> epsilon 1) (< epsilon 0))
      nil
      (->EpsilonGreedyAgent epsilon zeros zeros))))

(defmulti get-plot-label class)
(defmethod get-plot-label EpsilonGreedyAgent [a] (str "Epsilon greedy (ε=" (:epsilon a) ")"))

