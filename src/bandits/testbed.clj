(ns bandits.testbed
  (:require [clojure.core :refer :all])
  (:require [bandits.agents :as agents])
  (:require [bandits.bandits :as bandits])
  (:require [stats]))

(defn perform-run
  "Performs a run for the given agent and bandit with the specified number of steps."
  [agent bandit steps]
  (reduce
    (fn [run-data _]
      (let [agent (:agent run-data)
            chosen-arm (agents/choose-arm agent)
            reward (bandits/pull-arm bandit chosen-arm)
            updated-agent (agents/update-agent agent chosen-arm reward)
            step-result {:choices chosen-arm :step-rewards reward}]
        (assoc (merge-with cons step-result run-data) :agent updated-agent)))
    {:agent agent :choices [] :step-rewards []}
    (range steps)))

; for each run, return 
; { :bandit {}, :agent {}, :step-rewards [] :choices [] }
; end result should be a seq of 2000 of those
(defn run-experiment
  [runs steps num-arms]
  (let [agent (agents/epsilon-greedy-agent 0 num-arms)]
    (reduce
      (fn [runs-data _]
        (let [bandit (bandits/n-armed-bandit num-arms)
              run-result (perform-run agent bandit steps)
              run-result-w-bandit (merge run-result {:bandit bandit})]
          (cons run-result-w-bandit runs-data)))
      []
      (range runs))))

(defn summarize-results
  "Summarize result to a vector of { :value 0 :step 1 }"
  [results]
  (let [indexed-avgs (->> (map :step-rewards results)
                          (apply map stats/mean)
                          (reverse)
                          (map-indexed vector))]
    (map #(assoc {}
           :value (get % 1)
           :step (inc (get % 0))) indexed-avgs)))

