(ns bandits.testbed
  (:require
   [bandits.agents :as agents]
   [bandits.bandits :as bandits]
   [reinforce-lib.seq :refer [argmax]]
   [reinforce-lib.stats :as rls]))

(defn summarize-results
  "Summarize result to a vector of { :value 0 :step 1 }"
  [results]
  (let [indexed-avgs (->> results
                          (apply map rls/mean)
                          (map-indexed vector))]
    (map #(assoc {}
           :value (get % 1)
           :step (get % 0)) indexed-avgs)))

(defn perform-trial
  "Performs a single trial with the specified pulls.
  
  Returns a sequence of {:reward 0.3 :optimal false|true}"
  [agent bandit pulls]
  (let [optimal-choice (argmax bandit)]
    (loop [pull 0
           results []
           agent agent]
      (if (> pull pulls)
        results
        (let [chosen-arm (agents/choose-arm agent)
              reward (bandits/pull-arm bandit chosen-arm)
              new-agent (agents/update-agent agent chosen-arm reward)
              was-optimal (= chosen-arm optimal-choice)]
          (recur (inc pull) (conj results {:reward reward :optimal was-optimal}) new-agent))))))

(defn perform-trials
  [agent bandit trials pulls]
  (pmap (fn [_] (perform-trial agent bandit pulls)) (range trials)))

(defn extract-rewards
  [results]
  (map #(map :reward %) results))

(defn extract-optimal
  [results]
  (map #(map :optimal %) results))

(defn run-testbed
  [agents arms trials pulls]
  (let [bandit (bandits/n-armed-bandit arms)
        results-vec-by-agent (pmap #(perform-trials % bandit trials pulls) agents)
        rewards-vec-by-agent (map extract-rewards results-vec-by-agent)
        optimal-choice-vec-by-agent (map extract-optimal results-vec-by-agent)
        avgd-results-vec-by-agent (pmap summarize-results rewards-vec-by-agent)] 
    avgd-results-vec-by-agent))

; TODO summarize the optimal choices per agent
  
