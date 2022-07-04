(ns bandits.testbed
  (:require 
    [bandits.agents :as agents]
    [bandits.bandits :as bandits]
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
  [agent bandit pulls]
  (loop [pull 0
         rewards []
         agent agent]
    (if (> pull pulls)
      rewards
      (let [chosen-arm (agents/choose-arm agent)
            reward (bandits/pull-arm bandit chosen-arm)
            new-agent (agents/update-agent agent chosen-arm reward)]
        (recur (inc pull) (conj rewards reward) new-agent)))))

(defn perform-trials
  [agent bandit trials pulls]
  (pmap (fn [_] (perform-trial agent bandit pulls)) (range trials)))

(defn run-testbed
  [agents arms trials pulls]
  (let [bandit (bandits/n-armed-bandit arms)
        results-vec-by-agent (pmap #(perform-trials % bandit trials pulls) agents)
        avgd-results-vec-by-agent (pmap summarize-results results-vec-by-agent)]
    avgd-results-vec-by-agent))
  
