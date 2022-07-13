(ns bandits.testbed
  (:require
   [bandits.agents :as agents]
   [bandits.bandits :as bandits]
   [reinforce-lib.seq :refer [argmax]]
   [reinforce-lib.stats :as rls]))

(defn summarize-results
  "Summarize result to a vector of { :value 0 :step 1 }"
  [func results]
  (let [indexed-avgs (->> results
                          (apply map func)
                          (map-indexed vector))]
    (map #(assoc {}
           :value (get % 1)
           :step (get % 0)) indexed-avgs)))

(defn perform-trial
  "Performs a single trial with the specified pulls.
  
  Returns a sequence of {:reward 0.3 :optimal false|true}"
  [agent arms pulls]
  (let [bandit (bandits/n-armed-bandit arms)
        optimal-choice (argmax bandit)]
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
  [agent arms trials pulls]
  (pmap (fn [_] (perform-trial agent arms pulls)) (range trials)))

(defn extract-nested
  [results kw]
  (map #(map kw %) results))

(defn fraction-true
  [& vals]
  (float (/ (count (filter identity vals)) (count vals))))

(defn summarize-optimal-choices
  [optimal-choices]
  (map-indexed (fn [idx p] {:percentage p :step idx}) (apply map fraction-true optimal-choices)))

(defn run-testbed
  [agents arms trials pulls]
  (let [results-vec-by-agent (pmap #(perform-trials % arms trials pulls) agents)
        rewards-vec-by-agent (map #(extract-nested % :reward) results-vec-by-agent)
        optimal-choice-vec-by-agent (map #(extract-nested % :optimal) results-vec-by-agent)
        optimal-percentage-by-agent (map summarize-optimal-choices optimal-choice-vec-by-agent)
        avgd-results-vec-by-agent (pmap (partial summarize-results rls/mean) rewards-vec-by-agent)] 
    {:rewards avgd-results-vec-by-agent :optimal-choices optimal-percentage-by-agent}))
  
