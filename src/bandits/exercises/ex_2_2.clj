(ns bandits.exercises.ex-2-2
  (:require
   [bandits.agents :as agents]
   [bandits.testbed :as testbed]
   [bandits.visualize :as vis]
   [oz.core :as oz]))

(defn run-exp
  [agents]
  (let [results (testbed/run-testbed agents 10 2000 1000)
        labels (map agents/get-plot-label agents)
        labeled-results (vis/label (:rewards results) labels)
        labeled-optimal-choice-results (vis/label (:optimal-choices results) labels)
        optimal-choice-plot (vis/create-line-plot labeled-optimal-choice-results ["step" "percentage"] "Optimal choices")
        rewards-plot (vis/create-line-plot labeled-results ["step" "value"] "Rewards")
        overall-plot {:vconcat [optimal-choice-plot rewards-plot]}]
      (oz/view! overall-plot)))

(comment
  "Running experiment 2.2"
  (let [agents (map #(agents/epsilon-greedy-agent % 10) [0 0.01 0.1])]
    (run-exp agents)))
  
