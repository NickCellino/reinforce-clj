(ns bandits.exercises.ex-2-2
  (:require
   [bandits.agents :as agents]
   [bandits.testbed :as testbed]
   [bandits.visualize :as vis]))

(defn run-exp
  [agents chart-title]
  (let [results (testbed/run-testbed agents 10 2000 1000)
        labels (map agents/get-plot-label agents)
        labeled-results (vis/label (:rewards results) labels)
        labeled-optimal-choice-results (vis/label (:optimal-choices results) labels)]
    (println labeled-optimal-choice-results)
    (vis/plot-lines labeled-optimal-choice-results ["step" "percentage"] {:title "Optimal choices"})
    (comment (vis/plot-lines labeled-results ["step" "value"] {:title chart-title}))))

(comment
  "Running experiment 2.2"
  (let [agents (map #(agents/epsilon-greedy-agent % 10) [0 0.01 0.05 0.1])]
    (run-exp agents "Epsilon greedy agents")))
  