(ns bandits.exercises
  (:require
   [bandits.agents :as agents]
   [bandits.testbed :as testbed]
   [oz.core :as oz] :reload-all))

(defn run-exp-v2
  [agents]
  (let [results (map (partial testbed/do-run 10 2000 1000) agents)
        labels (map agents/get-plot-label agents)
        rewards-plot (testbed/run-to-line-plot
                       results
                       "reward"
                       labels
                       "10-armed bandit testbed rewards")
        optimal-choices-plot (testbed/run-to-line-plot
                               results
                               "percent-optimal"
                               labels
                               "10-armed bandit testbed optimal choices")]
      (oz/view! {:vconcat [rewards-plot optimal-choices-plot]})))

(defn exp-2-2
  "Running experiment 2.2"
  []
  (let [agents (map #(agents/epsilon-greedy-agent % 10) [0 0.01 0.1])]
    (run-exp-v2 agents)))

(comment
  "Portal"
  (do
    (require '[portal.api :as p])
    (p/open)
    (add-tap #'p/submit))

  (oz/start-server!)
  (exp-2-2))

