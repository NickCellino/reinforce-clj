(ns bandits.experiments
  (:require [log :refer :all])
  (:require [bandits.testbed :as testbed])
  (:require [bandits.io :as io]))

(defn run1
  [{output :output :or {output "out.edn"}}]
  (log "Running bandit experiment with 2000 runs, 1000 steps/run, with a 10-armed bandit.")
  (let [results (testbed/run-experiment 2000 1000 10)
        results-summary (testbed/summarize-results results)]
    (print results-summary)))

