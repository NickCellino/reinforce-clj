(ns bandits.experiments
  (:require [bandits.testbed :as testbed])
  (:require [bandits.io :as io]))

(defn run1
  [{output :output :or {output "out.edn"}}]
  (println "Running bandit experiment with 2000 runs, 1000 steps/run, with a 10-armed bandit.")
  (let [results (testbed/run-experiment 2000 1000 10)
        results-summary (testbed/summarize-results results)]
    (print "Experiment complete. Writing output to" output)
    (io/save-to-file results-summary output)))

