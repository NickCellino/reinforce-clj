(ns bandits.test
  (:require [clojure.edn :as edn])
  (:require [bandits.testbed :as testbed])
  (:require [bandits.agents :as agents])
  (:require [bandits.bandits :as bandits]))

(defn save-to-file
  [results filename]
  (spit filename (prn-str r)))

(defn read-from-file
  [filename]
  (let [edn-readers {'bandits.agents.EpsilonGreedyAgent bandits.agents/map->EpsilonGreedyAgent}]
    (edn/read-string {:readers edn-readers} (slurp filename))))

(comment
  (do
    (save-to-file
      (testbed/run-experiment 200 100 10)
      "results-200-100.edn")

    (def foo (read-from-file "results-200-100.edn"))))

