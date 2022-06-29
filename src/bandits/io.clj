(ns bandits.io
  (:require [clojure.edn :as edn])
  (:require [bandits.testbed :as testbed])
  (:require [bandits.agents :as agents])
  (:require [bandits.bandits :as bandits]))

(defn save-to-file
  [results filename]
  (spit filename (prn-str results)))

(defn read-from-file
  [filename]
  (let [edn-readers {'bandits.agents.EpsilonGreedyAgent bandits.agents/map->EpsilonGreedyAgent}]
    (edn/read-string {:readers edn-readers} (slurp filename))))

