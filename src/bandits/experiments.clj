(ns bandits.experiments
  (:require [oz.core :as oz])
  (:require [bandits.testbed :as testbed])
  (:require [bandits.io :as io]))

(defn run1
  [{out :out :or {out "out.edn"}}]
  (println "Running bandit experiment with 2000 runs, 1000 steps/run, with a 10-armed bandit.")
  (let [results (testbed/run-experiment 2000 1000 10)
        results-summary (testbed/summarize-results results)]
    (io/save-to-file results-summary out)))

(defn transform-results
  [results]
  (let [color-mapping (fn
                        [row]
                        (let [step (:step row)]
                          (cond
                            (> step 750) 3
                            (> step 500) 2
                            (> step 250) 1
                            :else 0)))
        row-mapping (fn [row] (assoc row :color (color-mapping row)))]
  (map row-mapping results)))

(defn vis1
  [{in :in :or {in "out.edn"}}]
  (let [data (io/read-from-file in)
        tdata (transform-results data)
        plot {:data {:values tdata}
              :encoding {:x {:field "step" :type "quantitative"}
                         :y {:field "value" :type "quantitative"}
                         :color {:field "color" :type "nominal"}}
              :mark "line"}]
    (oz/view! plot)))

