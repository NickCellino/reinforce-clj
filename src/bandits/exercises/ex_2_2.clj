(ns bandits.exercises.ex-2-2 
  (:require
   [bandits.io :as io]
   [bandits.testbed :as testbed]
   [oz.core :as oz]
   [bandits.agents :as agents])
  (:import [java.text SimpleDateFormat]))

(defn gen-output-filename
  ([suffix]
   (let [curr-date (java.util.Date.)
         date-fmt (SimpleDateFormat. "yyyy_MM_dd_HH_mm_ss")]
     (str (.format date-fmt curr-date) suffix)))
  ([] (gen-output-filename ".edn")))

(defn get-bandits-plot
  [data]
  {:data {:values data}
   :encoding {:x {:field "step" :type "quantitative"}
              :y {:field "value" :type "quantitative"}
              :color {:field "epsilon" :type "nominal"}}
   :mark "line"})

(defn add-prop
  [data prop value]
  (map #(assoc % prop value) data))

(defn run
  [{arms :arms 
    trials :trials
    pulls :pulls :or
    {arms 10 trials 2000 pulls 1000}}]
  (println
    "Running experiment 2.2 with parameters:"
    "\n\tArms:" arms
    "\n\tTrials:" trials
    "\n\tPulls:" pulls)
  (let [epsilon-values [0 0.01 0.1]
        agents (map #(agents/epsilon-greedy-agent % arms) epsilon-values)
        results-by-agent (map #(testbed/run-experiment trials pulls arms %) agents)
        results-by-agent-summaries (map #(testbed/summarize-results %) results-by-agent)
        results-by-agent-summaries (map vector epsilon-values results-by-agent-summaries)
        results-by-agent-summaries (map (fn [[ep r]] (add-prop r :epsilon (str ep))) results-by-agent-summaries)
        flattened-results (apply concat results-by-agent-summaries)
        filename (gen-output-filename)
        plot (get-bandits-plot flattened-results)]
    (println filename)
    (oz/view! plot)
    (io/save-to-file flattened-results filename)))
                
