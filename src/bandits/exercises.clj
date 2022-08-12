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
  [& opts]
  (let [agents (map #(agents/epsilon-greedy-agent % 10) [0 0.01 0.1])]
    (run-exp-v2 agents)))

(defn exp-2-2b
  "Running experiment 2.2b"
  [& opts]
  (let [agents (concat
                 (map #(agents/epsilon-greedy-agent % 10) [0.01 0.1])
                 (map #(agents/smarter-epsilon-greedy-agent % 10 1000) [0.05 0.1 0.15]))]
    (run-exp-v2 agents)))

(defn get-str-rep
  [[agent score]]
  (str "Agent: " (agents/get-plot-label agent) " | Score: " score))

(defn exp-2-2ba
  [& opts]
  (let [agents (map #(agents/smarter-epsilon-greedy-agent % 10 1000) [0.04 0.05 0.06])
        avg-scores (map (fn [a]
                          (let [results (testbed/do-run 10 2000 1000 a)]
                            (apply + (map :reward results))))
                        agents)]
    (->> (map vector agents avg-scores)
         (map get-str-rep)
         (clojure.string/join "\n")
         (println))))

(defn abc [[a b]] (println a b))

(comment
  "Portal"
  (do
    (require '[portal.api :as p])
    (p/open)
    (add-tap #'p/submit))

  (oz/start-server!)
  (exp-2-2)
  (exp-2-2b)
  (exp-2-2ba)

  (def test '([{:agent-type :smarter-epsilon-greedy,
                :epsilon 0.05,
                :exploratory-pulls 50,
                :pulls-per-arm [0 0 0 0 0 0 0 0 0 0],
                :value-estimates [0 0 0 0 0 0 0 0 0 0]}
               1401.9374270162177]
              [{:agent-type :smarter-epsilon-greedy,
                :epsilon 0.1,
                :exploratory-pulls 100,
                :pulls-per-arm [0 0 0 0 0 0 0 0 0 0],
                :value-estimates [0 0 0 0 0 0 0 0 0 0]}
               1386.0786462068872]
              [{:agent-type :smarter-epsilon-greedy,
                :epsilon 0.15,
                :exploratory-pulls 150,
                :pulls-per-arm [0 0 0 0 0 0 0 0 0 0],
                :value-estimates [0 0 0 0 0 0 0 0 0 0]}
               1293.5712676311223]))

  (defn get-str-rep
    [[agent score]]
    (str "Agent: " (agents/get-plot-label agent) " | Score: " score))

  (get-str-rep (first test))
  (println (clojure.string/join "\n" (map get-str-rep test))))


