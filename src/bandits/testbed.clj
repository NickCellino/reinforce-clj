(ns bandits.testbed
  (:require
   [bandits.agents :as agents]
   [bandits.bandits :as bandits :refer [n-armed-bandit]]
   [reinforce-lib.map :as rl]
   [reinforce-lib.seq :refer [argmax]]
   [reinforce-lib.stats :as rls]))

(defn fraction-true
  [& vals]
  (float (/ (count (filter identity vals)) (count vals))))

(defn do-pull
  "Performs a single pull on the specified bandit.
  Returns a vector with two elements:
    1. the agent (updated with this observation)
    2. the result (the reward and the choice the agent made)"
  [agent bandit]
  (let [arm-choice (agents/choose-arm agent)
        reward (bandits/pull-arm bandit arm-choice)]
    [(agents/add-observation agent arm-choice reward)
     {:reward reward :choice arm-choice}]))

(defn do-trial
  "Performs a full trial of the specified number of pulls.
  Returns a vector containing the vector of results
  (1 for each pull).
  Each result is a map with:
    :reward numerical reward value
    :choice the arm that was chosen"
  [agent bandit pulls]
  (let [optimal-choice (argmax bandit)
        do-pull-reducer (fn [[results agent] _]
                          (let [[agent result] (do-pull agent bandit)
                                was-optimal (= (:choice result) optimal-choice)]
                            [(conj results {:reward (:reward result) :was-optimal was-optimal}) agent]))
        [results _] (reduce do-pull-reducer [[] agent] (vec (range pulls)))]
    results))

(defn do-run
  "Performs a full run of the specified number of trials.

  Returns a vector of 'pulls-per-trial' length where each
  element represents the mean of the rewards the agent obtained
  at that step across all the trials.

  TODO: also return the percentage optimal choices"
  [num-arms trials pulls-per-trial agent]
  (let [trial-fn (fn [] (do-trial agent (n-armed-bandit num-arms) pulls-per-trial))
        run-results (apply pcalls (repeat trials trial-fn))
        results-by-step (apply map vector run-results)
        get-step-means (fn [step-results]
                         (->> step-results
                              (map :reward)
                              (apply rls/mean)))
        get-step-optimal-choices (fn [step-results]
                                   (->> step-results
                                        (map :was-optimal)
                                        (apply fraction-true)))]
    (->> results-by-step
         (map (fn [step-results]
                  {:reward (get-step-means step-results) :percent-optimal (get-step-optimal-choices step-results)}))
         (map #(assoc %2 :step %1) (range)))))

(defn vega-line-plot
  [data x-field y-field]
  {:data {:values data}
         :encoding {:x {:field x-field :type "quantitative"}
                    :y {:field y-field :type "quantitative"}
                    :color {:field "label" :type "nominal"}}
         :mark "line"})

(defn run-to-line-plot
  "Creates a vega line plot from run data."
  [data field labels title]
  (let [label-run (fn [values label] (map #(assoc % :label label) values))
        labeled-results (mapcat #(label-run %1 %2) data labels)]
    (-> labeled-results
        (vega-line-plot "step" field)
        (rl/cond-assoc :title title (not (nil? title))))))

(comment
  (def agent' (agents/epsilon-greedy-agent 0.4 10))

  ; 2528 ms
  (time (doall (do-run 10 2000 1000 agent')))

  (map (partial do-run 10 20 10) [agent']))
