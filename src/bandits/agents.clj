(ns bandits.agents
  (:require
   [reinforce-lib.seq :refer [argmax]]))

(defmulti choose-arm :agent-type)
(defmulti get-plot-label :agent-type)

(defn add-observation
  "Add an observation to an agent."
  [agent arm reward]
  (let [n (inc (get (:pulls-per-arm agent) arm))
        alpha (/ 1 n)
        old-value-estimate (get (:value-estimates agent) arm)
        error (- reward old-value-estimate)]
    (-> agent
        (assoc-in [:pulls-per-arm arm] n)
        (update-in [:value-estimates arm] (partial + (* alpha error))))))

; Epsilon greedy agents
(defn epsilon-greedy-agent
  [epsilon num-arms]
  (if
    (or (> epsilon 1) (< epsilon 0))
    nil
    (let [zeros (vec (repeat num-arms 0))]
      {:agent-type :epsilon-greedy :epsilon epsilon :pulls-per-arm zeros :value-estimates zeros})))

(defmethod choose-arm :epsilon-greedy
  [agent]
  (if
    (< (rand) (:epsilon agent))
    (rand-int (count (:value-estimates agent)))
    (argmax (:value-estimates agent))))

(defmethod get-plot-label :epsilon-greedy [a] (str "Epsilon greedy (ε=" (:epsilon a) ")"))

(comment
  (do
    (require '[portal.api :as p])
    (p/open)
    (add-tap #'p/submit)

    (def test (epsilon-greedy-agent 0.3 4))
    (-> test
        (add-observation 2 12.2)
        (add-observation 2 4)
        (add-observation 3 6)
        (add-observation 3 7)
        (add-observation 3 6))))
