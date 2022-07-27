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
        (update-in [:pulls-per-arm arm] inc)
        (update-in [:value-estimates arm] (partial + (* alpha error))))))

(comment
  (do
    (def test-agent {:pulls-per-arm [0 0 0]
                     :value-estimates [0 0 0]})
    (-> {:pulls-per-arm [0 0 0]
         :value-estimates [0 0 0]}
        (add-observation 0 3)
        (add-observation 0 4)
        (add-observation 0 6))))
; {:pulls-per-arm [3 0 0], :value-estimates [13/3 0 0]}

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

    (def my-agent (epsilon-greedy-agent 0.3 10))
    my-agent
; {:agent-type :epsilon-greedy,
;  :epsilon 0.3,
;  :pulls-per-arm [0 0 0 0 0 0 0 0 0 0],
;  :value-estimates [0 0 0 0 0 0 0 0 0 0]}
    (-> test
        (add-observation 2 12.2)
        (add-observation 2 4)
        (add-observation 3 6)
        (add-observation 3 7)
        (add-observation 3 6))

    (choose-arm my-agent)

    (take 10 (repeatedly #(choose-arm {:agent-type :epsilon-greedy
                                       :epsilon 0.5
                                       :value-estimates [0.75 0 0 0 0 0 0]}))))) ; (0 1 0 0 4 6 0 0 0 2)
