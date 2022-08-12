(ns bandits.agents
  (:require
   [reinforce-lib.seq :refer [argmax]]))

(defmulti choose-arm :agent-type)
(defmulti get-plot-label :agent-type)
(defmulti add-observation (fn [agent & _] (:agent-type agent)))

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

(defn update-arm-estimates
  "Update the agents value estimate for the specified arm."
  [agent arm reward]
  (let [n (inc (get (:pulls-per-arm agent) arm))
          alpha (/ 1 n)
          old-value-estimate (get (:value-estimates agent) arm)
          error (- reward old-value-estimate)]
      (-> agent
          (update-in [:pulls-per-arm arm] inc)
          (update-in [:value-estimates arm] (partial + (* alpha error))))))


(defmethod add-observation :epsilon-greedy
  [agent arm reward]
  (update-arm-estimates agent arm reward))

(defmethod get-plot-label :epsilon-greedy [a] (str "Epsilon greedy (ε=" (:epsilon a) ")"))

(defn smarter-epsilon-greedy-agent
  "This is a smarter epsilon greedy agent.
   He knows how long the trial is and he does
   all of his exploring at the beginning."
   [epsilon num-arms num-pulls]
   (let [zeros (vec (repeat num-arms 0))]
     {:agent-type :smarter-epsilon-greedy
      :epsilon epsilon
      :exploratory-pulls (int (* epsilon num-pulls))
      :pulls-per-arm zeros
      :value-estimates zeros}))

(defmethod choose-arm :smarter-epsilon-greedy
  [agent]
  (if
    (> (:exploratory-pulls agent) 0)
    (rand-int (count (:value-estimates agent)))
    (argmax (:value-estimates agent))))

(defmethod add-observation :smarter-epsilon-greedy
  [agent arm reward]
  (-> agent
      (update-arm-estimates arm reward)
      (update-in [:exploratory-pulls] dec)))

(defmethod get-plot-label :smarter-epsilon-greedy [a] (str "Smart epsilon greedy (ε=" (:epsilon a) ")"))

(comment

  (def nine (fn [] 99))
  (def ten nine)
  (def ten #'nine)
  (ten)

  (require '[clojure.repl :refer :all])
  (doc var)

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

    (def smart-agent (smarter-epsilon-greedy-agent 0.3 10 100))
    (choose-arm smart-agent) ; random
    (choose-arm (assoc smart-agent :exploratory-pulls 0)) ; 9

    (take 10 (repeatedly #(choose-arm {:agent-type :epsilon-greedy
                                       :epsilon 0.5
                                       :value-estimates [0.75 0 0 0 0 0 0]}))))) ; (0 1 0 0 4 6 0 0 0 2)
