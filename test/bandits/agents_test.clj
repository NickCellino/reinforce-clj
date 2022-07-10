(ns bandits.agents-test 
  (:require
   [bandits.agents :refer [choose-arm epsilon-greedy-agent update-agent]]
   [clojure.test :refer [deftest is testing]]))

(deftest epsilon-greedy-agent-choose-arm-test
  (let [test-agent (epsilon-greedy-agent 0.5 10)]
    (testing "random (exploratory) strategy"
          (with-redefs
            ; rand redefined to something below 0.5 to trigger exploratory action
            [rand (fn [] 0.2)
             rand-int (fn [_] 4)]
            (let [chosen-arm (choose-arm test-agent)]
                (is (= 4 chosen-arm)))))

    (testing "greedy strategy"
      (with-redefs
        [rand (fn [] 0.6)]
        (let [test-agent (assoc-in test-agent [:value-estimates 7] 9999)
              chosen-arm (choose-arm test-agent)]
          (is (= 7 chosen-arm)))))))

(deftest epsilon-greedy-agent-update-agent-test
  (let [test-agent (epsilon-greedy-agent 0.5 2)
        simulate-rewards (fn [rwds]
                           (reduce (fn [agent [arm r]] (update-agent agent arm r)) test-agent rwds))]
    (testing "two pulls on single arm"
      (let [agent (simulate-rewards [[0 10], [0 20]])]
        (is (= [15 0] (:value-estimates agent)))))
    (testing "two pulls on each arm"
      (let [agent (simulate-rewards [[0 5] [1 20] [0 10] [1 21]])]
        (is (= [15/2 41/2] (:value-estimates agent)))))))

(deftest epsilon-greedy-agent-builder-test
  (let [agent (epsilon-greedy-agent 0.2 5)]
    (is (.equals {:epsilon 0.2
                  :pulls-per-arm [0 0 0 0 0]
                  :value-estimates [0 0 0 0 0]}
           agent))))
    
