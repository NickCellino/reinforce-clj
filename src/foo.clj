(ns foo
  (:require [clojure.repl :refer :all])
  (:require [oz.core :as oz])
  (:require [bandits.test :as test]))

(oz/start-server!)

(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :item n :quantity (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))

(def line-plot
  {:data {:values (play-data "monkey" "slipper" "broom")}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative"}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

(oz/view! line-plot)

(def stacked-bar
  {:data {:values (play-data "munchkin" "witch" "dog" "lion" "tiger" "bear")}
   :mark "bar"
   :encoding {:x {:field "time"
                  :type "ordinal"}
              :y {:aggregate "sum"
                  :field "quantity"
                  :type "quantitative"}
              :color {:field "item"
                      :type "nominal"}}})

(defn update-by-idx
  [my-vec idx f]
  (let [old (get my-vec idx)]
    (assoc my-vec idx (f old))))

(def new-stacked-bar
  (assoc-in stacked-bar [:data :values]
   (seq (let 
    [values (vec (get-in stacked-bar [:data :values]))
     first (get values 1)]
    (assoc values 1 (assoc first :quantity 100))))))

(oz/view! stacked-bar)
(oz/view! new-stacked-bar)

(def data (test/read-from-file "results-200-100.edn"))
((nth data 0) :step-rewards)

(defn mean
  [& args]
  (/ (apply + args) (count args)))

(map-indexed vector [1 2 3])
(reverse '(1 2 3))

(def avgs
  (map #(assoc {} :value (get % 1) :step (+ (get % 0) 1))
       (map-indexed vector (reverse (apply map mean (map :step-rewards data))))))

(def avg-plot
  {:data {:values avgs}
   :encoding {:x {:field "step" :type "quantitative"}
              :y {:field "value" :type "quantitative"}}
   :mark "line"})

(oz/view! avg-plot)


