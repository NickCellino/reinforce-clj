(ns bandits.visualize 
  (:require
   [oz.core :as oz]
   [reinforce-lib.map :as rl]))

(defn label
  [coll labels]
  (let [coll-w-labels (map vector coll labels)]
    (reduce
      (fn [labeled [val label]]
        (assoc labeled label val))
      {}
      coll-w-labels)))

(defn create-line-plot
  [data [x-label y-label] title]
  (let [label-values (fn [[label values]] (map #(assoc % :label label) values))
        flattened-labeled-values (mapcat label-values data)]
    (-> {:data {:values flattened-labeled-values}
         :encoding {:x {:field x-label :type "quantitative"}
                    :y {:field y-label :type "quantitative"}
                    :color {:field "label" :type "nominal"}}
         :mark "line"}
        (rl/cond-assoc :title title (not (nil? title))))))

(comment
  (let [test-data {"epsilon = 0.1" [1 2 3 4 5]
                   "epsilon = 0" [0 0 0 0 1]}
        to-values (fn [n] {:value n})
        test-data (rl/map-kv (fn [[label data]] [label (map to-values data)]) test-data)]
    (create-line-plot test-data ["step" "value"] nil)))

(defn plot-lines
  "Create a line chart using the specified data.
  
  The data is expected to be a map with keys treated as the labels and values
  treated as the series of datapoints for the chart.
  
  Axes should be a tuple with [x-label y-label]"
  ([data axes]
   (plot-lines data axes {}))
  ([data axes opts]
   (let [title (:title opts)
         plot-spec (create-line-plot data axes title)]
     (oz/view! plot-spec))))

