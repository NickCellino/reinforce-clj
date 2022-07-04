(ns reinforce-lib.seq)

(defn argmax
  "Returns the index of the largest element in values."
  [values]
  (let [indexed (map-indexed vector values)
        ret (apply max-key second indexed)]
    (first ret)))

