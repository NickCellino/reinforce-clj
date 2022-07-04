(ns reinforce-lib.vec)

(defn update-by-idx
  [my-vec idx f]
  (let [old (get my-vec idx)]
    (assoc my-vec idx (f old))))

