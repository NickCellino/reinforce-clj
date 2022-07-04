(ns reinforce-lib.stats)

(defn mean
  [& args]
  (/ (apply + args) (count args)))

