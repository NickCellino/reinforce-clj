(ns stats)

(defn mean
  [& args]
  (/ (apply + args) (count args)))

