(ns reinforce-lib.map)

(defn map-kv
  [f _map]
  (apply hash-map (apply concat (map f _map))))

(defn cond-assoc
  [m k v guard]
  (if guard (assoc m k v) m))

