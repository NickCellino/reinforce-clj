(ns bandits.bandits
  (:require [kixi.stats.distribution :refer [normal sample]]))

(defn n-armed-bandit
  [arms]
  (vec (sample arms (normal {:location 0 :scale 1}))))

(defn pull-arm
  [bandit arm]
  (let [mean (get bandit arm)]
    (first (sample 1 (normal {:location mean :scale 1})))))

(comment
  (def my-bandit (n-armed-bandit 10))  ; #'bandits.bandits/my-bandit

  (def my-bandit [0.02835533640472183
                  -0.5921152593867257
                  0.8107932784184523
                  -0.13893647669879633
                  -1.560230824161466
                  -1.3897640492033752
                  -1.6847154560130533
                  0.10596403977853011
                  -1.3676288732559299
                  0.6137388332977948])

  (take 5 (repeatedly #(pull-arm my-bandit 2)))
; (-0.12799747344658496
;  1.1085487089359316
;  1.288373261483287
;  0.9628381861738933
;  1.1102991043285948)

  (take 5 (repeatedly #(pull-arm my-bandit 6))))
; (-1.5243679178863392
;  -3.203919353507713
;  -1.5466509360251393
;  -2.498546230565993
;  -1.22230652786426)
