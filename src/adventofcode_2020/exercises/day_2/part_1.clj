(ns adventofcode-2020.exercises.day-2.part-1)

(defn is-valid-password
  [min max letter password]
  (let [occ (count (re-seq (re-pattern letter) password))]
    (<= min occ max)))

(defn run
  [inputs]
  (reduce 
    (fn [total input] 
      (let [[_ min max letter password] (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]+)" input)]
        (if (is-valid-password (Integer/parseInt min) (Integer/parseInt max) letter password)
          (inc total)
          total)))
    0
    inputs))
