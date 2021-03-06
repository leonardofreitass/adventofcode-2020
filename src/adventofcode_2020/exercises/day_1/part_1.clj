(ns adventofcode-2020.exercises.day-1.part-1)

(defn find-pair
  [inputs target]
  (loop [numbers inputs
         hs #{}]
    (if (empty? numbers)
      nil
      (let [number (Integer/parseInt  (first numbers))
            pair (- target number)]
        (if (contains? hs number)
          (* number pair)
          (recur 
            (drop 1 numbers)
            (conj hs pair)))))))

(defn run
  [inputs]
  (find-pair inputs 2020))
