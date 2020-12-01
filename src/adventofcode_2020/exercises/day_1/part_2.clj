(ns adventofcode-2020.exercises.day-1.part-2)

(defn make-trios
  [inputs]
  (for [x (range (count inputs)) 
        y (range (count inputs)) 
        z (range (count inputs)) 
        :when (> z y x)
        :let [a (nth inputs x)
              b (nth inputs y)
              c (nth inputs z)]]
      [(Integer/parseInt a) (Integer/parseInt b) (Integer/parseInt c)]))

(defn run
  [inputs]
  (loop [trios (make-trios inputs)]
    (let [[a b c] (first trios)]
      (if (= 2020 (+ a b c))
        (* a b c)
        (recur (drop 1 trios))))))
