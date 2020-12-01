(ns adventofcode-2020.exercises.day-1.part-1)

(defn make-pairs
  [inputs]
  (for [x (range (count inputs)) 
        y (range (count inputs)) 
        :when (> y x)
        :let [a (nth inputs x)
              b (nth inputs y)]]
      [(Integer/parseInt a) (Integer/parseInt b)]))

(defn run
  [inputs]
  (loop [pairs (make-pairs inputs)]
    (let [[a b] (first pairs)]
      (if (= 2020 (+ a b))
        (* a b)
        (recur (drop 1 pairs))))))
