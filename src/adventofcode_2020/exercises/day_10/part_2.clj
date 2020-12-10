(ns adventofcode-2020.exercises.day-10.part-2)

(def SOLUTIONS (atom {}))

(defn count-possibilities
  [numbers]
  (if (= 1 (count numbers))
    1
    (let [start (first numbers)]
      (loop [i 1
             acc 0]
        (if (or (>= i (count numbers)) (> (- (nth numbers i) start) 3))
          acc
          (let [n (- (count numbers) i)
                solved-result (get @SOLUTIONS n)
                result (if (nil? solved-result) (count-possibilities (drop i numbers)) solved-result)]
            (if (nil? solved-result) (swap! SOLUTIONS assoc n result))
            (recur
              (inc i)
              (+ acc result))))))))

(defn run
  [inputs]
  (let [sorted (sort (map #(Integer/parseInt %) inputs))
        all (concat (conj sorted 0) [(+ 3 (last sorted))])]
    (count-possibilities all)))
