(ns adventofcode-2020.exercises.day-10.part-1)

(defn run
  [inputs]
  (let [sorted (sort (map #(Integer/parseInt %) inputs))
        all (concat (conj sorted 0) [(+ 3 (last sorted))])]
    (loop [numbers all
           diffs [0 0 0 0]]
      (if (<= (count numbers) 1)
        (* (nth diffs 1) (nth diffs 3))
        (let [left (first numbers)
              right (second numbers)
              d (- right left)]
          (if (> d 3)
            (* (nth diffs 1) (nth diffs 3))
            (recur
              (drop 1 numbers)
              (assoc diffs d (inc (nth diffs d))))))))))
