(ns adventofcode-2020.exercises.day-3.part-1)

(require '[clojure.string :as str])

(defn navigate
  [chart step-x step-y]
  (let [charter-size (count (first chart))]
    (loop [x 0
           y 0
           trees 0]
      (if (>= y (count chart))
        trees
        (let [pos (nth (nth chart y) x)
              next-x (rem (+ step-x x) charter-size)
              next-y (+ step-y y)
              has-tree (= pos \#)]
          (recur next-x next-y (if has-tree (inc trees) trees)))))))

(defn run
  [inputs]
  (navigate inputs 3 1))
