(ns adventofcode-2020.exercises.day-3.part-2)

(require '[clojure.string :as str])

(def navigations
  [[1 1]
   [3 1]
   [5 1]
   [7 1]
   [1 2]])

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
  (reduce
    (fn [total [step-x step-y]]
      (* total (navigate inputs step-x step-y)))
    1
    navigations))
