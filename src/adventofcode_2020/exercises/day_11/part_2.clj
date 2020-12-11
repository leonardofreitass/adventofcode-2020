(ns adventofcode-2020.exercises.day-11.part-2)

(require '[clojure.string :as str])

(def directions
  [[-1 -1]
   [0 -1]
   [1 -1]
   [1 0]
   [1 1]
   [0 1]
   [-1 1]
   [-1 0]])

(defn count-neighbours
  [m x y]
  (let [max-y (dec (count m))
        max-x (dec (count (first m)))]
    (reduce
      (fn [acc [accx accy]]
        (loop [nx (+ x accx)
               ny (+ y accy)]
          (if (or
                (not (<= 0 nx max-x))
                (not (<= 0 ny max-y)))
            acc
            (let [cell (nth (nth m ny) nx)]
              (if (= cell "L")
                acc
                (if (= cell "#")
                  (inc acc)
                  (recur (+ nx accx) (+ ny accy))))))))
      0
      directions)))

(defn advance
  [current-map]
  (map-indexed
    (fn [y row]
      (map-indexed
        (fn [x cell]
          (let [neighbours (count-neighbours current-map x y)]
            (if (and (= cell "L") (= neighbours 0))
              "#"
              (if (and (= cell "#") (>= neighbours 5))
                "L"
                cell))))
        row))
    current-map))

(defn advance-until-stable
  [unstable-map]
  (loop [current-map unstable-map]
    (let [new-map (advance current-map)]
      (if (= current-map new-map)
        new-map
        (recur new-map)))))

(defn run
  [inputs]
  (let [stable-map (advance-until-stable (map #(str/split % #"") inputs))]
    (reduce #(+ %1 (get (frequencies %2) "#" 0)) 0 stable-map)))
