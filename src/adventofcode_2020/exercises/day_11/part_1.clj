(ns adventofcode-2020.exercises.day-11.part-1)

(require '[clojure.string :as str])

(defn count-neighbours
  [m x y]
  (let [max-y (dec (count m))
        max-x (dec (count (first m)))
        neighbours (for [nx (range (dec x) (+ x 2)) 
                         ny (range (dec y) (+ y 2))
                         :when (or (not= x nx) (not= ny y))] 
              [nx ny])]
    (reduce
      (fn [acc [nx ny]]
        (if (and 
              (<= 0 nx max-x)
              (<= 0 ny max-y)
              (= "#" (nth (nth m ny) nx)))
          (inc acc)
          acc))
      0
      neighbours)))

(defn advance
  [current-map]
  (map-indexed
    (fn [y row]
      (map-indexed
        (fn [x cell]
          (let [neighbours (count-neighbours current-map x y)]
            (if (and (= cell "L") (= neighbours 0))
              "#"
              (if (and (= cell "#") (>= neighbours 4))
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
