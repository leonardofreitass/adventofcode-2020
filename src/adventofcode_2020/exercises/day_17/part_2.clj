(ns adventofcode-2020.exercises.day-17.part-2)

(require '[clojure.string :as str])

(defn map-neighbours
  [[pos-x pos-y pos-z pos-w]]
  (for [x (range (dec pos-x) (+ 2 pos-x))
        y (range (dec pos-y) (+ 2 pos-y))
        z (range (dec pos-z) (+ 2 pos-z))
        w (range (dec pos-w) (+ 2 pos-w))
        :when (or (not= pos-x x) (not= pos-y y) (not= pos-z z) (not= pos-w w))]
    [x y z w]))

(defn map-bounds
  [[bound-x bound-y bound-z bound-w]]
  (for [x (range (first bound-x) (inc (second bound-x)))
        y (range (first bound-y) (inc (second bound-y)))
        z (range (first bound-z) (inc (second bound-z)))
        w (range (first bound-w) (inc (second bound-w)))]
    [x y z w]))

(defn pos->key [pos] (str/join "|" pos))

(defn key->pos [k] (map #(Integer/parseInt %) (str/split (name k) #"\|")))

(defn initial-map
  [inputs]
  (reduce-kv
    (fn [acc y line]
      (reduce-kv 
        #(assoc 
            %1 
            (keyword (pos->key [%2 (- y) 0 0]))
            {:active (= %3 "#")})
        acc
        (str/split line #"")))
    {}
    (vec inputs)))

(defn get-cell
  [hm pos]
  (let [cell ((keyword (pos->key pos)) hm)]
    (if (nil? cell)
      {:active false}
      cell)))

(defn get-bounds
  [state]
  (reduce-kv
    (fn [[[m-x M-x] [m-y M-y] [m-z M-z] [m-w M-w]] k _]
      (let [[x y z w] (key->pos k)]
        [[(min m-x (dec x)) (max M-x (inc x))]
         [(min m-y (dec y)) (max M-y (inc y))]
         [(min m-z (dec z)) (max M-z (inc z))]
         [(min m-w (dec w)) (max M-w (inc w))]]))
    [[##Inf ##-Inf] [##Inf ##-Inf] [##Inf ##-Inf] [##Inf ##-Inf]]
    state))

(defn cycle-map
  [state bounds]
  (loop [pos-left (map-bounds bounds)
         new-state state]
    (if (empty? pos-left)
      new-state
      (let [pos (first pos-left)
            cell (get-cell state pos)
            neighbours-pos (if (nil? (:neighbours cell)) (map-neighbours pos) (:neighbours cell))
            neighbours-count (reduce #(if (:active (get-cell state %2)) (inc %1) %1) 0 neighbours-pos)
            new-active (if (:active cell) (if (<= 2 neighbours-count 3) true false) (if (= neighbours-count 3) true false))]
        (recur
          (drop 1 pos-left)
          (assoc new-state (keyword (pos->key pos)) (assoc cell :active new-active :neighbours neighbours-pos)))))))

(def cycles 6)

(defn run
  [inputs]
  (let [initial-state (initial-map inputs)]
    (loop [i 0
          state initial-state
          bounds (get-bounds initial-state)]
      (if (= i cycles)
        (reduce #(if (:active %2) (inc %1) %1) 0 (vals state))
        (let [new-state (cycle-map state bounds)
              new-bounds (get-bounds new-state)]
          (recur (inc i) new-state new-bounds))))))
