(ns adventofcode-2020.exercises.day-13.part-2)

(require '[clojure.string :as str])
(defn abs [n] (max n (- n)))

(defn xgcd
  "I proudly adapted this from somewhere else heh"
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s-a 0
                     s-b 1
                     t-a 1
                     t-b 0
                     r-a (abs b)
                     r-b (abs a)]
                (if (zero? r-a)
                  [r-b s-b t-b]
                  (let [q (quot r-b r-a)]
                    (recur (- s-b (* q s-a)) s-a
                           (- t-b (* q t-a)) t-a
                           (- r-b (* q r-a)) r-a))))))

(defn chinese-remainder
  [pairs]
  (let [prod (reduce #(* %1 (first %2)) 1 pairs)]
    (mod 
      (reduce
        (fn [sum [n r]]
          (let [p (quot prod n)
                [_ x] (xgcd p n)]
            (+ sum (* r x p))))
        0
        pairs)
      prod)))

(defn parse-numbers
  [numbers]
  (reduce-kv
    (fn [acc index number]
      (if (= number "x")
        acc
        (let [n (Integer/parseInt number)]
          (conj acc [n (- n index)]))))
    []
    numbers))

(defn run
  [inputs]
  (let [pairs (parse-numbers (str/split (second inputs) #","))]
    (chinese-remainder pairs)))
