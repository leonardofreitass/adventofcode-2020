(ns adventofcode-2020.exercises.day-5.part-1)

(require '[clojure.string :as str])

(def lower "FL")
(def upper "BR")

(defn binary-search
  [instructions upper-bound]
  (loop [left instructions
         min 0
         max upper-bound]
    (if (empty? left)
      min
      (let [instruction (subs left 0 1)
            half (/ (+ min max) 2)
            lower-half (str/includes? lower instruction)]
        (recur
          (subs left 1)
          (if lower-half min half)
          (if lower-half half max))))))

(defn parse-boarding-pass
  [boarding-pass]
  (let [[_ r c] (re-matches #"([F|B]{7})([R|L]{3})" boarding-pass)
        row (binary-search r 128)
        column (binary-search c 8)]
    (+ (* row 8) column)))

(defn run
  [inputs]
  (reduce
    (fn [max boarding-pass]
      (let [parsed (parse-boarding-pass boarding-pass)]
        (if (> parsed max)
          parsed
          max)))
    0
    inputs))
