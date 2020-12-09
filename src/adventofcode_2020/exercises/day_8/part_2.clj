(ns adventofcode-2020.exercises.day-8.part-2)

(require '[clojure.string :as str])

(defn get-acc
  [inputs switch]
  (loop [visited #{}
         acc 0
         n 0]
    (if (>= n (count inputs))
      acc
      (let [line (nth inputs n)
            [cmd param] (str/split line #"\s")
            number (Integer/parseInt param)]
        (if (contains? visited n)
          nil
          (recur
            (conj visited n)
            (if (= cmd "acc")
              (+ acc number)
              acc)
            (if (or 
                  (and (= cmd "jmp") (not= n switch))
                  (and (= cmd "nop") (= n switch)))
              (+ n number)
              (inc n))))))))

(defn run
  [inputs]
  (loop [n 0]
    (let [acc (get-acc inputs n)]
      (if (nil? acc)
        (recur (inc n))
        acc))))
