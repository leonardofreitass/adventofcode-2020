(ns adventofcode-2020.exercises.day-8.part-1)

(require '[clojure.string :as str])

(defn run
  [inputs]
  (loop [visited #{}
         acc 0
         n 0]
    (let [line (nth inputs n)
          [cmd param] (str/split line #"\s")
          number (Integer/parseInt param)]
      (if (contains? visited n)
        acc
        (recur
          (conj visited n)
          (if (= cmd "acc")
            (+ acc number)
            acc)
          (if (= cmd "jmp")
            (+ n number)
            (inc n)))))))
