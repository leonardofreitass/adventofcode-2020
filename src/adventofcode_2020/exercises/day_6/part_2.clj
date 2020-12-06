(ns adventofcode-2020.exercises.day-6.part-2)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn process-customs
  [inputs]
  (loop [current-question nil
         questions inputs
         answers 0]
    (if (empty? questions)
      (+ answers (count current-question))
      (let [line (first questions)
            eod (= line "")
            new-set (set (seq line))
            new-question (if (nil? current-question) new-set (set/intersection current-question new-set))]
        (recur
          (if eod nil new-question)
          (drop 1 questions)
          (if eod
            (+ answers (count current-question))
            answers))))))

(defn run
  [inputs]
  (process-customs inputs))
