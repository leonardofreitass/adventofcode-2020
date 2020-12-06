(ns adventofcode-2020.exercises.day-6.part-1)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn process-customs
  [inputs]
  (loop [current-question #{}
         questions inputs
         answers 0]
    (if (empty? questions)
      (+ answers (count current-question))
      (let [line (first questions)
            eod (= line "")
            new-question (set/union current-question (set (seq line)))]
        (recur
          (if eod #{} new-question)
          (drop 1 questions)
          (if eod
            (+ answers (count new-question))
            answers))))))

(defn run
  [inputs]
  (process-customs inputs))
