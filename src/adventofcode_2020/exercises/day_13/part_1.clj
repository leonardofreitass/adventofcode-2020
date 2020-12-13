(ns adventofcode-2020.exercises.day-13.part-1)

(require '[clojure.string :as str])

(defn min-by
  [c k]
  (reduce #(if (< (k %2) (k %1)) %2 %1) c))

(defn run
  [inputs]
  (let [arrival (Integer/parseInt (first inputs))
        bus-schedules-str (str/split (second inputs) #",")
        bus-schedules (map #(Integer/parseInt %) (filter #(not= "x" %) bus-schedules-str))
        bus-timetable (map #(hash-map :interval % :next (* % (Math/ceil (/ arrival %)))) bus-schedules)
        next-bus (min-by bus-timetable :next)]
    (* (:interval next-bus) (- (:next next-bus) arrival))))
