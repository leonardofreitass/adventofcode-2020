(ns adventofcode-2020.exercises.day-19.part-1)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def SOLUTIONS (atom {}))

(defn parse-rules
  [rules]
  (reduce
    (fn [acc rule]
      (let [[_ id rest] (re-matches #"^(\d+): (.*)$" rule)
            val (str/split rest #" \| ")]
        (assoc acc id (map #(str/split % #"\s") val))))
    {}
    rules))

(defn expand-rule
  [rules id]
  (let [rule (get rules id)]
    (mapcat
      (fn [routines]
        (loop [next-routines routines
               parsed [""]]
          (if (empty? next-routines)
            parsed
            (let [routine (first next-routines)
                  [_ lit] (re-matches #"\"(\w)\"" routine)
                  solved-solution (get @SOLUTIONS routine)
                  solution (if (and (nil? solved-solution) (nil? lit)) (expand-rule rules routine) solved-solution)]
              (if (nil? solved-solution) (swap! SOLUTIONS assoc routine solution))
              (recur
                (drop 1 next-routines)
                (if (not (nil? lit))
                  (map #(str % lit) parsed)
                  (for [acc parsed
                        next solution]
                    (str acc next))))))))
      rule)))

(defn run
  [inputs]
  (let [bp (.indexOf inputs "")
        rules (parse-rules (take bp inputs))
        messages (set (drop (inc bp) inputs))
        zero-rule (set (expand-rule rules "0"))]
    (count (set/intersection messages zero-rule))))
