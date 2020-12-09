(ns adventofcode-2020.exercises.day-9.part-1)

(def preamble-size 25)

(defn find-pair
  [inputs target]
  (loop [numbers inputs
         hs #{}]
    (if (empty? numbers)
      nil
      (let [number (Integer/parseInt (first numbers))
            pair (- target number)]
        (if (contains? hs number)
          (* number pair)
          (recur 
            (drop 1 numbers)
            (conj hs pair)))))))

(defn run
  [inputs]
  (loop [numbers inputs]
    (let [target (Integer/parseInt (nth numbers preamble-size))
          pair (find-pair (take preamble-size numbers) target)]
      (if (nil? pair)
        target
        (recur (drop 1 numbers))))))
