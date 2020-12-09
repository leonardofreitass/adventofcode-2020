(ns adventofcode-2020.exercises.day-9.part-2)

(def preamble-size 65)

(defn find-pair
  [inputs target]
  (loop [numbers inputs
         hs #{}]
    (if (empty? numbers)
      nil
      (let [number (first numbers)
            pair (- target number)]
        (if (contains? hs number)
          (* number pair)
          (recur 
            (drop 1 numbers)
            (conj hs pair)))))))

(defn find-invalid
  [inputs]
  (loop [numbers inputs]
    (let [target (nth numbers preamble-size)
          pair (find-pair (take preamble-size numbers) target)]
      (if (nil? pair)
        target
        (recur (drop 1 numbers))))))

(defn sum-until-target
  [inputs target]
  (loop [n 2]
    (let [numbers (take n inputs)
          s (reduce + numbers)]
      (if (= s target)
        numbers
        (if (> s target)
          nil
          (recur (inc n)))))))

(defn run
  [str-inputs]
  (let [inputs (map #(Integer/parseInt %) str-inputs)
        invalid (find-invalid inputs)]
    (loop [numbers inputs]
      (let [sequence (sum-until-target numbers invalid)]
        (if (nil? sequence)
          (recur (drop 1 numbers))
          (+ (apply min sequence) (apply max sequence)))))))
