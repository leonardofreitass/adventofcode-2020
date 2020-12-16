(ns adventofcode-2020.exercises.day-16.part-1)

(require '[clojure.string :as str])

(defn extract-rules
  [line]
  (let [m (re-seq #"\d+\-\d+" line)]
    (map (fn [g] (map #(Integer/parseInt %) (str/split g #"\-"))) m)))

(defn parse-ticket
  [line]
  (map #(Integer/parseInt %) (str/split line #",")))

(defn parse-inputs
  [inputs]
  (loop [lines inputs
         rules []]
    (let [line (first lines)]
      (if (= line "")
        [rules 
         (parse-ticket (nth lines 2))
         (map parse-ticket (drop 5 lines))]
        (recur 
          (drop 1 lines)
          (conj rules (extract-rules line)))))))

(defn valid?
  [rules number]
  (some
    (fn [rule] (some #(<= (first %) number (second %)) rule)) 
    rules))

(defn count-invalid
  [rules ticket]
  (reduce
    #(+ %1 (if (valid? rules %2) 0 %2))
    0
    ticket))

(defn run
  [inputs]
  (let [[rules _ nearby] (parse-inputs inputs)]
    (reduce
      (fn [acc val]
        (+ acc (count-invalid rules val)))
      0
      nearby)))
