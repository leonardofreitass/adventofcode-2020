(ns adventofcode-2020.exercises.day-16.part-2)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn extract-rules
  [line]
  (let [[name] (str/split line #":")
        m (re-seq #"\d+\-\d+" line)]
    {:name name :ranges (map (fn [g] (map #(Integer/parseInt %) (str/split g #"\-"))) m)}))

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

(defn valid-rule?
  [rule number]
  (some #(<= (first %) number (second %)) (:ranges rule)))

(defn valid-number?
  [rules number]
  (some #(valid-rule? % number) rules))

(defn valid-seq?
  [rules s]
  (every? #(valid-number? rules %) s))

(defn identify-possibilities
  [rule valid-tickets]
  (loop [i 0
         possible #{}]
    (if (>= i (count (first valid-tickets)))
      possible
      (let [column (map #(nth % i) valid-tickets)
            valid (every? #(valid-rule? rule %) column)]
        (recur (inc i) (if valid (conj possible i) possible))))))

(defn identify-rules
  [rules valid-tickets]
  (let [possible-rules (map #(assoc % :possible (identify-possibilities % valid-tickets)) rules)
        sorted-rules (sort #(compare (count (:possible %1)) (count (:possible %2))) possible-rules)]
    (loop [taken #{}
           rules-left sorted-rules
           new-rules '()]
      (if (empty? rules-left)
        new-rules
        (let [rule (first rules-left)
              columns (set/difference (:possible rule) taken)
              column (first columns)]
          (recur
            (conj taken column)
            (drop 1 rules-left)
            (conj new-rules (assoc rule :column column))))))))

(defn multiply-rules
  [rules ticket]
  (reduce
    (fn [acc rule]
      (*' acc (nth ticket (:column rule))))
    1
    rules))

(defn run
  [inputs]
  (let [[rules ticket nearby] (parse-inputs inputs)
        valid-tickets (conj (filter #(valid-seq? rules %) nearby) ticket)
        identified-rules (identify-rules rules valid-tickets)
        departure-rules (filter #(str/starts-with? (:name %) "departure") identified-rules)]
    (multiply-rules departure-rules ticket)))
