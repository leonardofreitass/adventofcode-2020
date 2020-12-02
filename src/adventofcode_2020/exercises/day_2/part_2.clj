(ns adventofcode-2020.exercises.day-2.part-2)

(defn valid?
  [pos-a pos-b letter password]
  (let [a (subs password (dec pos-a) pos-a)
        b (subs password (dec pos-b) pos-b)]
    (and
      (or (= a letter) (= b letter))
      (not= a b))))

(defn run
  [inputs]
  (reduce 
    (fn [total input] 
      (let [[_ pos-a pos-b letter password] (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]+)" input)]
        (if (valid? (Integer/parseInt pos-a) (Integer/parseInt pos-b) letter password)
          (inc total)
          total)))
    0
    inputs))
