(ns adventofcode-2020.exercises.day-7.part-2)

(require '[clojure.set :as set])

(defn make-set
  [inputs]
  (reduce
    (fn [hm line]
      (let [[[_ _ bag] & inside] (re-seq #"(?:(\d+)|^)\s?(\w+\s\w+)\sbag" line)
            bag-key (keyword bag)
            bag-val (bag-key hm)]
        (assoc 
            hm 
            bag-key 
            (reduce 
              (fn [cur-bag [_ n inside-bag]]
                (assoc cur-bag (keyword inside-bag) (Integer/parseInt n)))
              bag-val
              inside))))
    {}
    inputs))

(defn count-children
  [graph target]
  (let [node (target graph)]
    (reduce
        (fn [a k]
          (+ 
            a 
            (k node) 
            (* (k node) (count-children graph k))))
        0
        (keys node))))

(defn run
  [inputs]
  (count-children (make-set inputs) (keyword "shiny gold")))
