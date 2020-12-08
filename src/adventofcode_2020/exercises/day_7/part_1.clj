(ns adventofcode-2020.exercises.day-7.part-1)

(require '[clojure.set :as set])

(defn make-set
  [inputs]
  (reduce
    (fn [hm line]
      (let [[[_ _ bag] & inside] (re-seq #"(?:(\d+)|^)\s?(\w+\s\w+)\sbag" line)
            bag-key (keyword bag)]
        (reduce
          (fn [root-hm [_ _ inside-bag]]
            (let [inside-bag-key (keyword inside-bag)]
              (assoc
                root-hm
                inside-bag-key
                (conj 
                  (inside-bag-key root-hm #{}) 
                  bag-key))))
          hm
          inside)))
    {}
    inputs))

(defn find-uniq-parents
  [graph target]
  (loop [parents #{}
         search (conj '() target)]
    (if (empty? search)
      parents
      (let [next-key (first search)
            new-parents (set/difference (next-key graph) parents)]
        (recur
          (set/union parents new-parents)
          (concat (drop 1 search) new-parents))))))

(defn run
  [inputs]
  (count (find-uniq-parents (make-set inputs) (keyword "shiny gold"))))
