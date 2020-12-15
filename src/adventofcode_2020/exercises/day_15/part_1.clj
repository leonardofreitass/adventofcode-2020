(ns adventofcode-2020.exercises.day-15.part-1)

(require '[clojure.string :as str])

(def l 2020)

(defn run
  [inputs]
  (let [numbers (str/split (first inputs) #",")]
    (loop [s (count numbers)
           hm (reduce-kv #(assoc %1 (Integer/parseInt %3) (inc %2)) {} numbers)
           n (Integer/parseInt(last numbers))]
      (if (= l s)
        n
        (let [new-hm (assoc hm n s)
              last-n (get hm n)
              new-n (if (nil? last-n) 0 (- s last-n))]
          (recur (inc s) new-hm new-n))))))

