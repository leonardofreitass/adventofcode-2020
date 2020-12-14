(ns adventofcode-2020.exercises.day-14.part-1)

(require '[clojure.string :as str])

(defn stract-mask
  [line]
  (let [[_ _ mask] (str/split line #" ")]
    mask))

(defn mask-number
  [mask binary-num]
  (reduce-kv
    (fn [acc index num]
      (let [mask-num (subs mask index (inc index))]
        (str acc (if (= mask-num "X") num mask-num))))
    ""
    (vec binary-num)))

(def bin-size 36)

(defn pad-left
  [number]
  (if (= bin-size (count number))
    number
    (str (str/join "" (repeat (- bin-size (count number)) 0)) number)))

(defn stract-number
  [mask line]
  (let [[_ pos-str num-str] (re-matches #"mem\[(\d+)\] = (\d+)" line)
        pos (Integer/parseInt pos-str)
        num (Integer/parseInt num-str)
        binary-num (pad-left (Integer/toString num 2))
        masked-num (mask-number mask binary-num)]
      [pos (Long/valueOf masked-num 2)]))

(defn iterate-writes
  [inputs]
  (loop [writes inputs
         mask ""
         memory {}]
    (if (empty? writes)
      memory
      (let [line (first writes)
            is-mask (str/starts-with? line "mask")
            new-mask (if is-mask (stract-mask line) mask)
            [pos number] (if is-mask [] (stract-number new-mask line))] 
        (recur (drop 1 writes) new-mask (if is-mask memory (assoc memory pos number)))))))

(defn run
  [inputs]
  (reduce + (vals (iterate-writes inputs))))

