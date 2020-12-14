(ns adventofcode-2020.exercises.day-14.part-2)

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
        (str acc (if (= mask-num "0") num mask-num))))
    ""
    (vec binary-num)))

(def bin-size 36)

(defn pad-left
  [number]
  (if (= bin-size (count number))
    number
    (str (str/join "" (repeat (- bin-size (count number)) 0)) number)))

(defn apply-permutations
  [num index]
  [(str (subs num 0 index) "0" (subs num (inc index)))
   (str (subs num 0 index) "1" (subs num (inc index)))])

(defn permutate
  [masked-pos]
  (loop [all []
         next (conj '() masked-pos)]
    (if (empty? next)
      all
      (let [num (first next)
            floating (str/index-of num "X")
            permutations (if (nil? floating) [] (apply-permutations num floating))
            new-next (drop 1 next)]
        (recur 
          (if (nil? floating) (conj all (Long/valueOf num 2)) all)
          (if (nil? floating) new-next (apply conj new-next permutations)))))))

(defn stract-number
  [mask line]
  (let [[_ pos-str num-str] (re-matches #"mem\[(\d+)\] = (\d+)" line)
        pos (Integer/parseInt pos-str)
        num (Integer/parseInt num-str)
        binary-pos (pad-left (Integer/toString pos 2))
        masked-pos (mask-number mask binary-pos)
        all-pos (permutate masked-pos)]
      [all-pos num]))

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
            [all-pos number] (if is-mask [] (stract-number new-mask line))] 
        (recur (drop 1 writes) new-mask (if is-mask memory (reduce #(assoc %1 %2 number) memory all-pos)))))))

(defn run
  [inputs]
  (reduce + (vals (iterate-writes inputs))))

