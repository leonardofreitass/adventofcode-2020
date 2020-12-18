(ns adventofcode-2020.exercises.day-18.part-2)

(require '[clojure.string :as str])

(def number-re #"^(\d+)$")
(def add-exp-re #"(\d+) [\+] (\d+)")
(def mul-exp-re #"^(.*) [\*] (\d+)$")
(def rec-exp-re #"^(.*)\(([^()]+)\)(.*)$")

(defn eval-exp
  [exp]
  (cond
    (str/includes? exp "(")
      (let [[_ s rec-exp e] (re-find rec-exp-re exp)]
        (eval-exp (str s (eval-exp rec-exp) e)))
    (not (nil? (re-find number-re exp)))
      (bigint exp)
    (not (nil? (re-find add-exp-re exp)))
      (let [[add-exp s e] (re-find add-exp-re exp)
            l (Long/valueOf s)
            r (Long/valueOf e)]
        (eval-exp (str/replace exp add-exp (str (+' l r)))))
    :else
      (let [[_ s e] (re-find mul-exp-re exp)
            l (eval-exp s)
            r (Long/valueOf e)]
        (*' l r))))

(defn run
  [inputs]
  (reduce #(+' %1 (eval-exp %2)) 0 inputs))
