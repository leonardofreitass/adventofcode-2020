(ns adventofcode-2020.exercises.day-18.part-1)

(require '[clojure.string :as str])

(def number-re #"^(\d+)$")
(def simple-exp-re #"^(.*) ([\+\*]) (\d+)$")
(def rec-exp-re #"^(.*)\(([^()]+)\)(.*)$")

(defn eval-exp
  [exp]
  (cond
    (str/includes? exp "(")
      (let [[_ s rec-exp e] (re-find rec-exp-re exp)]
        (eval-exp (str s (eval-exp rec-exp) e)))
    (not (nil? (re-find number-re exp)))
      (Integer/parseInt exp)
    :else
      (let [[_ s op e] (re-find simple-exp-re exp)
            l (eval-exp s)
            r (Integer/parseInt e)]
        (if (= op "+") (+' l r) (*' l r)))))

(defn run
  [inputs]
  (reduce #(+' %1 (eval-exp %2)) 0 inputs))
