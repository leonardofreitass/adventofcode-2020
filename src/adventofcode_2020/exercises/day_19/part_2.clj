(ns adventofcode-2020.exercises.day-19.part-2)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def SOLUTIONS (atom {}))

(defn parse-rules
  [rules]
  (reduce
    (fn [acc rule]
      (let [[_ id rest] (re-matches #"^(\d+): (.*)$" rule)]
        (assoc acc id rest)))
    {}
    rules))

(defn expand-rule
  [rules id]
  (let [existing-solution (get @SOLUTIONS id)
        rule (get rules id)]
    (cond
      (not (nil? existing-solution))
        existing-solution
      (str/starts-with? rule "\"")
        (let [[_ lit] (re-matches #"^\"\w+\"$" rule)]
          (swap! SOLUTIONS assoc id lit)
          lit)
      :else
        (let [split-rule (str/split rule #" \| ")
              routines (map #(str/split % #"\s") split-rule)
              rec (map
                    (fn [routine] (str/join "" (map #(expand-rule rules %) routine)))
                    routines)
              solution (str "(?:" (str/join "|" rec) ")")]
          (swap! SOLUTIONS assoc id solution)
          solution))))

(def replacements
  [["8: 42" "8: 42 | 42 8"]
   ["11: 42 31" "11: 42 31 | 42 11 31"]])

(def max-size 101)

(defn update-rules
  [rules]
  (reduce
    (fn [acc [id new-rule]]
      (let [index (.indexOf rules id)]
        (concat
          (take index acc)
          [new-rule]
          (drop (inc index) acc))))
    rules
    replacements))

(defn prefil-solutions
  [rules]
  (expand-rule rules "42")
  (expand-rule rules "31")
  (swap! SOLUTIONS assoc "8" (str "(?:" (get @SOLUTIONS "42") "+)"))
  (swap!
    SOLUTIONS
    assoc
    "11"
    (str
      "("
      (let [forty-two-rule (get @SOLUTIONS "42")
            thirty-one-rule (get @SOLUTIONS "31")]
        (str/join
          "|"
          (map
            #(str "(?:" forty-two-rule "{" % "}" thirty-one-rule "{" % "})")
            (range 1 max-size))))
      ")")))

(defn run
  [inputs]
  (let [bp (.indexOf inputs "")
        rules (parse-rules (update-rules (take bp inputs)))
        messages (drop (inc bp) inputs)
        _ (prefil-solutions rules)
        zero-rule (str "^" (expand-rule rules "0") "$")]
    (count (filter #(not (nil? (re-seq (re-pattern zero-rule) %))) messages))))
