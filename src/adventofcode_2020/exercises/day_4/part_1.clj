(ns adventofcode-2020.exercises.day-4.part-1)

(require '[clojure.string :as str])

(def required-fields
  [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn passport?
  [document]
  (every? #(contains? document %) required-fields))

(defn make-document
  [document-line]
  (reduce
    (fn [hm kv]
      (let [[key value] (str/split kv #":")]
        (assoc hm (keyword key) value)))
    {}
    (str/split document-line #"\s")))

(defn process-documents
  [inputs]
  (loop [current-document ""
         file inputs
         passports 0]
    (if (empty? file)
      (if (passport? (make-document current-document))
        (inc passports) 
        passports)
      (let [line (first file)
            eod (= line "")
            new-document (str/join " " [current-document line])]
        (recur
          (if eod "" new-document)
          (drop 1 file)
          (if eod
            (if (passport? (make-document current-document))
              (inc passports) 
              passports)
            passports))))))

(defn run
  [inputs]
  (process-documents inputs))
