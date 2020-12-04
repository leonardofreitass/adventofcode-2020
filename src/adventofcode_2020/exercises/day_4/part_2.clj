(ns adventofcode-2020.exercises.day-4.part-2)

(require '[clojure.string :as str])

(def required-fields
  {:byr (fn [v] (<= 1920 (Integer/parseInt v) 2002))
   :iyr (fn [v] (<= 2010 (Integer/parseInt v) 2020))
   :eyr (fn [v] (<= 2020 (Integer/parseInt v) 2030))
   :hgt (fn [v]
    (let [[_ n u] (re-matches  #"(\d+)(cm|in)" v)]
      (if (or (nil? n) (nil? u))
        false
        (if (= u "cm")
          (<= 150 (Integer/parseInt n) 193)
          (<= 59 (Integer/parseInt n) 76)))))
   :hcl (fn [v] (not (nil? (re-matches #"#([a-fA-F0-9]{6}|[a-fA-F0-9]{3})" v))))
   :ecl (fn [v] (not (nil? (re-matches #"amb|blu|brn|gry|grn|hzl|oth" v))))
   :pid (fn [v] (not (nil? (re-matches #"\d{9}" v))))})

(defn passport?
  [document]
  (every? 
    #(and 
      (contains? document %) 
      ((% required-fields) (% document))) 
    (keys required-fields)))

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
