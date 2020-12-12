(ns adventofcode-2020.exercises.day-12.part-1)

(def move-cmds #{"N" "E" "S" "W" "F"})
(def turn-cmds #{"L" "R"})

(def directions ["N" "E" "S" "W"])

(defn abs [n] (max n (- n)))

(def direction-moves
  {"N" [0 1]
   "E" [1 0]
   "S" [0 -1]
   "W" [-1 0]})

(defn turn
  [cur-dir command n]
  (if (not (contains? turn-cmds command))
    cur-dir
    (let [max-dir (count directions)
          step (/ n 90)
          cur-pos (.indexOf directions cur-dir)]
    (if (= command "R")
      (nth directions (rem (+ cur-pos step) max-dir))
      (nth directions (rem (+ max-dir (rem (- cur-pos step) max-dir)) max-dir))))))

(defn move
  [cur-pos cur-dir cmd-dir n]
  (if (not (contains? move-cmds cmd-dir))
    cur-pos
    (let [command (if (= cmd-dir "F") cur-dir cmd-dir)
          [nx ny] (get direction-moves command)]
      (loop [i n
             [x y] cur-pos]
        (if (= i 0)
          [x y]
          (recur (dec i) [(+ x nx) (+ y ny)]))))))

(defn execute-inputs
  [inputs]
  (loop [commands inputs
         current-dir "E"
         current-pos [0 0]]
    (if (empty? commands)
      current-pos
      (let [[_ command n-str] (re-matches #"(\w)(\d+)" (first commands))
            n (Integer/parseInt n-str)
            new-dir (turn current-dir command n)
            new-pos (move current-pos current-dir command n)]
        (recur (drop 1 commands) new-dir new-pos)))))

(defn run
  [inputs]
  (let [[x y] (execute-inputs inputs)]
    (+ (abs x) (abs y))))
