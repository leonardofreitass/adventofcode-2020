(ns adventofcode-2020.exercises.day-12.part-2)

(def move-waypoint-cmds #{"N" "E" "S" "W"})
(def move-ship-cmds #{"F"})
(def turn-waypoint-cmds #{"L" "R"})

(defn abs [n] (max n (- n)))

(def direction-moves
  {"N" [0 1]
   "E" [1 0]
   "S" [0 -1]
   "W" [-1 0]})

(defn turn-waypoint
  [cur-pos command n]
  (if (not (contains? turn-waypoint-cmds command))
    cur-pos
    (loop [i (/ n 90)
           [x y] cur-pos]
        (if (= i 0)
          [x y]
          (recur
            (dec i)
            [(if (= command "L") (- y) y) (if (= command "R") (- x) x)])))))

(defn move-ship
  [cur-pos [nx ny] cmd-dir n]
  (if (not (contains? move-ship-cmds cmd-dir))
    cur-pos
    (loop [i n
            [x y] cur-pos]
      (if (= i 0)
        [x y]
        (recur (dec i) [(+ x nx) (+ y ny)])))))

(defn move-waypoint
  [cur-waypoint cmd-dir n]
  (if (not (contains? move-waypoint-cmds cmd-dir))
    cur-waypoint
    (let [[nx ny] (get direction-moves cmd-dir)]
      (loop [i n
             [x y] cur-waypoint]
        (if (= i 0)
          [x y]
          (recur (dec i) [(+ x nx) (+ y ny)]))))))

(defn execute-inputs
  [inputs]
  (loop [commands inputs
         current-pos [0 0]
         current-waypoint [10 1]]
    (if (empty? commands)
      current-pos
      (let [[_ command n-str] (re-matches #"(\w)(\d+)" (first commands))
            n (Integer/parseInt n-str)
            turned-waypoint (turn-waypoint current-waypoint command n)
            new-pos (move-ship current-pos current-waypoint command n)
            new-waypoint (move-waypoint turned-waypoint command n)]
        (recur (drop 1 commands) new-pos new-waypoint)))))

(defn run
  [inputs]
  (let [[x y] (execute-inputs inputs)]
    (+ (abs x) (abs y))))
