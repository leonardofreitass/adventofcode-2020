(ns adventofcode-2020.core
  (:require [adventofcode-2020.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise part]]
  (exercises/execute exercise part))
