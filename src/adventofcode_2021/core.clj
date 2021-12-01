(ns adventofcode-2021.core
  (:require [adventofcode-2021.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise part]]
  (exercises/execute exercise part))
