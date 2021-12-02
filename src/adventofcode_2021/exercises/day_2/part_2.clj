(ns adventofcode-2021.exercises.day-2.part-2)

(require '[clojure.string :as str])

(defn exec-command
  [[pos depth aim] line]
  (let [[cmd n] (str/split line #" ")
        number (Integer/parseInt n)]
    (case cmd
      "forward" [(+ pos number) (+ depth (* aim number)) aim]
      "down" [pos depth (+ aim number)]
      "up" [pos depth (- aim number)])))

(defn run
  [inputs]
  (apply * (pop (reduce exec-command [0 0 0] inputs))))
