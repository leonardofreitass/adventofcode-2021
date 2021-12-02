(ns adventofcode-2021.exercises.day-2.part-1)

(require '[clojure.string :as str])

(defn exec-command
  [[pos depth] line]
  (let [[cmd n] (str/split line #" ")
        number (Integer/parseInt n)]
    (case cmd
      "forward" [(+ pos number) depth]
      "down" [pos (+ depth number)]
      "up" [pos (- depth number)])))

(defn run
  [inputs]
  (apply * (reduce exec-command [0 0] inputs)))
