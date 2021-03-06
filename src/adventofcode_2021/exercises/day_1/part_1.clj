(ns adventofcode-2021.exercises.day-1.part-1)

(def window 1)

(defn run
  [inputs]
  (loop [numbers (map #(Integer/parseInt %) inputs)
         acc 0]
    (if (= window (count numbers))
      acc
      (recur
        (next numbers)
        (if (> (nth numbers window) (first numbers)) (inc acc) acc)))) )
