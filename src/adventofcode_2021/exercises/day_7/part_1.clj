(ns adventofcode-2021.exercises.day-7.part-1)

(require '[clojure.string :as str])

(defn run
  [inputs]
  (let [crabs (map #(Integer/parseInt %) (str/split (first inputs) #","))
        all-pos (range (apply min crabs) (inc (apply max crabs)))]
    (reduce
      (fn [best pos]
        (min (apply + (map #(Math/abs (- % pos)) crabs)) best))
      ##Inf
      all-pos)))
