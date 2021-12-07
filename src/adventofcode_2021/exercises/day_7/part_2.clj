(ns adventofcode-2021.exercises.day-7.part-2)

(require '[clojure.string :as str])

(defn calculate-fuel-cost
  [crab pos]
  (let [distance (Math/abs (- crab pos))]
    (/ (* distance (inc distance)) 2)))

(defn run
  [inputs]
  (let [crabs (map #(Integer/parseInt %) (str/split (first inputs) #","))
        all-pos (range (apply min crabs) (inc (apply max crabs)))]
    (reduce
      (fn [best pos]
        (min (apply + (map #(calculate-fuel-cost % pos) crabs)) best))
      ##Inf
      all-pos)))
