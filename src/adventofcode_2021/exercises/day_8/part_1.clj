(ns adventofcode-2021.exercises.day-8.part-1)

(require '[clojure.string :as str])

(defn parse-inputs
  [inputs]
  (map (fn [line] 
          (map #(str/split (str/trim %) #" ") (str/split line #" \| ")))
       inputs))

(defn run
  [inputs]
  (reduce
    (fn [acc [patterns output]]
      (+ acc (count (filter #(contains? #{2 3 4 7} (count %)) output))))
    0
    (parse-inputs inputs)))
