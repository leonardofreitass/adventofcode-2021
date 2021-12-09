(ns adventofcode-2021.exercises.day-9.part-1)

(require '[clojure.string :as str])

(defn parse-inputs
  [inputs]
  (vec (map (fn [line] (vec (map #(Integer/parseInt %) (str/split line #"")))) inputs)))

(defn adjacent-pos
  [r c]
  [[(inc r) c] [(dec r) c] [r (inc c)] [r (dec c)]])

(defn run
  [inputs]
  (let [grid (parse-inputs inputs)]
    (reduce-kv
      (fn [racc ridx row]
        (reduce-kv
          (fn [acc idx item]
            (let [adjacent (map #(get-in grid % ##Inf) (adjacent-pos ridx idx))
                  lower (filter #(<= % item) adjacent)]
              (if (empty? lower)
                (+ acc (inc item))
                acc)))
          racc
          row))
      0
      grid)))
