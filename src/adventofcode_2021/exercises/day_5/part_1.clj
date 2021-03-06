(ns adventofcode-2021.exercises.day-5.part-1)

(require '[clojure.string :as str])

(defn parse-inputs
  [inputs]
  (map
    (fn [line]
      (map 
        (fn [n]
          (map 
            #(Integer/parseInt %)
            (str/split n #","))) 
        (str/split line #" -> "))) 
    inputs))

(defn vertical-segment
  [ya yb]
  (range (min ya yb) (inc (max ya yb))))

(defn horizontal-segment
  [xa xb]
  (range (min xa xb) (inc (max xa xb))))

(defn run
  [inputs]
  (reduce
    #(if (> %2 1) (inc %1) %1)
    0
    (vals (reduce
      (fn [dia [[xa ya] [xb yb]]]
        (if (= xa xb)
          (reduce #(update %1 (str/join "-" [xa %2]) (fnil inc 0)) dia (vertical-segment ya yb))
          (if (= ya yb)
            (reduce #(update %1 (str/join "-" [%2 ya]) (fnil inc 0)) dia (horizontal-segment xa xb))
            dia)))
      {}
      (parse-inputs inputs)))))
