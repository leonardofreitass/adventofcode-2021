(ns adventofcode-2021.exercises.day-3.part-2)

(require '[clojure.string :as str])

(defn transpose
  [matrix]
  (apply map list matrix))

(defn life-support
  [readings]
  (apply * (map #(Integer/parseInt (apply str %) 2) readings)))

(defn filter-criteria
  [bit-criteria draw-value freq]
  (if (= (get freq "0") (get freq "1"))
    draw-value
    (key (apply bit-criteria val freq))))

(defn reading
  [bit-criteria draw-value initial-matrix]
  (loop [matrix initial-matrix
         pos 0]
    (let [freq (frequencies (nth (vec matrix) pos))
          filter-crit (filter-criteria bit-criteria draw-value freq)
          filtered-matrix (filter #(= (nth % pos) filter-crit) (transpose matrix))]
      (if (= 1 (count filtered-matrix))
        (first filtered-matrix)
        (recur (transpose filtered-matrix) (inc pos))))))

(defn run
  [inputs]
  (let [matrix (transpose (map #(str/split % #"") inputs))
        oxygen-generator (reading max-key "1" matrix)
        co2-scrubber (reading min-key "0" matrix)]
    (life-support [oxygen-generator co2-scrubber])))
