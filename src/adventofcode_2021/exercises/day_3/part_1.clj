(ns adventofcode-2021.exercises.day-3.part-1)

(require '[clojure.string :as str])

(defn transpose
  [matrix]
  (apply map list matrix))

(defn power-consumption
  [readings]
  (apply * (map #(Integer/parseInt % 2) readings)))

(defn run
  [inputs]
  (power-consumption
    (reduce
      (fn [[gamma epsilon] line]
        (let [freq (frequencies line)
              g (key (apply max-key val freq))
              e (key (apply min-key val freq))]
          [(str gamma g) (str epsilon e)]))
      ["" ""]
      (transpose (map #(str/split % #"") inputs)))))
