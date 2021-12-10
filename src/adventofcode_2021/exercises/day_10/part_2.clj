(ns adventofcode-2021.exercises.day-10.part-2)

(require '[clojure.string :as str])

(defn parse-inputs
  [inputs]
  (map #(str/split % #"") inputs))

(def points {
  ")" 1,
  "]" 2,
  "}" 3,
  ">" 4
})

(def opening #{"(" "{" "<" "["})

(def expected-pair {
  "(" ")",
  "{" "}",
  "<" ">",
  "[" "]"
})

(defn autocomplete-points
  [line]
  (loop [rest line
         expected '()]
    (if (empty? rest)
      (if (empty? expected)
        0
        (reduce #(+ (get points %2) (* 5 %1)) 0 expected))
      (let [char (first rest)
            matching-pair (get expected-pair char)
            is-opening (contains? opening char)
            closing-expected (first expected)]
        (if (and (not is-opening) (not= char closing-expected))
          0
          (recur
            (next rest)
            (if is-opening (conj expected matching-pair) (pop expected))))))))

(defn generate-points
  [inputs]
  (reduce
    (fn [acc line]
      (let [line-points (autocomplete-points line)]
        (if (zero? line-points)
          acc
          (conj acc line-points))))
    []
    (parse-inputs inputs)))

(defn run
  [inputs]
  (let [all-points (generate-points inputs)]
    (nth (sort all-points) (/ (dec (count all-points)) 2))))
