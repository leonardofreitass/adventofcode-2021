(ns adventofcode-2021.exercises.day-10.part-1)

(require '[clojure.string :as str])

(defn parse-inputs
  [inputs]
  (map #(str/split % #"") inputs))

(def points {
  ")" 3,
  "]" 57,
  "}" 1197,
  ">" 25137
})

(def opening #{"(" "{" "<" "["})

(def expected-pair {
  "(" ")",
  "{" "}",
  "<" ">",
  "[" "]"
})

(defn first-ilegal-character
  [line]
  (loop [rest line
         expected '()]
    (if (empty? rest)
      nil
      (let [char (first rest)
            matching-pair (get expected-pair char)
            is-opening (contains? opening char)
            closing-expected (first expected)]
        (if (and (not is-opening) (not= char closing-expected))
          char
          (recur
            (next rest)
            (if is-opening (conj expected matching-pair) (pop expected))))))))

(defn run
  [inputs]
  (reduce
    (fn [acc line]
      (let [ilegal-character (first-ilegal-character line)]
        (if (nil? ilegal-character)
          acc
          (+ acc (get points ilegal-character)))))
    0
    (parse-inputs inputs)))
