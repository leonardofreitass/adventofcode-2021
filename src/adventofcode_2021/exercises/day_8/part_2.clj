(ns adventofcode-2021.exercises.day-8.part-2)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn slice [coll a b] (drop a (take (+ b a) coll)))

(defn parse-inputs
  [inputs]
  (map (fn [line] 
          (map #(map sort (str/split (str/trim %) #" ")) (str/split line #" \| ")))
       inputs))

(defn intersec
  [src target]
  (count (set/intersection src (set target))))

(defn decode-five-seg
  [five-seg n one four]
  (loop [rest five-seg]
    (let [seg (first rest)]
      (if (= n 1)
        [seg 2]
        (if (and (= n 2) (= 3 (intersec four seg)))
          [seg 5]
          (if (and (= n 3) (= 2 (intersec one seg)))
            [seg 3]
            (recur (next rest))))))))

(defn decode-six-seg
  [five-seg n one four]
  (loop [rest five-seg]
    (let [seg (first rest)]
      (if (= n 1)
        [seg 6]
        (if (and (= n 2) (= 2 (intersec one seg)))
          [seg 0]
          (if (and (= n 3) (= 4 (intersec four seg)))
            [seg 9]
            (recur (next rest))))))))

(defn decode-seg-coll
  [seg-coll dc-fn refs initial-dict]
  (loop [rest seg-coll
         dict initial-dict]
    (if (empty? rest)
      dict
      (let [[seg number] (apply dc-fn rest (count rest) refs)]
        (recur (disj rest seg) (assoc dict seg number))))))

(defn decode
  [patterns]
  (let [sorted-patterns (sort-by count patterns)
        [one seven four _ _ _ _ _ _ eight] sorted-patterns
        five-seg (set (slice sorted-patterns 3 3))
        six-seg (set (slice sorted-patterns 6 3))
        dict (assoc {} one 1 seven 7 four 4 eight 8)
        refs [(set one) (set four)]]
    (decode-seg-coll 
      five-seg decode-five-seg refs
      (decode-seg-coll six-seg decode-six-seg refs dict))))

(defn display
  [dict output]
  (Integer/parseInt (apply str (map #(get dict %) output))))

(defn run
  [inputs]
  (reduce
    (fn [acc [patterns output]]
      (let [dict (decode patterns)]
        (+ acc (display dict output))))
    0
    (parse-inputs inputs)))
