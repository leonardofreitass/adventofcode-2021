(ns adventofcode-2021.exercises.day-9.part-2)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-inputs
  [inputs]
  (vec (map (fn [line] (vec (map #(Integer/parseInt %) (str/split line #"")))) inputs)))

(defn adjacent-pos
  [r c]
  [[(inc r) c] [(dec r) c] [r (inc c)] [r (dec c)]])

(defn adjacent-basin
  [grid r c]
  (let [item (get-in grid [r c])]
    (set 
      (filter
        (fn [pos]
          (let [adjacent (get-in grid pos ##-Inf)]
            (and (not= adjacent 9) (>= adjacent item)))) 
        (adjacent-pos r c)))))

(defn generate-basin
  [grid r c]
  (loop [rest (adjacent-basin grid r c)
         basin (conj (adjacent-basin grid r c) [r c])]
    (if (empty? rest)
      basin
      (let [pos (first rest)
            all-adjacent-basin (apply adjacent-basin grid pos)
            new-basin (set/difference (apply adjacent-basin grid pos) basin)]
        (recur (apply conj (next rest) new-basin) (apply conj basin new-basin))))))

(defn find-all-basins
  [grid]
  (reduce-kv
    (fn [racc ridx row]
      (reduce-kv
        (fn [acc idx item]
          (let [adjacent (map #(get-in grid % ##Inf) (adjacent-pos ridx idx))
                lower (filter #(<= % item) adjacent)]
            (if (empty? lower)
              (conj acc (generate-basin grid ridx idx))
              acc)))
        racc
        row))
    []
    grid))

(defn desc-count [a b]
  (compare (count b) (count a)))

(defn run
  [inputs]
  (let [basins (find-all-basins (parse-inputs inputs))]
    (reduce #(* %1 (count %2)) 1 (take 3 (sort desc-count basins)))))
