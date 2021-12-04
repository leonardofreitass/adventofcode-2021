(ns adventofcode-2021.exercises.day-4.part-2)

(require '[clojure.string :as str])

(defn parse-boards
  [boards]
  (map
    (fn [b]
      (map #(str/split (str/trim %) #"\s+") (next b)))
    (partition 6 boards)))

(defn index-boards
  [boards]
  (reduce-kv
    (fn [bres bidx bitm]
      (reduce-kv
        (fn [rres ridx ritm]
          (reduce-kv 
            (fn [cres cidx citm]
              {:index (update (:index cres) citm conj [bidx ridx cidx])
               :initial-sum (update (:initial-sum cres) bidx (fnil + 0) (Integer/parseInt citm))})
            rres
            (vec ritm)))
        bres
        (vec bitm)))
    {:index {} :initial-sum {}}
    (vec boards)))

(defn update-markups
  [n index markups sum playing]
  (loop [next-index index
         updated-markup markups
         next-sum sum
         next-playing playing]
    (if (empty? next-index)
      {:loser nil :markups updated-markup :sum next-sum :playing next-playing}
      (let [[bidx ridx cidx] (first next-index)
            row-index [bidx :rows ridx]
            column-index [bidx :columns cidx]
            marked-row (update-in updated-markup row-index conj n)
            new-markup (update-in marked-row column-index conj n)
            row (get-in new-markup row-index)
            column (get-in new-markup column-index)
            new-sum (update next-sum bidx - (Integer/parseInt n))
            won (or (= 5 (count row) ) (= 5 (count column)))
            new-playing (if won (disj next-playing bidx) next-playing)]
        (if (empty? new-playing)
          {:loser {:sum (get new-sum bidx) :n (Integer/parseInt n)} :markups new-markup :sum new-sum :playing new-playing}
          (recur (next next-index) new-markup new-sum new-playing))))))

(defn run
  [inputs]
  (let [boards (parse-boards (next inputs))
        {:keys [index initial-sum]} (index-boards boards)
        initial-playing (set (range (count boards)))]
    (loop [numbers (str/split (first inputs) #",")
           updated-markups (vec (repeat (count boards) {:rows {} :columns {}}))
           next-sum initial-sum
           next-playing initial-playing]
      (let [n (first numbers)
            {:keys [loser markups sum playing]} (update-markups n (get index n) updated-markups next-sum next-playing)]
        (if (not (nil? loser))
          (* (:sum loser) (:n loser))
          (recur (next numbers) markups sum playing))))))
