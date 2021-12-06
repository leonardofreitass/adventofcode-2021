(ns adventofcode-2021.exercises.day-6.part-1)

(require '[clojure.string :as str])

(def days 80)

(def fnil-inc (fnil inc 0))
(def fnil-add (fnil + 0))

(defn initial-state
  [fish]
  (reduce #(update-in %1 [%2 :adults] fnil-inc) {} fish))

(defn count-all
  [state]
  (reduce #(apply + %1 (vals (second %2))) 0 state))

(defn iterate
  [state]
  (reduce
    (fn [s n]
      (let [day (str (rem n 7))
            spawn-day (str (rem (+ n 2) 7))
            adults (get-in s [day :adults] 0)
            children (get-in s [day :children] 0)
            spawn (update-in s [spawn-day :children] fnil-add adults)
            mature (update-in (assoc-in spawn [day :children] 0) [day :adults] fnil-add children)]
        mature))
    state
    (range days)))

(defn run
  [inputs]
  (let [initial-state (initial-state (str/split (first inputs) #","))
        final-state (iterate initial-state)]
    (count-all final-state)))
