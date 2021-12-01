(ns adventofcode-2021.exercises.day-1.part-1)

(defn run
  [inputs]
  ((reduce
      (fn [{:keys [last acc]} next]
        (if (and (not (nil? last)) (> next last))
          {:last next :acc (inc acc)}
          {:last next :acc acc}))
      {:last nil :acc 0}
      (map #(Integer/parseInt %) inputs)) :acc))
