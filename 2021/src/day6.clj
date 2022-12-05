(ns day6
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input
  (->> (get-lines "input/day6")
       first
       str/trim
       (#(str/split % #","))
       (map keyword)))

(def fish-cycle
  {:1 :0 :2 :1 :3 :2 :4 :3 :5 :4 :6 :5 :7 :6 :8 :7})

(defn process [fish]
  (let [new-fish (:0 fish)
        next-fish (dissoc fish :0)
        next-fish (set/rename-keys next-fish fish-cycle)
        next-fish (assoc next-fish :8 new-fish)]
    (cond (nil? new-fish) next-fish
          :else (cond
                  (nil? (:6 next-fish)) (assoc next-fish :6 new-fish)
                  :else (update next-fish :6 (partial + new-fish)))) ) )

(defn latternfish [n input]
  (->> (frequencies input)
       (iterate process)
       (take (inc n))
       last
       vals
       (reduce +)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (latternfish 80 input))))
  (pp/pprint (format "Problem two: %d" (latternfish 256 input)))
