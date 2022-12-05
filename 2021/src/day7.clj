(ns day7
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input
  (->> (get-lines "input/day7")
       first
       str/trim
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

(defn abs [n] (max n (- n)))

(defn fuel [f in target]
  (let [diff (fn [a b] (f (abs (- a b))))]
    (->> in
         (map #(diff target %))
         (reduce +))))

(defn align-pos [f in]
  (let [from (apply min in)
        to (inc (apply max in))]
    (->> (range from to)
         (map #(fuel f in %))
         (apply min))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (align-pos identity input)))
  (pp/pprint (format "Problem two: %d"
                     (align-pos #(/ (* % (inc %)) 2) input))))
