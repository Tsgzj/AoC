(ns day20
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input (get-lines "input/day20"))

(def bitmap {\# \1 \. \0})

(def enhance-algo (mapv bitmap (first input)))

(def image (mapv #(mapv bitmap %) (rest (rest input))))

(defn neighbours [[x y]]
  (into []
        (for [j [-1 0 1]
              i [-1 0 1]]
          [(+ y j) (+ x i)])))

(defn enhance-pixel [algo img p]
  (->> (neighbours p)
       (map #(get-in img % \0))
       (apply str "2r")
       read-string
       (nth algo)))

(defn enhance [algo img]
  (let [enhanced-img (for [j (range -1 (inc (count img)))
                           i (range -1 (inc (count (first img))))]
                       (enhance-pixel algo img [i j]))]
    (->> enhanced-img
         (partition (+ 2 (count (first img))))
         (map vec)
         vec)))

(defn enhance-nth [algo img times]
  (nth (iterate (partial enhance algo) img) times))

(defn count-lit [img]
  (->> img
       (map (fn [row] (filter #(= \1 %) row)))
       (map count)
       (reduce +)))

(def part1
  (count-lit (enhance-nth enhance-algo image 2)))
