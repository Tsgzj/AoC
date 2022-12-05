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

(defn get-enhanced-pixel [algo pattern]
  (->> pattern
       (apply str "2r")
       read-string
       (nth algo)))

(defn enhance-pixel [algo img p inf-bit]
  (->> (neighbours p)
        (map #(get-in img % inf-bit))
        (get-enhanced-pixel algo)))

(defn enhance [algo [img inf-bit]]
  (let [enhanced-img (for [j (range -1 (inc (count img)))
                           i (range -1 (inc (count (first img))))]
                       (enhance-pixel algo img [i j] inf-bit))
        n-inf-bit (get-enhanced-pixel algo (repeat 9 inf-bit))]
    [(->> enhanced-img
          (partition (+ 2 (count (first img))))
          (map vec)
          vec)
     n-inf-bit]))

(defn enhance-nth [algo [img inf-bit] times]
  (nth (iterate (partial enhance algo) [img inf-bit]) times))

(defn count-lit [[img inf-bit]]
  (->> img
       (map (fn [row] (filter #(= \1 %) row)))
       (map count)
       (reduce +)))

(defn part-n [times]
  (count-lit (enhance-nth enhance-algo [image \0] times)))

(defn part1 []
  (part-n 2))

(defn part2 []
  (part-n 50))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (part1)))
  (pp/pprint (format "Problem two: %d" (part2))))
