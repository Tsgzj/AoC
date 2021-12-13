(ns day11
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]) )

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn read-grid [lines]
  (into {}
        (for [[i line] (map-indexed vector lines)
              [j n] (map-indexed vector line)]
          [[i j] (Character/digit n 10)])))

(def grid
  (read-grid (get-lines "input/day11")))

(defn neighbours [g [i j]]
  (let [move (fn [i j m n] [(+ i m) (+ j n)])]
    (filter #(g %)
            (for [m (range -1 2)
                  n (range -1 2)]
              (move i j m n)))))

(defn filter-grid [f value]
  (fn [g]
    (filter (fn [[p v]] (f v value)) g)))

(defn f-over9 [g]
  ((filter-grid > 9) g))

(defn f-neg [g]
  ((filter-grid < 0) g))

(defn tick [g]
  (reduce #(update %1 %2 inc) g (map first g)))

(defn reset [g]
  (let [flashed (f-neg g)]
    (reduce #(assoc %1 (first %2) 0) g flashed)))

(defn flash [g]
  (letfn [(flash-one [g [p v]]
            (reduce #(update %1 %2 inc)
                    (assoc g p -1000)
                    (neighbours g p)))
          (flash-grid [g flashed]
            (reduce flash-one g flashed))
          (flash-helper [g flashed cnt]
            (if (empty? flashed)
              [g cnt]
              (let [new-g (flash-grid g flashed)
                    new-flashed (f-over9 new-g)]
                (flash-helper new-g new-flashed (+ cnt (count new-flashed))))))]
    (flash-helper g (f-over9 g) (count (f-over9 g)))))

(defn step [[g cnt]]
  (let [[new-g flash-cnt] (flash (tick g))]
    [(reset new-g) (+ cnt flash-cnt)]))

(def dumbo-octo-1
  (->> (iterate step [grid 0])
       (take 101)
       last
       second))

(def dumbo-octo-2
  (loop [[g cnt] [grid 0 ]
         lcnt 0]
    (if (= 100 (count ((filter-grid = 0) g)))
      lcnt
      (recur (step [g cnt]) (inc lcnt)))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" dumbo-octo-1))
  (pp/pprint (format "Problem two: %d" dumbo-octo-2)))
