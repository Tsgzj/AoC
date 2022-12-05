(ns day13
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]) )

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-loc [line]
  (let [[a b] (map #(Integer/parseInt %) (str/split line #","))]
    [a b]))

(defn parse-foldline [line]
  (let [[a v] (str/split (subs line 11) #"=")]
    [(keyword a) (Integer/parseInt v)]))

(def group-input
  (group-by #(str/starts-with? % "fold")
            (filter (comp not empty?) (get-lines "input/day13"))))

(def loc
  (map parse-loc (group-input false)))

(def foldline
  (map parse-foldline (group-input true)))

(defn fold-paper [grid l]
  (into #{} (map #(fold-line % l) grid)))

(defn fold-line [p l]
  (let [[m n] p
        [axis v] l
        fold? (fn [p l]
                (if (= axis :x)
                  (> m v)
                  (> n v)))]
    (if (fold? p l)
      (if (= axis :x)
        [(- v (- m v)) n]
        [m (- v (- n v))])
      p)))

(def fold-n
  (reduce fold-paper loc foldline))

(defn print-grid [grid]
  (let [width (inc (apply max (map first grid)))
        height (inc (apply max (map second grid)))]
    (for [j (range height)]
      (str/join
       (for [i (range width)]
         (if (fold-n [i j])
           \@
           \ ))))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (count (fold-paper loc (first foldline)))))
  (pp/pprint (format "Problem two: \n"))
  (pp/pprint (print-grid fold-n)))
