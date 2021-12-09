(ns day8
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def input
  (letfn [(parse-part [p] (str/split (str/trim p) #" "))
          (parse-line [l] (map parse-part (str/split l #"\|")))]
    (->> (get-lines "input/day8")
         (map parse-line))))

(defn unique-seg [m]
  (map #(get m %) '(2 3 4 7)))

(def segmentSearch1
  (let [unique-seg
        (fn [m]
          ( map #(get m %) '(2 3 4 7)))]
    (->> (mapcat last input)
         (map count)
         frequencies
         unique-seg
         (reduce +))))

(defn overlap [s1 s2]
  (filter (into #{} s1) s2))

(defn identify [seg seg1 seg4]
  (match [(count seg) (count (overlap seg4 seg)) (count (overlap seg1 seg))]
         [2 _ _] 1
         [3 _ _] 7
         [4 _ _] 4
         [7 _ _] 8
         [5 2 _] 2
         [5 3 1] 5
         [5 3 2] 3
         [6 4 _] 9
         [6 3 1] 6
         [6 3 2] 0))

(defn seek [f coll]
  (first (filter f coll)))

(defn decode-line [entry output]
  (let [seg1 (seek #(= 2 (count %)) entry)
        seg4 (seek #(= 4 (count %)) entry)]
    (->> (map #(identify % seg1 seg4) output)
         (reduce #(+ (* %1 10) %2)))))

(def segmentSearch2
  (->> (map #(decode-line (first %) (last %)) input)
       (reduce +)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" segmentSearch1))
  (pp/pprint (format "Problem two: %d" segmentSearch2)))
