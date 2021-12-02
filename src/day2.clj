(ns day2
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(defn parse-input [in]
  (->> (str/split in #" ")
       (#(hash-map (keyword (first %)) (Integer/parseInt (last %))))))

(def read-input
  (map #(parse-input %) (get-lines "input/day2")))

(defn dive []
  (->> (apply merge-with + read-input)
       (#(* (:forward %) (- (:down %) (:up %))))))

(defn move [pos m]
  (case (first (keys m))
    :down (list (first pos) (second pos) (+ (last pos) (:down m)))
    :up (list (first pos) (second pos) (- (last pos) (:up m)))
    :forward (list (+ (first pos) (:forward m)) (+ (second pos) (* (last pos) (:forward m))) (last pos))
    pos))

(defn dive2 []
  (->> (reduce move '(0 0 0) read-input)
       (#(* (first %) (second %)))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (dive)))
  (pp/pprint (format "Problem two: %d" (dive2))))
