(ns day15
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def file (get-lines "input/day15"))

(defn parse-grid [lines]
  (into {}
        (for [[j line] (map-indexed vector lines)
              [i v] (map-indexed vector line)]
          [[i j] (Character/digit v 10)])))

(def grid (parse-grid file))

(defn neighbours [grid [i j]]
  (filter #(grid %)
          [[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]]))

(defn heuristic [p]
  ;; Since all the edges will be larger than 0 it's safe to estimate
  ;; the heuristic function to be the distance to goal
  (let [[i j] p]
    (+ (- 100 i) (- 100 j))))

(defn A* [graph heuristic start goal])
