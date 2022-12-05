(ns day15
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def file (get-lines "input/day15"))

(def height (count file))

(def width (count file))

(defn parse-grid [lines]
  (into {}
        (for [[j line] (map-indexed vector lines)
              [i v] (map-indexed vector line)]
          [[i j] (Character/digit v 10)])))

(def grid (parse-grid file))

(defn neighbours [m x]
  (let [[i j] x]
    (filter #(m %)
            [[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]])))

(defn edges [m a b]
  (m b))

(defn heuristic [p goal]
  ;; Since all the edges will be larger than 0 it's safe to estimate
  ;; the heuristic function to be the distance to goal
  (let [[i j] p
        [gi gj] goal]
    (+ (- gi i) (- gj j))))

(defn A* [edges neighbours heuristic start goal]
  (loop [q (priority-map start (heuristic start goal))
         preds {}
         shortest {start 0}
         done #{}]
    (when-let [[x hx] (peek q)]
      (if (= x goal)
        (reverse (take-while identity (iterate preds goal)))
        (let [dx (- hx (heuristic x goal))
              bn (for [n (remove done (neighbours x))
                       :let [hn (+ dx (edges x n) (heuristic x goal))
                             sn (shortest n Double/POSITIVE_INFINITY)]
                       :when (< hn sn)]
                   [n hn])]
          (recur (into (pop q) bn)
                 (into preds (for [[n] bn] [n x]))
                 (into shortest bn)
                 (conj done x)))))))


(defn chiton [m start goal]
  (A* (partial edges m) (partial neighbours m) heuristic start goal))

(defn enlarge [g n]
  (into {}
        (for [i (range (* n width))
              j (range (* n height))]
          (let [ni (mod i width)
                nj (mod j height)
                v (+ (g [ni nj]) (+ (quot i width) (quot j height)))
                nv (mod (+ v (quot v 10)) 10)]
            [[i j] nv]))))

(def lgrid (enlarge grid 5))

(def part1
  (- (reduce + (map #(grid %)
                    (chiton grid [0 0] [(dec width) (dec height)])))
     (grid [0 0])))

(def part2
  (- (reduce + (map #(lgrid %)
                    (chiton lgrid [0 0] [(dec (* 5 width)) (dec (* 5 height))])))
     (lgrid [0 0])))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" part1))
  (pp/pprint (format "Problem two: %d" part2)))

(solve nil)
