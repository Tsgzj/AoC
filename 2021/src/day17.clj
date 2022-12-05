(ns day17
  (:require [clojure.pprint :as pp]))

(def x-limit [217 240])
;; (def x-limit [20 30])
(def y-limit [-126 -69])
;; (def y-limit [-10 -5])

(defn in-target-axis [v limit]
  (let [[minv maxv] limit]
    (and (>= v minv)
         (<= v maxv))))

(defn in-target? [p]
  (let [[x y] p]
    (and (in-target-axis x x-limit)
         (in-target-axis y y-limit))))

(defn next-x [x]
  (cond (pos? x) (dec x)
        (neg? x) (inc x)
        :else 0))

(defn stop? [p]
  (let [[x y] p]
    (or (> x (second x-limit))
        (< y (first y-limit)))))

(defn next-p [p v]
  (let [[x y] p
        [vx vy] v]
    (vector (vector (+ x vx) (+ y vy)) (vector (next-x vx) (dec vy)))))

(defn fire [velo]
  (loop [loc [0 0]
         v velo
         path (vector loc)]
    (let [[n-loc n-v] (next-p loc v)]
      (if (stop? n-loc)
        (if (in-target? (last path))
          path
          nil)
        (recur n-loc
               n-v
               (conj path n-loc))))))

(defn max-y [path]
  (apply max (map second path)))

(defn brute-force []
  (->> (for [vx (range 0 (+ 2 (second x-limit)))
             vy (range (first y-limit) (- 1 (first y-limit)))]
         (fire (vector vx vy)))
       (filter some?)))

(defn best-style []
  (->> (brute-force)
       (map max-y)
       (apply max)))

(def part1 (best-style))

(def part2 (count (brute-force)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" part1))
  (pp/pprint (format "Problem two: %d" part2)))

(solve nil)
