(ns day21
  (:require [clojure.pprint :as pp]))

(defn part1 []
  (loop [p1 6
         p2 4
         s1 0
         s2 0
         dice (cycle (range 1 101))
         cnt 0]
    (let [total (reduce + (take 3 dice))
          p1 (inc (mod (+ (dec p1) total) 10))
          s1 (+ p1 s1)]
      (if (>= s1 1000)
        (* s2 (+ 3 cnt))
        (recur p2 p1 s2 s1 (drop 3 dice) (+ 3 cnt))))))

(def part2-helper
  (memoize
   (fn [p1 p2 s1 s2]
     (if (>= s2 21)
       [0 1]
       (reduce #(mapv + %1 %2)
               (for [d1 [1 2 3]
                     d2 [1 2 3]
                     d3 [1 2 3]]
                 (let [total (+ d1 d2 d3)
                       p1 (inc (mod (+ (dec p1) total) 10))
                       s1 (+ p1 s1)]
                   (reverse (part2-helper p2 p1 s2 s1)))))))))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (part1)))
  (pp/pprint (format "Problem two: %d" (apply max (part2-helper 6 4 0 0)))))
