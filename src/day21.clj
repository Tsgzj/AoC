(ns day21
  (:require [clojure.pprint :as pp]))

(defn step [[pawn point dices]]
  (let [total (reduce + (take 3 dices))
        loc (inc (mod (+ total (dec pawn)) 10))]
    [loc (+ point loc) (drop 6 dices)]))

(defn player-n [s-pos s-dice]
  (mapv (fn [[pawn point _]] (vector pawn point))
       (take-while (fn [[pawn point _]] (< point 1000))
              (iterate step [s-pos 0 s-dice]))))

(def determ-dice (cycle (range 1 101)))

(def player-one
  (player-n 6 determ-dice))

(def player-two
  (player-n 4 (drop 3 determ-dice)))

(defn part1 []
  (let [cnt1 (dec (count player-one))
        cnt2 (dec (count player-two))]
    (if (<= cnt1 cnt2)
      (* (inc (* 2 cnt1)) (last (nth player-two cnt1)) 3)
      (* (* 2 cnt2) (last (nth player-one cnt2)) 3))))
