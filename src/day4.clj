(ns day4
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

(defn get-lines [fname]
  (with-open [r (io/reader fname)]
    (doall (line-seq r))))

(def read-input
  (let [input (get-lines "input/day4")]
    [(parse-numbers (first input))
     (parse-boards (rest input))] ))

(defn parse-numbers [input]
  (map #(Integer/parseInt %) (str/split input #",")))

(defn parse-board [input]
  (->> input
       (map #(str/split % #"\W+"))
       (map (fn [lst]
              (map #(Integer/parseInt %) lst)))))

(defn parse-boards [input]
  (->> input
       (map #(str/trim %))
       (partition 6)
       (map #(parse-board (rest %)))))

(defn mark [boards n]
  (map #(replace {n nil} %) boards))

(defn v-board [board]
  (map (fn [n]
         (map #(nth % n) board))
       (range 5)))

(defn win? [board]
  (let [line? (fn [l] (some true? (map #(every? nil? %) l)))]
    (cond  (or (line? board)
               (line? (v-board board))) board
           :else nil)))

(defn score [board n]
  (cond (win? board) (calculate board n)
        :else nil))

(defn calculate [board n]
  (* n
     (reduce + (filter some? (flatten board)))))

(defn squidgame [boards numbers]
  (letfn [(helper [boards numbers res]
            (cond (some? res) res
                  (empty? numbers) res
                  :else (helper (map #(mark % (first (rest numbers))) boards)
                                (rest numbers)
                                (some identity (map #(score % (first numbers)) boards)))))]
    (helper boards numbers nil)))

(defn game [board numbers]
  (letfn [(helper [board numbers stp]
            (cond
              (empty? numbers) {:step 0 :score nil}
              (win? board) {:step stp :score (calculate board (first numbers))}
              :else (helper (mark board (first (rest numbers)))
                            (rest numbers)
                            (inc stp))))]
    (helper board numbers 0)))

(defn squidgame2 [boards numbers]
  (->> (map #(game % numbers) boards)
       (sort-by :step #(compare %2 %1))
       first
       :score))

(defn puzzle [rule]
  (rule (mark (second read-input) (first (first read-input)))
             (first read-input)))

(defn solve [opts]
  (pp/pprint (format "Problem one: %d" (puzzle squidgame)))
  (pp/pprint (format "Problem two: %d" (puzzle squidgame2))))
